module Elektron.Digitakt.Dump exposing
  ( PatternKit
  , patternKitName
  , setPatternKitName, setPatternKitIndex
  , sampleTrack

  , Pattern
  , Track
  , anyTrigsSet
  , trackSoundPLocks
  , setTrackSoundPlocks

  , PLock
  , plockSamplePLocks
  , setPlockSamplePlocks

  , Kit
  , isDefaultKit

  , Sound
  , sameSound
  , soundName
  , setSoundName
  , soundSampleSlot
  , setSoundSampleSlot

  , ProjectSettings

  , Sample
  , emptySample
  , sameSample
  , isEmptySample
  , sampleLength

  , StorageStruct

  , structPatternKit
  , structPattern
  , structKit
  , structSound
  , structProjectSettings
  )

{-| Elektron instruments can transfer the state of the current project. It does
so through the "dump" SysEx messages that contain the "storage" format of the
data. This is the form of the data that you can store on your computer for
backup and send back to the instrument later.

The state consistes of several pieces, like pattern, kit, and sound. A dump
of a whole project is really just a dump of these items in sequence.

Most of these structures are versioned: That is, there are different versions
of the same data over time. Instrument OS releases sometimes introduce newer
versions of the structures.

Originally, I reverse engineered these structures. Later, I was given access
to the .h files the Elektron uses to define these structures. The .h files
aren't needed to build elk-herd, but when new instrument OS releases introduce
new versions of the structures, they must be checked to see if there are any
code changes needed (usually not), and the field offsets and sizes (in the
module `Elektron.Digitakt.CppStructs`) must be updated (using the `struc-util`
tool).
-}

import Array exposing (Array)
import Bitwise

import ByteArray exposing (ByteArray)
import Elektron.Digitakt.CppStructs as CppStructs
import Elektron.Instrument exposing (Device(..), Version)
import Elektron.Struct as ST
import Elektron.Struct.Part as Part exposing (Part)
import Elektron.Struct.Version as Version exposing (VersionSpec(..))
import Missing.Maybe as Maybe


{- Structure names follow Elektron's header files names for these structures.

Except for patternKit, in the header files, each of these has versioned storage
versions named in the form:
  <thing>Storage_v<n>_t
These are put in a union called:
  <thing>StorageContainer_t
And there is an "in memory" version:
  <thing>_t

The goal of this code is to move between the transfered bytes which are forms
of the versioned structs, and an Elm type that is equivalent to the in memory
struct (though with only the fields we care about.)

The names used in the Digitakt UI differ somewhat.

    elk-herd        UI              Elektron code
    -----------     -------------   --------------------
    PatternKit      Whole Pattern   patternKit
      Pattern         Sequence        pattern + patternSettings
        Track           Track           track
        PLocks          --              patternParamLocks
      Kit             Kit             kit
        Sound           Sound           sound
        FxSetup         --              fxSetup
        MidiSetup       --              midiSetup
    ProjectSettings   Project         projectSettings
-}

type alias StorageStruct s =
  ST.Struct VersionSpec Version s


{- It is very common for one structure to include another structure, or an
array of another structure.  In either case, the version of the parent
structure determines the version of the child structure.

`List SubStructMap` provides for a mapping of such a relationship. Use
`subStruct` or `subStructArray` to build such a child Part.
-}


type alias SubStructMap a = { a | device : Device, parent : Int, child : Int }

{-| Lookup an entry for a version, then pass that entry plus a VersionSpec
for the child to a function for creating the Part. If none found, return
Part.fail
-}
subStruct :
  List (SubStructMap a)
  -> ((SubStructMap a) ->  VersionSpec -> Part c)
  -> Version
  -> Part c
subStruct mapping fPart v =
  let
    go items =
      case items of
        [] ->
          Part.fail ("No mapping from v" ++ String.fromInt v.int)
        m :: rest ->
          if m.device == v.device && m.parent == v.int
            then fPart m <| MatchVersion <| Version m.device m.child
            else go rest
  in
    go mapping


type alias SubStructArrayMap = SubStructMap { n : Int }

{-| Like subStruct, but create an array of a particular Struct. The size of
the array is in the lookup table entry as `n`.
-}
subStructArray :
  List SubStructArrayMap
  -> StorageStruct s
  -> Version
  -> Part (Array s)
subStructArray mapping struct =
  subStruct mapping
    (\args childSpec -> Part.array args.n <| ST.forVersionSpec struct childSpec)


numSteps : Version -> Maybe Int
numSteps v =
  case v.device of
      Digitakt  -> Just  64
      Digitakt2 -> Just 128
      _         -> Nothing

numSampleSlots : Version -> Maybe Int
numSampleSlots v =
  case v.device of
      Digitakt  -> Just  128
      Digitakt2 -> Just 1024
      _         -> Nothing


--
-- PATTERN
--    corresponds to nothing - it is a pair of dumps given in response to
--    the whole pattern fetch command
--

type alias PatternKit =
  { pattern:   Pattern
  , kit:       Kit
  }

structPatternKit : StorageStruct PatternKit
structPatternKit =
  ST.struct PatternKit
    |> ST.version   .pattern    "pattern"   structPattern
    |> ST.fieldV    .kit        "kit"       (MatchVersion >> ST.forVersionSpec structKit)
    |> ST.build "PatternKit"



{- On the Digitakt, the pattern name is stored in both the sequence data
structure and in the kit data structure. The sequence verison is what is
displayed in the device's UI, so that is the version used here. When setting
the name, both are set to keep them in sync.
-}

patternKitName : PatternKit -> String
patternKitName patternKit = patternName patternKit.pattern

setPatternKitName : String -> PatternKit -> PatternKit
setPatternKitName s patternKit =
  { patternKit
  | pattern = setPatternName s patternKit.pattern
  , kit = setKitName s patternKit.kit
  }

setPatternKitIndex : Int -> PatternKit -> PatternKit
setPatternKitIndex i patternKit =
  { patternKit
  | pattern = setPatternIndex i patternKit.pattern
  }


sampleTrack : PatternKit -> Int -> Bool
sampleTrack patternKit =
  case patternKit.kit.midiMask of
    Nothing -> (\t -> 0 <= t && t < 8)
    Just mask -> (\t -> 0 <= t && t < 15
                        && Bitwise.and mask (Bitwise.shiftLeftBy t 1) == 0)

--
-- PATTERN
--    corresponds to patternStorageContainer_t
--

type alias Pattern =
  { version:      Version
  , tracks:       Array Track         -- 16 x
  , pLocks:       Array PLock         -- 80 x
  , name:         String              -- 1st part of patternSettingsStorage
  ,   skip1:        ByteArray
  , kitIndex:     Int
  ,   skip2:        ByteArray
  }

structPattern : StorageStruct Pattern
structPattern =
  ST.struct Pattern
    |> ST.version .version      "version"     Version.uint32be
    |> ST.fieldV  .tracks       "tracks"      (subStructArray mapPatternVersionToTracks structTrack)
    |> ST.fieldV  .pLocks       "pLocks"      (\v ->
                                                Part.array 80
                                                <| ST.forVersionSpec structPLock
                                                <| patternToPlockVersion v
                                              )
    |> ST.field   .name         "name"        (Part.chars 16)
    |> ST.skipTo  .skip1                      CppStructs.patternStorage_kitIndex
    |> ST.field   .kitIndex     "kitIndex"    Part.uint8
    |> ST.skipTo  .skip2                      CppStructs.patternStorage_sizeof
    |> ST.build "Pattern"

patternToPlockVersion : Version -> VersionSpec
patternToPlockVersion v = MatchVersion (Version v.device 0)


mapPatternVersionToTracks : List SubStructArrayMap
mapPatternVersionToTracks =
  [ { device = Digitakt, parent = 0, child = 0, n = 16 }
  , { device = Digitakt, parent = 1, child = 1, n = 16 }
  , { device = Digitakt, parent = 2, child = 1, n = 16 }
  , { device = Digitakt, parent = 3, child = 2, n = 16 }
  , { device = Digitakt, parent = 4, child = 2, n = 16 }
  , { device = Digitakt, parent = 5, child = 3, n = 16 }
  , { device = Digitakt, parent = 6, child = 3, n = 16 }
  , { device = Digitakt, parent = 7, child = 4, n = 16 }
  , { device = Digitakt, parent = 8, child = 4, n = 16 }
  , { device = Digitakt, parent = 9, child = 5, n = 16 }

  , { device = Digitakt2, parent = 0, child = 0, n = 16 }
  ]

patternName : Pattern -> String
patternName pattern = pattern.name

setPatternName : String -> Pattern -> Pattern
setPatternName s pattern = { pattern | name = s }

setPatternIndex : Int -> Pattern -> Pattern
setPatternIndex i pattern = { pattern | kitIndex = i }



--
-- TRACK
--    corresponds to trackStorage_v<x>_t
--    there is no trackStorageContainer_t
--

type alias Track =
  { version:      Version
  , steps:        Array Int
  ,   skip1:        ByteArray
  , soundPLocks:  Array Int       -- may be empty for some versions
  ,   skip2:        ByteArray
  }

structTrack : StorageStruct Track
structTrack =
  ST.struct Track
    |> ST.version .version      "version"     Version.external
    |> ST.fieldV  .steps        "steps"       stepsPart
    |> ST.skipToOrOmit
                  .skip1  CppStructs.trackStorage_soundSlotLocks
    |> ST.fieldV  .soundPLocks  "soundPLocks" soundPlocksPart
    |> ST.skipTo .skip2 CppStructs.trackStorage_sizeof
    |> ST.build "Track"

soundPlocksPart : Version -> Part (Array Int)
soundPlocksPart v =
  CppStructs.trackStorage_soundSlotLocks v
  |> Maybe.andThen      (\_ -> numSteps v)
  |> Maybe.map          (\n -> Part.arrayHex n Part.uint8)
  |> Maybe.withDefault  (Part.ephemeral Array.empty)


stepsPart : Version -> Part (Array Int)
stepsPart v =
  numSteps v
  |> Maybe.map          (\n -> Part.arrayHex n Part.uint16be)
  |> Maybe.withDefault  (Part.fail "unknown device")


anyTrigsSet : Track -> Bool
anyTrigsSet track =
  let
    check i =
      case (Array.get i track.steps) of
        Just v ->
          if Bitwise.and 0x0001 v == 1
            then True
            else check (i + 1)
        Nothing ->
          False
  in
    check 0

trackSoundPLocks : Track -> Array (Maybe Int)
trackSoundPLocks track =
  let
    plock i = if i == 0xff then Nothing else Just i
  in
    Array.map plock track.soundPLocks

setTrackSoundPlocks : Array (Maybe Int) -> Track -> Track
setTrackSoundPlocks plocks track =
  { track | soundPLocks = Array.map (Maybe.withDefault 0xff) plocks}



--
-- PLOCK
--    corresponds to patternParamLocksStorage_v0_t
--

type alias PLock =
  { version:  Version
  , paramId:  Int
  , track:    Int
  , steps:    Array Int
    -- 64x uint16be, but only high byte is used for some params, including
    -- sampleSlot which is the only plock we're interested in.  Hence it is
    -- easier to keep this as a ByteArray.
  }

structPLock : StorageStruct PLock
structPLock =
  ST.struct PLock
    |> ST.version .version    "vesion"    Version.external
    |> ST.field   .paramId    "paramId"   Part.uint8
    |> ST.field   .track      "track"     Part.uint8
    |> ST.fieldV  .steps      "steps"     stepsPart
    |> ST.build "PLock"

plockSamplePLocks : PatternKit -> PLock -> Maybe (Array (Maybe Int))
plockSamplePLocks patternKit plock =
  let
    decodeStep =
      case plock.version.device of
        Digitakt ->
          \v ->
            if Bitwise.and 0xff00 v == 0xff00
              then Nothing
              else Just <| Bitwise.shiftRightBy 8 v
        Digitakt2 -> \v -> if v == 0xffff then Nothing else Just v
        _ -> always Nothing
    isSampleTrack = sampleTrack patternKit
    isSamplePlock =
      CppStructs.soundParameters_sampleParamId plock.version
      |> Maybe.map (\n -> n == plock.paramId && isSampleTrack plock.track)
      |> Maybe.withDefault False
  in
    if isSamplePlock
      then Just <| Array.map decodeStep plock.steps
      else Nothing

setPlockSamplePlocks : Maybe (Array (Maybe Int)) -> PLock -> PLock
setPlockSamplePlocks mSteps plock =
  let
    encodeStep =
      case plock.version.device of
        Digitakt ->
          \mv ->
            case mv of
                Nothing -> 0xff00
                Just v -> Bitwise.shiftLeftBy 8 v
        Digitakt2 -> Maybe.withDefault 0xffff
        _ -> always 0xffff
  in
    case mSteps of
      Just steps -> { plock | steps = Array.map encodeStep steps }
      Nothing -> plock



--
-- KIT
--    corresponds to kitStorageContainer_t
--

type alias Kit =
  { magicHead:    Maybe Int
  , version:      Version
  , name:         String
  ,   skip1:         ByteArray
  , sounds:       Array Sound         -- 8 x
  ,   skip2:         ByteArray
  , midiSetup:    Array MidiSetup     -- 8 x
  ,   skip3:         ByteArray
  , midiMask:     Maybe Int
  ,   skip4:         ByteArray
  }

structKit : StorageStruct Kit
structKit =
  ST.struct Kit
    |> ST.field   .magicHead    "magicHead"   (Part.optional Part.magicHead)
    |> ST.version .version      "version"     Version.uint32be
    |> ST.field   .name         "name"        (Part.chars 16)
    |> ST.skipTo  .skip1                      CppStructs.kitStorage_trackSounds
    |> ST.fieldV  .sounds       "sounds"      (subStructArray kitVersionToSounds structSound)
    |> ST.skipTo  .skip2                      CppStructs.kitStorage_midiParams
    |> ST.fieldV  .midiSetup    "midiSetup"   (subStructArray kitVersionToMidiSetups structMidiSetup)
    |> ST.skipToOrOmit
                  .skip3                      CppStructs.kitStorage_midiMask
    |> ST.fieldV  .midiMask     "midiMask"    midiMaskPart
    |> ST.skipTo  .skip4                      CppStructs.kitStorage_sizeof
    |> ST.build "Kit"

{- NB: the magic header is optional because it exists in DT2 structures, but
not DT1. The right thing to do would be to choose between parsing it, or
making it ephemeral based on the device.

However, since this field becomes BEFORE the version, we don't yet have a full
version at parse time. We do have the device from the version spec... but we
don't have the Struct machinery to get it there.

For now, since the magic value will never be a valid version, the hack is to
parse the version if we can, and if not, assume it isn't present.
-}


kitVersionToSounds : List SubStructArrayMap
kitVersionToSounds =
  [ { device = Digitakt, parent = 0, child = 0, n = 8 }
  , { device = Digitakt, parent = 1, child = 0, n = 8 }
  , { device = Digitakt, parent = 2, child = 0, n = 8 }
  , { device = Digitakt, parent = 3, child = 0, n = 8 }
  , { device = Digitakt, parent = 4, child = 0, n = 8 }
  , { device = Digitakt, parent = 5, child = 0, n = 8 }
  , { device = Digitakt, parent = 6, child = 1, n = 8 }
  , { device = Digitakt, parent = 7, child = 1, n = 8 }
  , { device = Digitakt, parent = 8, child = 2, n = 8 }
  , { device = Digitakt, parent = 9, child = 2, n = 8 }

  , { device = Digitakt2, parent = 0, child = 0, n = 16 }
  ]

kitVersionToMidiSetups : List SubStructArrayMap
kitVersionToMidiSetups =
  [ { device = Digitakt, parent = 0, child = 0, n = 8 }
  , { device = Digitakt, parent = 1, child = 0, n = 8 }
  , { device = Digitakt, parent = 2, child = 0, n = 8 }
  , { device = Digitakt, parent = 3, child = 0, n = 8 }
  , { device = Digitakt, parent = 4, child = 0, n = 8 }
  , { device = Digitakt, parent = 5, child = 0, n = 8 }
  , { device = Digitakt, parent = 6, child = 1, n = 8 }
  , { device = Digitakt, parent = 7, child = 1, n = 8 }
  , { device = Digitakt, parent = 8, child = 1, n = 8 }
  , { device = Digitakt, parent = 9, child = 1, n = 8 }

  , { device = Digitakt2, parent = 0, child = 0, n = 16 }
  ]

midiMaskPart : Version -> Part (Maybe Int)
midiMaskPart v =
  CppStructs.kitStorage_midiMask v
  |> Maybe.map          (\_ -> Part.uint16be |> Part.map Just (Maybe.withDefault 0))
  |> Maybe.withDefault  (Part.ephemeral Nothing)



setKitName : String -> Kit -> Kit
setKitName s kit = { kit | name = s }

{-| Does this look like one of the empty pattern kits?
Currently the check is
  - the sample slots are
    - all zero
    - or 1 ~ 8 in order
    - or in 0 ~ 8
  - and the MIDI setups don't have channel enabled

In theory the other sound and MIDI settings could be checked for default,
which may or may not lead to a better definition of default. But since
patterns are also checked for non-empty name... this is probably good enough.
-}
isDefaultKit : Kit -> Bool
isDefaultKit kit =
  let
    slots = Array.toIndexedList <| Array.map soundSampleSlot kit.sounds
    isDigitakt = kit.version.device == Digitakt
    allZero = List.all (\(i, s) -> s == 0)
      -- older cleared kits had the slots all set to 0
    allDefault = List.all (\(i, s) -> s == (i + 1))
      -- clearing the kit sets the tracks to slots 1 ~ 8
    allSmall = List.all (\(i, s) -> 0 <= s && s <= 8)
      -- default kits prior to 3.0beta3 were sample shuffled, and so could end
      -- up with a permutation of 1..8 and zeros for deleted samples
    allDisabled =
      List.all (not << midiTrackEnabled) <| Array.toList kit.midiSetup
    noMidiMachines = Maybe.map ((==) 0) kit.midiMask |> Maybe.withDefault True
  in
    (allDefault slots
    || (isDigitakt && (allSmall slots || allZero slots))
    )
    && allDisabled
    && noMidiMachines


-- currently we consider just the sample slot settings of the eight tracks
-- and the MIDI channel settings

--
-- SOUND
--    corresponds to soundStorageContainer_t
--

type alias Sound =
  { magicHead:    Int
  , version:      Version
  , tagMask:      Int
  , name:         String
  ,   skip1:        ByteArray
  , sampleSlot:   Int
  ,   skip2:        ByteArray
  , sample:       Sample
  ,   skip3:        ByteArray
  }

structSound : StorageStruct Sound
structSound =
  ST.struct Sound
    |> ST.field   .magicHead    "magicHead"   Part.magicHead
    |> ST.version .version      "version"     Version.uint32be
    |> ST.field   .tagMask      "tagMask"     Part.uint32be
    |> ST.field   .name         "name"        (Part.chars 16)
    |> ST.skipTo  .skip1                      CppStructs.soundStorage_sampleSlot
    |> ST.fieldV  .sampleSlot   "sampleSlot"  sampleSlotPart
    |> ST.skipTo  .skip2                      CppStructs.soundStorage_sampleFile
    |> ST.field   .sample       "sample"      structSample
    |> ST.skipTo  .skip3                      CppStructs.soundStorage_sizeof
    |> ST.build "Sound"

sampleSlotPart : Version -> Part Int
sampleSlotPart v =
  case numSampleSlots v of
    Just n  -> if n > 128 then Part.uint16be else Part.uint8
    _       -> Part.fail "unknown sample pool size"

sameSound : Sound -> Sound -> Bool
sameSound a b =
  sameSample a.sample b.sample    -- most likely to fail, so first!
  && a.magicHead == b.magicHead
  && a.version == b.version
  && a.tagMask == b.tagMask
  && a.name == b.name
  && ByteArray.same a.skip1 b.skip1
  -- we don't care if the sampleSlot is the same,
  -- so long as the sample is the same
  && ByteArray.same a.skip2 b.skip2
  && ByteArray.same a.skip3 b.skip3

soundName : Sound -> String
soundName sound = sound.name

setSoundName : String -> Sound -> Sound
setSoundName s sound = { sound | name = s }

soundSampleSlot : Sound -> Int
soundSampleSlot sound = sound.sampleSlot

setSoundSampleSlot : Int -> Sound -> Sound
setSoundSampleSlot i sound = { sound | sampleSlot = i }





--
-- MIDISETUP
--    corresponds to midiSetupStorage_v0_t
--

-- TODO: This structure has version, params, setup, and reserved areas
-- once there is more than just V0, this will need to be represented.

type alias MidiSetup =
  { magicHead:    Maybe Int
  , version:      Version
  ,   skip1:        ByteArray
  , enableMask:   Int
  ,   skip2:        ByteArray
  }

structMidiSetup : StorageStruct MidiSetup
structMidiSetup =
  ST.struct MidiSetup
    |> ST.field   .magicHead    "magicHead"   (Part.optional Part.magicHead)
    |> ST.version .version      "version"     Version.uint32be
    |> ST.skipTo  .skip1                      CppStructs.midiSetupStorage_enableMask
    |> ST.field   .enableMask   "enableMask"  Part.uint16be
    |> ST.skipTo  .skip2                      CppStructs.midiSetupStorage_sizeof
    |> ST.build "MidiSetup"


midiTrackEnabled : MidiSetup -> Bool
midiTrackEnabled m = Bitwise.and 0x0001 m.enableMask /= 0
  -- check only channel enable, as in some older projects, the other
  -- bits were on, even when the channel wasn't enabled

--
-- PROJECT
--    corresponds to projectSettingsStorageContainer_t
--

type alias ProjectSettings =
  { version:      Version
  ,   skip1:        ByteArray
  , samples:      Array Sample    -- 128x
  ,   skip2:        ByteArray
  }

structProjectSettings : StorageStruct ProjectSettings
structProjectSettings =
  ST.struct ProjectSettings
    |> ST.version .version      "version"     Version.uint32be
    |> ST.skipTo  .skip1                      CppStructs.projectSettingsStorage_sampleList
    |> ST.fieldV   .samples      "samples"    sampleBankPart
    |> ST.skipTo  .skip2                      CppStructs.projectSettingsStorage_sizeof
    |> ST.build "ProjectSettings"

sampleBankPart : Version -> Part (Array Sample)
sampleBankPart v =
  case numSampleSlots v of
    Just n -> Part.array n structSample
    _      -> Part.fail "unknown sample pool size"


--
-- SAMPLE
--    corresponds to uniqueSampleFileID_t
--    If inode is inodeInvalid, other values are zero and sample slot is unused.
--    If inode and seqnr are set to unknown (0), then they will be recomputed
--    on load.
--    filesize, if non-zero, includes the 16 byte waveform padding.

type alias Sample =
  { inode : Int
  , hash : Int
  , filesize : Int
  , seqnr : Int
  }

structSample : Part Sample
structSample =
  ST.struct Sample
    |> ST.field     .inode    "inode"     Part.uint32be
    |> ST.field     .hash     "hash"      Part.uint32be
    |> ST.field     .filesize "filesize"  Part.uint32be
    |> ST.field     .seqnr    "seqnr"     Part.uint32be
    |> ST.buildAsPart "Sample"

inodeInvalid : Int
inodeInvalid = 0xffffffff

inodeUnknown : Int
inodeUnknown = 0

seqnrUnknown : Int
seqnrUnknown = 0

emptySample : Sample
emptySample =
  { inode = inodeInvalid
  , hash = 0
  , filesize = 0
  , seqnr = 0
  }

sameSample : Sample -> Sample -> Bool
sameSample a b =
  if a.inode == inodeInvalid
    then b.inode == inodeInvalid
    else a.hash == b.hash && a.filesize == b.filesize

isEmptySample : Sample -> Bool
isEmptySample sample = sample.inode == inodeInvalid

sampleLength : Sample -> Int
sampleLength sample = if isEmptySample sample then 0 else sample.filesize - 16

