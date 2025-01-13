module Elektron.Digitakt.Dump exposing
  ( PatternKit
  , patternKitName
  , setPatternKitName

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
import Elektron.Instrument exposing (Device(..))
import Elektron.Struct as ST
import Elektron.Struct.Part as Part exposing (Part)
import Elektron.Struct.Version as Version exposing (Version, VersionSpec(..))
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
    |> ST.build



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
    |> ST.skipTo  .skip1                      (.int >> CppStructs.patternStorage_kitIndex)
    |> ST.field   .kitIndex     "kitIndex"    Part.uint8
    |> ST.skipTo  .skip2                      (.int >> CppStructs.patternStorage_sizeof)
    |> ST.build

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
  ]

patternName : Pattern -> String
patternName pattern = pattern.name

setPatternName : String -> Pattern -> Pattern
setPatternName s pattern = { pattern | name = s }




--
-- TRACK
--    corresponds to trackStorage_v<x>_t
--    there is no trackStorageContainer_t
--

type alias Track =
  { version:      Version
  , steps:        ByteArray
  ,   skip1:        ByteArray
  , soundPLocks:  ByteArray       -- may be empty for some versions
  ,   skip2:        ByteArray
  }

structTrack : StorageStruct Track
structTrack =
  ST.struct Track
    |> ST.version .version      "version"     Version.external
    |> ST.field   .steps        "steps"       (Part.bytes 128)
    |> ST.skipTo  .skip1  (.int >> CppStructs.trackStorage_soundSlotLocks)
    |> ST.fieldV  .soundPLocks  "soundPLocks"
                                              (\v ->
                                                if Maybe.isJust (CppStructs.trackStorage_soundSlotLocks v.int)
                                                  then (Part.bytes 64)
                                                  else (Part.bytes 0)
                                              )
    -- |> (case CppStructs.trackStorage_soundSlotLocks v of
    --       Nothing ->
    --            ST.omitField .skip1 ByteArray.empty
    --         >> ST.omitField .soundPLocks ByteArray.empty

    --       Just offset ->
    --            ST.skipTo .skip1 (\_ -> Just offset)
    --         >> ST.field .soundPLocks "soundPLocks" (Part.bytes 64)
    -- )
    |> ST.skipTo .skip2 (.int >> CppStructs.trackStorage_sizeof)
    |> ST.build


allSteps : List Int
allSteps = List.range 0 63

anyTrigsSet : Track -> Bool
anyTrigsSet track =
  let
    step i = (2 * i) + 1
    trig i =
      ByteArray.get (step i) track.steps
        |> Maybe.unwrap False (\v -> Bitwise.and 0x0001 v == 1)
  in
    List.any trig allSteps

trackSoundPLocks : Track -> Array (Maybe Int)
trackSoundPLocks track =
  let
    getByte i = ByteArray.get i track.soundPLocks
    plock i = if i == 0xff then Nothing else Just i
  in
    Array.initialize 64 (getByte >> Maybe.andThen plock)

setTrackSoundPlocks : Array (Maybe Int) -> Track -> Track
setTrackSoundPlocks plocks track =
  let
    plockBytes =
      ByteArray.section 0 (ByteArray.length track.soundPLocks)
      <| ByteArray.fromArray
      <| Array.map (Maybe.withDefault 0xff) plocks

    soundPLocks = ByteArray.replace 0 plockBytes track.soundPLocks
  in
    { track | soundPLocks = soundPLocks}



--
-- PLOCK
--    corresponds to patternParamLocksStorage_v0_t
--

type alias PLock =
  { version:  Version
  , paramId:  Int
  , track:    Int
  , steps:    ByteArray
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
    |> ST.field   .steps      "steps"     (Part.bytes 128)
    |> ST.build

plockSamplePLocks : Maybe Sound -> PLock -> Maybe (Array (Maybe Int))
plockSamplePLocks sound plock =
  let
    getByte i = ByteArray.get (2 * i) plock.steps
    step i = if i == 0xff then Nothing else Just i
    isSamplePlock = sound
      |> Maybe.andThen (.version >> .int >> CppStructs.soundParameters_sampleParamId)
      |> Maybe.map (\n -> n == plock.paramId)
      |> Maybe.withDefault False
  in
    if isSamplePlock
      then Just <| Array.initialize 64 (getByte >> Maybe.andThen step)
      else Nothing

setPlockSamplePlocks : Maybe (Array (Maybe Int)) -> PLock -> PLock
setPlockSamplePlocks mSteps plock =
  let
    stepBytes =
      ByteArray.fromList
      << List.concat
      << Array.toList
      << Array.map (Maybe.unwrap [0xff, 0xff] (\b -> [b, 0x00]))
  in
    case mSteps of
      Just steps -> { plock | steps = stepBytes steps }
      Nothing -> plock



--
-- KIT
--    corresponds to kitStorageContainer_t
--

type alias Kit =
  { version:      Version
  , name:         String
  ,   skip1:         ByteArray
  , sounds:       Array Sound         -- 8 x
  ,   skip2:         ByteArray
  , midiSetup:    Array MidiSetup     -- 8 x
  ,   skip3:         ByteArray
  }

structKit : StorageStruct Kit
structKit =
  ST.struct Kit
    |> ST.version .version      "version"     Version.uint32be
    |> ST.field   .name         "name"        (Part.chars 16)
    |> ST.skipTo  .skip1                      (.int >> CppStructs.kitStorage_trackSounds)
    |> ST.fieldV  .sounds       "sounds"      (subStructArray kitVersionToSounds structSound)
    |> ST.skipTo  .skip2                      (.int >> CppStructs.kitStorage_midiParams)
    |> ST.fieldV  .midiSetup    "midiSetup"   (subStructArray kitVersionToMidiSetups structMidiSetup)
    |> ST.skipTo  .skip3                      (.int >> CppStructs.kitStorage_sizeof)
    |> ST.build

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
  ]


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
    allZero = List.all (\(i, s) -> s == 0)
      -- older cleared kits had the slots all set to 0
    allDefault = List.all (\(i, s) -> s == (i + 1))
      -- clearing the kit sets the tracks to slots 1 ~ 8
    allSmall = List.all (\(i, s) -> 0 <= s && s <= 8)
      -- default kits prior to 3.0beta3 were sample shuffled, and so could end
      -- up with a permutation of 1..8 and zeros for deleted samples
    allDisabled =
      List.all (not << midiTrackEnabled) <| Array.toList kit.midiSetup
  in
    (allZero slots || allDefault slots || allSmall slots) && allDisabled


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
    |> ST.skipTo  .skip1                      (.int >> CppStructs.soundStorage_sampleSlot)
    |> ST.field   .sampleSlot   "sampleSlot"  Part.uint8
    |> ST.skipTo  .skip2                      (.int >> CppStructs.soundStorage_sampleFile)
    |> ST.field   .sample       "sample"      structSample
    |> ST.skipTo  .skip3                      (.int >> CppStructs.soundStorage_sizeof)
    |> ST.build

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
  { version:      Version
  ,   skip1:        ByteArray
  , enableMask:   Int
  ,   skip2:        ByteArray
  }

structMidiSetup : StorageStruct MidiSetup
structMidiSetup =
  ST.struct MidiSetup
    |> ST.version .version      "version"     Version.uint32be
    |> ST.skipTo  .skip1                      (.int >> CppStructs.midiSetupStorage_enableMask)
    |> ST.field   .enableMask   "enableMask"  Part.uint16be
    |> ST.skipTo  .skip2                      (.int >> CppStructs.midiSetupStorage_sizeof)
    |> ST.build


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
    |> ST.skipTo  .skip1                      (.int >> CppStructs.projectSettingsStorage_sampleList)
    |> ST.field   .samples      "samples"     (Part.array 128 structSample)
    |> ST.skipTo  .skip2                      (.int >> CppStructs.projectSettingsStorage_sizeof)
    |> ST.build


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
    |> ST.buildAsPart

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

