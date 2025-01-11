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
import ByteArray.Parser as Parser
import Elektron.Digitakt.CppStructs as CppStructs
import Elektron.Struct as ST
import Elektron.Struct.Part as Part exposing (Part)
import Missing.Maybe as Maybe
import Util

{-| Just like uint32be, but it checks that it is in the supported range.
-}
version : String -> Int -> Int -> Part Int
version t lo hi =
  let
    pbase = Part.uint32be
    errorMsg v =
      t ++ " version " ++ String.fromInt v
      ++ " out of supported range "
      ++ String.fromInt lo ++ " ~ " ++ String.fromInt hi
  in
    { pbase
    | decoder =
        pbase.decoder |> Parser.andThen (\v ->
          if lo <= v && v <= hi
            then Parser.succeed v
            else Parser.fail (errorMsg v)
          )
    }

magicHead : Int
magicHead = 0xbeefbace

magicTail : Int
magicTail = 0xbacef00c

{-| A field that always has a constant, magic value.
-}
magic : Int -> Part Int
magic x = Part.const x ("magic " ++ Util.hexBytesString x) Part.uint32be


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

--
-- PATTERN
--    corresponds to nothing - it is a pair of dumps given in response to
--    the whole pattern fetch command
--

type alias PatternKit =
  { pattern:   Pattern
  , kit:       Kit
  }

structPatternKit : ST.VersionedPart Int PatternKit
structPatternKit =
  ST.object PatternKit
    |> ST.versionStruct     .pattern    "pattern"   structPattern .version
    |> ST.fieldWithVersion  .kit        "kit"       (ST.fixedVersion structKit)
    |> ST.buildVersioned



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
  { version:      Int                 -- should be 4
  , tracks:       Array Track         -- 16 x
  , pLocks:       Array PLock         -- 80 x
  , name:         String              -- 1st part of patternSettingsStorage
  ,   skip1:        ByteArray
  , kitIndex:     Int
  ,   skip2:        ByteArray
  }

structPattern : ST.VersionedPart Int Pattern
structPattern =
  ST.object Pattern
    |> ST.version .version      "version"     (version "pattern" 0 9)
    |> ST.variant .tracks       "tracks"
        [ (0, Part.array 16 (structTrack 0))
        , (1, Part.array 16 (structTrack 1))
        , (2, Part.array 16 (structTrack 1))
        , (3, Part.array 16 (structTrack 2))
        , (4, Part.array 16 (structTrack 2))
        , (5, Part.array 16 (structTrack 3))
        , (6, Part.array 16 (structTrack 3))
        , (7, Part.array 16 (structTrack 4))
        , (8, Part.array 16 (structTrack 4))
        , (9, Part.array 16 (structTrack 5))
        ]
    |> ST.field   .pLocks       "pLocks"      (Part.array 80 structPLock)
    |> ST.field   .name         "name"        (Part.chars 16)
    |> ST.skipTo  .skip1                      CppStructs.patternStorage_kitIndex
    |> ST.field   .kitIndex     "kitIndex"    Part.uint8
    |> ST.skipTo  .skip2                      CppStructs.patternStorage_sizeof
    |> ST.buildVersioned


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
  { steps:        ByteArray
  ,   skip1:        ByteArray
  , soundPLocks:  ByteArray       -- may be empty for some versions
  ,   skip2:        ByteArray
  }

structTrack : Int -> Part Track
structTrack v =
  ST.object Track
    |> ST.field   .steps        "steps"       (Part.bytes 128)
    |> (case CppStructs.trackStorage_soundSlotLocks v of
          Nothing ->
               ST.omitField .skip1 ByteArray.empty
            >> ST.omitField .soundPLocks ByteArray.empty

          Just offset ->
               ST.skipTo .skip1 (\_ -> Just offset)
            >> ST.field .soundPLocks "soundPLocks" (Part.bytes 64)
    )
    |> ST.skipTo .skip2 (\_ -> CppStructs.trackStorage_sizeof v)
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
  { paramId:  Int
  , track:    Int
  , steps:    ByteArray
    -- 64x uint16be, but only high byte is used for some params, including
    -- sampleSlot which is the only plock we're interested in.  Hence it is
    -- easier to keep this as a ByteArray.
  }

structPLock : Part PLock
structPLock =
  ST.object PLock
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
      |> Maybe.andThen (.version >> CppStructs.soundParameters_sampleParamId)
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
  { version:      Int
  , name:         String
  ,   skip1:         ByteArray
  , sounds:       Array Sound         -- 8 x
  ,   skip2:         ByteArray
  , midiSetup:    Array MidiSetup     -- 8 x
  ,   skip3:         ByteArray
  }

structKit : ST.VersionedPart Int Kit
structKit =
  ST.object Kit
    |> ST.version .version      "version"     (version "kit" 0 9)
    |> ST.field   .name         "name"        (Part.chars 16)
    |> ST.skipTo  .skip1                      CppStructs.kitStorage_trackSounds
    |> ST.variant .sounds       "sounds"
        [ (0, Part.array 8 <| ST.fixedVersion structSound 0)
        , (1, Part.array 8 <| ST.fixedVersion structSound 0)
        , (2, Part.array 8 <| ST.fixedVersion structSound 0)
        , (3, Part.array 8 <| ST.fixedVersion structSound 0)
        , (4, Part.array 8 <| ST.fixedVersion structSound 0)
        , (5, Part.array 8 <| ST.fixedVersion structSound 0)
        , (6, Part.array 8 <| ST.fixedVersion structSound 1)
        , (7, Part.array 8 <| ST.fixedVersion structSound 1)
        , (8, Part.array 8 <| ST.fixedVersion structSound 2)
        , (9, Part.array 8 <| ST.fixedVersion structSound 2)
        ]
    |> ST.skipTo  .skip2                    CppStructs.kitStorage_midiParams
    |> ST.variant .midiSetup    "midiSetup"
        [ (0, Part.array 8 <| ST.fixedVersion structMidiSetup 0)
        , (1, Part.array 8 <| ST.fixedVersion structMidiSetup 0)
        , (2, Part.array 8 <| ST.fixedVersion structMidiSetup 0)
        , (3, Part.array 8 <| ST.fixedVersion structMidiSetup 0)
        , (4, Part.array 8 <| ST.fixedVersion structMidiSetup 0)
        , (5, Part.array 8 <| ST.fixedVersion structMidiSetup 0)
        , (6, Part.array 8 <| ST.fixedVersion structMidiSetup 1)
        , (7, Part.array 8 <| ST.fixedVersion structMidiSetup 1)
        , (8, Part.array 8 <| ST.fixedVersion structMidiSetup 1)
        , (9, Part.array 8 <| ST.fixedVersion structMidiSetup 1)
        ]
    |> ST.skipTo  .skip3                    CppStructs.kitStorage_sizeof
    |> ST.buildVersioned

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
  , version:      Int
  , tagMask:      Int
  , name:         String
  ,   skip1:        ByteArray
  , sampleSlot:   Int
  ,   skip2:        ByteArray
  , sample:       Sample
  ,   skip3:        ByteArray
  }

structSound : ST.VersionedPart Int Sound
structSound =
  ST.object Sound
    |> ST.field   .magicHead    "magicHead"   (magic magicHead)
    |> ST.version .version      "version"     (version "sound" 0 2)
    |> ST.field   .tagMask      "tagMask"     Part.uint32be
    |> ST.field   .name         "name"        (Part.chars 16)
    |> ST.skipTo  .skip1                      CppStructs.soundStorage_sampleSlot
    |> ST.field   .sampleSlot   "sampleSlot"  Part.uint8
    |> ST.skipTo  .skip2                      CppStructs.soundStorage_sampleFile
    |> ST.field   .sample       "sample"      structSample
    |> ST.skipTo  .skip3                      CppStructs.soundStorage_sizeof
    |> ST.buildVersioned

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
  { version:      Int
  ,   skip1:        ByteArray
  , enableMask:   Int
  ,   skip2:        ByteArray
  }

structMidiSetup : ST.VersionedPart Int MidiSetup
structMidiSetup =
  ST.object MidiSetup
    |> ST.version .version      "version"     (version "midi setup" 0 1)
    |> ST.skipTo  .skip1                      CppStructs.midiSetupStorage_enableMask
    |> ST.field   .enableMask   "enableMask"  Part.uint16be
    |> ST.skipTo  .skip2                      CppStructs.midiSetupStorage_sizeof
    |> ST.buildVersioned


midiTrackEnabled : MidiSetup -> Bool
midiTrackEnabled m = Bitwise.and 0x0001 m.enableMask /= 0
  -- check only channel enable, as in some older projects, the other
  -- bits were on, even when the channel wasn't enabled

--
-- PROJECT
--    corresponds to projectSettingsStorageContainer_t
--

type alias ProjectSettings =
  { version:      Int
  ,   skip1:        ByteArray
  , samples:      Array Sample    -- 128x
  ,   skip2:        ByteArray
  }

structProjectSettings : ST.VersionedPart Int ProjectSettings
structProjectSettings =
  ST.object ProjectSettings
    |> ST.version .version      "version"     (version "settings" 0 7)
    |> ST.skipTo  .skip1                      CppStructs.projectSettingsStorage_sampleList
    |> ST.field   .samples      "samples"     (Part.array 128 structSample)
    |> ST.skipTo  .skip2                      CppStructs.projectSettingsStorage_sizeof
    |> ST.buildVersioned


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
  ST.object Sample
    |> ST.field     .inode    "inode"     Part.uint32be
    |> ST.field     .hash     "hash"      Part.uint32be
    |> ST.field     .filesize "filesize"  Part.uint32be
    |> ST.field     .seqnr    "seqnr"     Part.uint32be
    |> ST.build

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

