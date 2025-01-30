module Elektron.Digitakt.HighLevel exposing
  ( Project
  , emptyProject

  , rebuildCrossReference

  , updateFromDump
  , updateFromSysEx

  , neededSampleNames
  , updateSampleNames

  , toSysExDumpsForFile
  , toSysExDumpsForSend

  , shufflePatterns
  , shuffleSamples
  , shuffleSounds

  , Shuffles
  , importProject

  , setPatternName
  , setSoundName

  , findSameSamples
  , findSameSounds
  )

{-| This is the data model that the `Project` modules interact with. All actual
data manipulation happens here.

Interestingly... it all comes down to shuffles (moving around the items in the
banks of patterns, samples, and sounds), and renaming.
-}

import Array

import Bank exposing (BankOf, Index(..))
import Bank.Shuffle as Shuffle exposing (Shuffle)
import Elektron.Digitakt.Dump as Dump
import Elektron.Digitakt.Blank as Blank
import Elektron.Digitakt.Related as Rel
import Elektron.Digitakt.Types as T exposing (BankItem, Pattern, Sample, Sound)
import Elektron.Drive as Drive
import Elektron.Instrument as EI exposing (Version)
import Missing.Maybe as Maybe
import SysEx.Dump
import SysEx.SysEx exposing (SysEx)

{-| Note that the binary here is only the project settings structure. The
`Pattern`, `Sample`, and `Sound` values hold on to their respective binary
blobs.

The `blankPattern` is needed so that a pattern slot can be filled with
something if the user deletes a pattern, or moves it, leaving a hole. While
`BankOf` can handle a `Nothing` in a slot, the instrument itself needs every
pattern and sample slot filled, even if they are logically empty.
-}
type alias Project =
  { patterns        : BankOf Pattern
  , samplePool      : BankOf Sample
  , soundPool       : BankOf Sound

  , crossReference  : Rel.CrossReference

  , binary          : Maybe Dump.ProjectSettings

  , blankPattern    : Maybe Dump.PatternKit
  }


emptyProject : EI.Device -> EI.ProjectSpec -> Project
emptyProject device spec =
  let
    projectSettingsVersion = Version device spec.storageVersions.projectSettings
    patternAndKitVersion = Version device spec.storageVersions.patternAndKit

    setZeros = Bank.indexedMapUpdate (\i ms ->
      if T.isZeroSampleIndex (Index i)
        then Just T.zeroSample
        else ms
      )
  in
  { patterns = Bank.initializeEmpty 128
  , samplePool = setZeros <| Bank.initializeEmpty spec.numSampleSlots
  , soundPool = Bank.initializeEmpty 128
  , crossReference = Rel.nullCrossReference
  , binary = Blank.blankProjectSettings projectSettingsVersion
  , blankPattern = Blank.blankPatternKit patternAndKitVersion
  }

projectEmptySound : Project -> Maybe Dump.Sound
projectEmptySound proj =
  proj.blankPattern
  |> Maybe.andThen (\pat -> Array.get 0 pat.kit.sounds)
  |> Maybe.map (\snd ->
      { snd
      | tagMask = 0
      , name = ""
      , sampleSlot = 0
      , sample = Dump.emptySample
      }
    )


rebuildCrossReference : Project -> Project
rebuildCrossReference proj =
  { proj
  | crossReference =
      Rel.buildCrossReference
        proj.patterns proj.samplePool proj.soundPool
  }


{- These three update functions are called as the binary dumps are received
from the instrument.

Note: We rely on the fact that the instrument responds to a whole project
dump request in this order:

  * 128 PatternKit dumps
  * 0 to 128 Sound dumps (the sound pool, only non-empty entries)
  * ProjectSettings

This code (as well as others in the application), use the PatternSettings
as the indication that all has been received.
-}

updateProjectPattern : Int -> Dump.PatternKit -> Project -> Project
updateProjectPattern i dump proj =
  { proj
  | patterns = Bank.put (Index i) (T.buildPatternFromDump dump) proj.patterns
  }

updateProjectSound : Int -> Dump.Sound -> Project -> Project
updateProjectSound i dump proj =
  { proj
  | soundPool = Bank.put (Index i) (T.buildSoundFromDump dump) proj.soundPool
  }

updateProject : Dump.ProjectSettings -> Project -> Project
updateProject dump proj =
  enlivenPhantoms
  <| rebuildCrossReference
    { proj
    | samplePool =
        Bank.fromArray
        <| Array.indexedMap (\i b -> Just <| T.buildSampleFromDump i b)
        <| dump.samples
    , binary = Just dump
    }



{-| Find samples that were marked empty, but have references,
 and mark them not empty.
-}
enlivenPhantoms : Project -> Project
enlivenPhantoms proj =
  let
    enlivenSample i item =
      if T.isEmptyItem item
          && (not (Rel.freeSample (Index i) proj.crossReference))
        then { item
             | status = T.Phantom
             , name =
                if item.name == ""
                  then "---"
                  else item.name
             }
        else item

    phantomSound =
      projectEmptySound proj
      |> Maybe.map ( \snd ->
        { name = "--"  -- this is what is used on the device!
        , status = T.Phantom
        , sampleSlot = Index 0
        , binary = snd
        }
      )
    enlivenSound i mItem =
      case mItem of
        Just _ -> mItem
        Nothing ->
          if Rel.freeSound (Index i) proj.crossReference
            then Nothing
            else phantomSound
  in
    { proj
    | samplePool = Bank.indexedMap enlivenSample proj.samplePool
    , soundPool = Bank.indexedMapUpdate enlivenSound proj.soundPool
    }


{- SysEx Dumps -}

updateFromDump : SysEx.Dump.ElkDump -> Project -> Result String Project
updateFromDump dump project =
  case dump.message of
    SysEx.Dump.DTPatternKitResponse i dPattern -> Ok <| updateProjectPattern i dPattern project
    SysEx.Dump.DTSoundResponse i dSound -> Ok <| updateProjectSound i dSound project
    SysEx.Dump.DTProjectSettingsResponse dProject -> Ok <| updateProject dProject project
    _ -> Err "An unexpected dump type was received."

updateFromSysEx : SysEx -> Project -> Result String Project
updateFromSysEx sysEx project =
    case sysEx of
      SysEx.SysEx.ElektronDump dump -> updateFromDump dump project
      _ -> Err "Something other than dump SysEx was received."


neededSampleNames : Project -> List Drive.HashSize
neededSampleNames project =
  let
    need s =
      if s.needsName
        then Just (T.sampleHashSize s)
        else Nothing
  in
  project.samplePool
  |> Bank.toIndexedList
  |> List.filterMap (\(_, s) -> Maybe.andThen need s)

updateSampleNames : Drive.FileNamesByHash -> Project -> Project
updateSampleNames names project =
  { project
  | samplePool = Bank.map (T.updateSampleName names) project.samplePool
  }


toSysExDumps : Maybe Dump.Sound -> Project -> List SysEx.Dump.ElkDump
toSysExDumps defSound project =
  let
    defPattern = project.blankPattern

    toDump prepAndDump defItem (Index i, item) =
      item
      |> Maybe.map .binary
      |> Maybe.unwrap defItem Just
      |> Maybe.map (prepAndDump i)

    prepAndDumpPatternKit i p =
      SysEx.Dump.DTPatternKitResponse i
        <| Dump.setPatternKitIndex i p
    prepAndDumpSound = SysEx.Dump.DTSoundResponse

    bankDump dumpConst defItem  =
      Bank.toIndexedList >> List.filterMap (toDump dumpConst defItem)

    patternDumps = bankDump prepAndDumpPatternKit defPattern project.patterns
    soundDumps = bankDump prepAndDumpSound defSound project.soundPool
    projectDump = case project.binary of
      Just pb -> [SysEx.Dump.DTProjectSettingsResponse pb]
      Nothing -> []

    allDumps = patternDumps ++ soundDumps ++ projectDump
    device =
      Maybe.map (.version >> .device) project.binary
      |> Maybe.withDefault EI.Unknown
  in
    List.map (SysEx.Dump.ElkDump device) allDumps


toSysExDumpsForFile : Project -> List SysEx.Dump.ElkDump
toSysExDumpsForFile = toSysExDumps Nothing
  -- to match the DT's send sysex function, empty sounds are not output

toSysExDumpsForSend : Project -> List SysEx.Dump.ElkDump
toSysExDumpsForSend proj = toSysExDumps (projectEmptySound proj) proj
  -- to ensure the sound pool is correct, send all sounds, even empty ones


{- SHUFFLING

Shuffling occurs when the user reorders items in a bank, or when they import
items from one bank to another.

While the work reordering the tiems in the bank is taken care of in the `Bank`
module... the important work is fixing up the refrences to the moved items.

For example, when a sample is moved to a new slot, any sound in any pattern that
referenced thad sample has to be updated to refer to the new slot. Same has to
happen with any sample slot plocks, and any sounds in the sound pool.
-}

mapPLock : Shuffle a -> T.PLock a -> T.PLock a
mapPLock shuf = Maybe.andThen (Shuffle.rereference shuf)

mapPLocks : Shuffle a -> T.PLocks a -> T.PLocks a
mapPLocks shuffle = Array.map (mapPLock shuffle)


rerefSoundSampleRef : Shuffle Sample -> Sound -> Sound
rerefSoundSampleRef shuffle sound =
  let
    origSampleRef = sound.sampleSlot
    newSampleRef =
      Shuffle.rereference shuffle origSampleRef
      |> Maybe.withDefault (Index 0)
    (Index newSampleInt) = newSampleRef
  in
    if newSampleRef == origSampleRef
      then sound
      else
        { sound
        | sampleSlot = newSampleRef
        , binary = Dump.setSoundSampleSlot newSampleInt sound.binary
        }

rerefPatternSampleRefs : Shuffle Sample -> Pattern -> Pattern
rerefPatternSampleRefs shuffle pattern =
  let
    trackSounds = Array.map (rerefSoundSampleRef shuffle) pattern.trackSounds
    samplePlocks = Array.map (Maybe.map <| mapPLocks shuffle) pattern.samplePlocks

    binaryPlocks i =
      Array.get i samplePlocks
      |> Maybe.withDefault Nothing
      |> Maybe.map (Array.map T.plockToMaybe)

    binary = pattern.binary
    binaryPattern = binary.pattern
    pLocks = binaryPattern.pLocks
    pLocks_ = Array.indexedMap (\i p -> Dump.setPlockSamplePlocks (binaryPlocks i) p) pLocks
    kit = binary.kit
    sounds = kit.sounds
    sounds_ = Array.map .binary trackSounds
  in
    { pattern
    | trackSounds = trackSounds
    , samplePlocks = samplePlocks
    , binary =
        { binary
        | pattern = { binaryPattern | pLocks = pLocks_ }
        , kit = { kit | sounds = sounds_}
        }
    }

rerefPatternSoundRefs : Shuffle Sound -> Pattern -> Pattern
rerefPatternSoundRefs shuffle pattern =
  let
    soundPlocks = Array.map (mapPLocks shuffle) pattern.soundPlocks

    binaryPlocks i =
      Array.get i soundPlocks
      |> Maybe.map (Array.map T.plockToMaybe)

    setTrackSoundPlocks i t =
      case binaryPlocks i of
        Just p -> Dump.setTrackSoundPlocks p t
        Nothing -> t

    binary = pattern.binary
    binaryPattern = binary.pattern
    binaryTracks = binaryPattern.tracks
    binaryTracks_ = Array.indexedMap setTrackSoundPlocks binaryTracks
  in
    { pattern
    | soundPlocks = soundPlocks
    , binary =
        { binary
        | pattern = { binaryPattern | tracks = binaryTracks_ }
        }
    }

rebuildBinarySamples : Project -> Project
rebuildBinarySamples project =
  let
    newBinary projectBinary =
      { projectBinary
      | samples =
        Array.map (Maybe.unwrap Dump.emptySample .binary)
        <| Bank.toArray project.samplePool
      }
  in
    { project
    | binary = Maybe.map newBinary project.binary
    }

shufflePatterns : Shuffle Pattern -> Project -> Project
shufflePatterns shuffle p =
  { p | patterns = Shuffle.reorder shuffle p.patterns }

shuffleSamples : Shuffle Sample -> Project -> Project
shuffleSamples shuffle project =
  let
    patterns = Bank.map (T.onNonEmpty <| rerefPatternSampleRefs shuffle) project.patterns
    samplePool = Shuffle.reorder shuffle project.samplePool
    soundPool = Bank.map (rerefSoundSampleRef shuffle) project.soundPool
  in
    rebuildBinarySamples
      { project
      | patterns = patterns
      , samplePool = samplePool
      , soundPool = soundPool
      }

shuffleSounds : Shuffle Sound -> Project -> Project
shuffleSounds shuffle project =
  { project
  | patterns = Bank.map (rerefPatternSoundRefs shuffle) project.patterns
  , soundPool = Shuffle.reorder shuffle project.soundPool
  }


{- IMPORTING -}

type alias Shuffles =
  { patterns : Shuffle Pattern
  , samples : Shuffle Sample
  , sounds : Shuffle Sound
  }

{-| Import items from one project into another. The first set of shuffles
describes those items that are to move. The second set of shuffles describes
those items that aren't moved, but already exist in the destination project.
-}
importProject : Shuffles -> Shuffles -> Project -> Project -> Project
importProject moving exisiting importProj baseProj =
  let
    rerefSamplesShuffle = Shuffle.mergeShuffles moving.samples exisiting.samples
    rerefSoundsShuffle = Shuffle.mergeShuffles moving.sounds exisiting.sounds

    samplePool =
      Shuffle.importFrom moving.samples identity
        importProj.samplePool baseProj.samplePool

    soundPool =
      Shuffle.importFrom moving.sounds
        (rerefSoundSampleRef rerefSamplesShuffle)
        importProj.soundPool baseProj.soundPool

    patterns =
      Shuffle.importFrom moving.patterns
        (rerefPatternSampleRefs rerefSamplesShuffle
          >> rerefPatternSoundRefs rerefSoundsShuffle)
        importProj.patterns baseProj.patterns

  in
    rebuildBinarySamples
      { baseProj
      | patterns = patterns
      , samplePool = samplePool
      , soundPool = soundPool
      }


findWithin :
  (BankItem a -> BankItem a -> Bool)
  -> BankOf (BankItem a)
  -> BankOf (BankItem a)
  -> Shuffle (BankItem a)
findWithin eq base source =
  let
    nonEmptyItem (i, ma) =
      case ma of
        Nothing -> Nothing
        Just a ->
          if T.isOccupiedItem a
            then Just (i, a)
            else Nothing

    itemList = Bank.toIndexedList >> List.filterMap nonEmptyItem

    baseList = itemList base
    sourceList = itemList source

    search bases ((i, srcItem) as src) =
      case bases of
        [] -> Nothing
        (j, baseItem) :: rest ->
          if eq srcItem baseItem
            then Just (i, j)
            else search rest src
  in
    Shuffle.asImport <| List.filterMap (search baseList) sourceList


findSameSamples : Project -> Project -> Shuffle Sample
findSameSamples base proj =
  let
    sameSample a b = Dump.sameSample a.binary b.binary
  in
    Shuffle.mergeShuffles
      (findWithin sameSample base.samplePool proj.samplePool)
      (Shuffle.asImport [(Index 0, Index 0)])
          -- "OFF" is always mapped, even though it is empty

findSameSounds : Project -> Project -> Shuffle Sound
findSameSounds base proj =
  let
    sameSound a b = Dump.sameSound a.binary b.binary
  in
    findWithin sameSound base.soundPool proj.soundPool


{- NAMES -}

setPatternName : Index Pattern -> String -> Project -> Project
setPatternName idx s p =
  let
    update pat =
      let
        binary_ = Dump.setPatternKitName s pat.binary
      in
        { pat
        | name = Dump.patternKitName binary_
        , binary = binary_
        }
  in
    { p | patterns = Bank.update (Maybe.map update) idx p.patterns }


setSoundName : Index Sound -> String -> Project -> Project
setSoundName idx s p =
  let
    update snd =
      let
        binary_ = Dump.setSoundName s snd.binary
      in
        { snd
        | name = Dump.soundName binary_
        , binary = binary_
        }
  in
    { p | soundPool = Bank.update (Maybe.map update) idx p.soundPool }

