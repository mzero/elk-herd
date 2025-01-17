module Elektron.Digitakt.Types exposing
  ( Status(..)
  , BankItem
  , isEmptyItem
  , isPhantomItem
  , onNonEmpty

  , Pattern
  , Sample
  , Sound

  , sampleHashSize
  , updateSampleName

  , PLock
  , PLocks
  , plockFromMaybe
  , plockToMaybe

  , SamplePLocks
  , SoundPLocks

  , buildPatternFromDump
  , buildSampleFromDump
  , buildSoundFromDump
  )

{-| High-level project data structures: These structures match the way elk-herd
presents the project, and is closer to how the instrument's UI operates.

These structures also contain cross-reference indicies that enable moving items
around and having references to them fixed up to the new slot numbers.
-}

import Array exposing (Array)
import Dict

import Bank exposing (Index(..))
import Elektron.Digitakt.Dump as Dump
import Elektron.Drive as Drive


{-| While `Bank` allows for entries to be actually empty (the slot returns
`Nothing`), in practice, that isn't how the instrument works. The pattern
slots and sample pool always have something in them, just some values are
considered by the machine "empty".

In addition there are entries that fall into limbo: The instrucment treats
them as empty, but in fact they have user content in them. For example, a
pattern that has no trigs, and no name, but has user assign kit settings.
These are marked as `Phantom`.
-}
type Status = Empty | Phantom | Live
type alias BankItem a = { a | status : Status, name : String }

isEmptyItem : BankItem a -> Bool
isEmptyItem a = a.status == Empty

isPhantomItem : BankItem a -> Bool
isPhantomItem a = a.status == Phantom

onNonEmpty : (BankItem a -> BankItem a) -> BankItem a -> BankItem a
onNonEmpty f item =
  if item.status == Empty
    then item
    else f item

{- Each of the following types contains the information needed for the UI.
Most of this information has been pulled out of the dump structures sent by
the instrument. That data is "mirrored" here, usually in a more normalized
form.

While it is usually an bad idea to have the same information in two places,
this is done for several reasons:
  * We must be able to "rount-trip" the binary structures, and keeping all
    the extra data here would over-shadow the information we care about.
  * The data needed by the application is often spread out in the binary
    structures, and burdoning the upper layers with that seems bad. In
    particular, there are many more structures involved in the binary layer,
    than the just three data types here.

Each type here has a name, status, and other data fields. Where that data
comes from in the lower level data structure is noted in comments.

Each type also has a field `binary` that holds the lower level structure
it represents.

TODO: `binary` is a misnomer... but `dump` seems like a poor word to have
in here... anyone have a better name?
-}


type alias Pattern =
  { name         : String             -- mirrors binary.pattern.name
                                      -- & mirrors binary.kit.name
  , status       : Status

  , trackSounds  : Array Sound        -- 8 x, mirrors binary.kit.sound
  , soundPlocks  : Array SoundPLocks  -- 8 x, mirrors binary.sequence.tracks
  , samplePlocks : Array (Maybe SamplePLocks)
    -- 80 x, mirrors binary.sequence.plocks
    -- Only plocks that reference the sample slot are mirrored here, a most
    -- there will be one per track. The other plocks are not mirrored, and
    -- are just Nothing

  , binary       : Dump.PatternKit
  }

type alias Sample =
  { name : String       -- cached from +Drive
  , status : Status
  , needsName : Bool

  , binary : Dump.Sample
  }

type alias Sound =
  { name : String              -- mirrors binary
  , status : Status
  , sampleSlot: Index Sample   -- mirrors binary

  , binary : Dump.Sound
  }


sampleHashSize : Sample -> Drive.HashSize
sampleHashSize s = Drive.hashSize s.binary.hash (Dump.sampleLength s.binary)

updateSampleName : Drive.FileNamesByHash -> Sample -> Sample
updateSampleName names s =
  if s.needsName
    then
      case Dict.get (sampleHashSize s) names of
        Just name -> { s | name = name, needsName = False }
        _         -> s
    else
      s


{- These high-level versions of `PLock` ensure that the value is indexing the
correct `Bank` by using `Index a`.
-}
type alias PLock a = Maybe (Index a)
type alias PLocks a = Array (PLock a)

type alias SamplePLocks = PLocks Sample
type alias SoundPLocks = PLocks Sound

{- These are only for converting to and from the binary dumps.
-}
plockFromMaybe : Maybe Int -> PLock a
plockFromMaybe = Maybe.map Index

plockToMaybe : PLock a -> Maybe Int
plockToMaybe = Maybe.map Bank.indexToInt


{- Converting from the dump structures.

These build up the high level versions from the low. You can see how each
high level field is extracted.

There are functions going the other way: In `Elektron.Digitakt.HighLevel`,
functions that modify the high level data, also update the low level so that
they are always kept in sync. It is argueable that those modification funcionts
should be better abstracted and placed here.
-}


buildPatternFromDump : Dump.PatternKit -> Pattern
buildPatternFromDump dPatternKit =
  let
    name = Dump.patternKitName dPatternKit

    trackSounds = Array.map buildSoundFromDump dPatternKit.kit.sounds

    buildSoundPLocksFromDump : Dump.Track -> SoundPLocks
    buildSoundPLocksFromDump = Array.map plockFromMaybe << Dump.trackSoundPLocks

    buildSamplePlocksFromDump : Dump.PLock -> Maybe SamplePLocks
    buildSamplePlocksFromDump =
      Maybe.map (Array.map plockFromMaybe) << Dump.plockSamplePLocks

    hasTrigs =
      List.any Dump.anyTrigsSet
      <| Array.toList dPatternKit.pattern.tracks

    status =
      if hasTrigs
        then Live
        else
          if (name /= "" && name /= "UNTITLED")
              || not (Dump.isDefaultKit dPatternKit.kit)
            then Phantom
            else Empty


    name_ =
      if status == Empty
        then if name == "UNTITLED" then "" else name
        else if name == "" then "UNTITLED" else name
        {- Whaaaaat? Lemme explain....
        If the pattern is actually empty, then we want to show nothing for
        the name. Indeed, on newer instrument OS, the pattern has the empty
        string as the name. But older ones had it set to "UNITITLED" and so
        for the UI we use the name ""

        On the other hand, if the pattern is actually modified by the user,
        but they haven't given it a name, on new instrument OS, the pattern
        still has the empty string as the name.. but we want to show the user
        the pattern is in use, so we substitute the name "UNTITLTED".
        -}
  in
    { name = name_
    , status = status
    , trackSounds = trackSounds
    , soundPlocks = Array.map buildSoundPLocksFromDump dPatternKit.pattern.tracks
    , samplePlocks = Array.map buildSamplePlocksFromDump dPatternKit.pattern.pLocks
    , binary = dPatternKit
    }

buildSampleFromDump : Dump.Sample -> Sample
buildSampleFromDump dSample =
  let
    empty = Dump.isEmptySample dSample
  in
    { name = if empty then "" else "???"
    , status = if empty then Empty else Live
    , needsName = not empty
    , binary = dSample
    }

buildSoundFromDump : Dump.Sound -> Sound
buildSoundFromDump dSound =
  { name = Dump.soundName dSound
  , status = Live
  , sampleSlot = Index <| Dump.soundSampleSlot dSound
  , binary = dSound
  }
