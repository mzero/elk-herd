module Elektron.Digitakt.Related exposing
  ( Related
  , noRelations
  , mergeRelations
  , isRelatedPattern
  , isRelatedSample
  , isRelatedSound
  , relatedPatterns
  , relatedSamples
  , relatedSounds

  , CrossReference
  , nullCrossReference
  , buildCrossReference

  , patternRelated
  , sampleRelated
  , soundRelated

  , unusedPatterns
  , unusedSamples
  , unusedSounds

  , freePattern
  , freeSample
  , freeSound
  )


{-| This is really the heart of elk-herd! The cross refernce keeps track of
which items are related to other items, and enables the UI to be repsonsive.
-}

import Array exposing (Array)
import Set

import Bank exposing (Bank, BankOf, Index(..))
import Elektron.Digitakt.Types exposing (..)
import Missing.Maybe as Maybe

{-| These arrays parallel a Bank and say if an item is related or not. The
phantom type `i` is same as the type of items being related to.
-}
type RelationArray i = RA (Array Bool)

{-  As implemented it is an invariant that the last element in the Array is
always True.

TODO: Do some benchmarking to see which is better: this or `Set (Index i)`
-}

emptyRelationArray : RelationArray i
emptyRelationArray = RA Array.empty

isEmptyRelationArray : RelationArray i -> Bool
isEmptyRelationArray (RA ra) = Array.length ra == 0

isRelated : Index i -> RelationArray i -> Bool
isRelated (Index i) (RA ra) = Array.get i ra |> Maybe.withDefault False

relatedExtent : RelationArray i -> Int
relatedExtent (RA ra) = Array.length ra

asRelationArray : List (Index i) -> RelationArray i
asRelationArray indicies =
  let
    intList = List.map Bank.indexToInt indicies
    intSet = Set.fromList intList
  in
    case List.maximum intList of
      Nothing -> emptyRelationArray
      Just imax ->
        RA <| Array.initialize (imax + 1) (\i -> Set.member i intSet)

asRelatedIndices : RelationArray i -> List (Index i)
asRelatedIndices (RA ra) =
  let
    include (i, b) =
      if b
        then Just (Index i)
        else Nothing
  in
    Array.toIndexedList ra |> List.filterMap include

mergeRelated : List (RelationArray i) -> RelationArray i
mergeRelated ras =
  case ras of
    [] -> emptyRelationArray
    [ra] -> ra
    _ ->
      let
        n = List.foldl (relatedExtent >> max) 0 ras
        inAny i = List.any (isRelated (Index i)) ras
      in
        RA <| Array.initialize n inAny

transpose : Array (RelationArray a) -> Array (RelationArray b)
transpose outer =
  {- Note there is a loss of type safety here as we don't know what type
  the array's axis is. Thus this can turn a RelationArray into any other type
  while transposing it.
  -}
  let
    size = Array.foldl (relatedExtent >> max) 0 outer
    indexedOuter = Array.toIndexedList outer
    inlcude i (j, ra) =
      if isRelated (Index i) ra
        then Just <| Index j
        else Nothing
    slice i =
      indexedOuter
      |> List.filterMap (inlcude i)
      |> asRelationArray
  in
    Array.initialize size slice

{-| Represents the items related to something. For example, there is one of
these objects for a pattern, and indicates which other items that pattern
references.

For patterns, of course it doesn't reference other patterns, but it is easiest
to treat all items has having all three arrays, even if some of empty.

Note that the relation goes either way, depending on the types.  For example
  A sound's `RelationArray Sample` indicates which samples the sound refers to.
  A sound's `RelationArray Pattern` indicates which patterns refer to the sound.
-}
type Related =
  Related
    { patterns : RelationArray Pattern
    , samples : RelationArray Sample
    , sounds : RelationArray Sound
    }


noRelations : Related
noRelations =
  Related
    { patterns = emptyRelationArray
    , samples = emptyRelationArray
    , sounds = emptyRelationArray
    }


mergeRelations_ : List Related -> Related
mergeRelations_ relateds =
  let
    pick sel (Related r) = sel r

    merge sel = mergeRelated <| List.map (pick sel) relateds
  in
    Related
      { patterns = merge .patterns
      , samples = merge .samples
      , sounds = merge .sounds
      }

mergeRelations : List Related -> Related
mergeRelations relateds =
  case relateds of
    [] -> noRelations
    [r] -> r
    rs -> mergeRelations_ rs

isRelatedPattern : Index Pattern -> Related -> Bool
isRelatedPattern i (Related r) = isRelated i r.patterns

isRelatedSample : Index Sample -> Related -> Bool
isRelatedSample i (Related r) = isRelated i r.samples

isRelatedSound : Index Sound -> Related -> Bool
isRelatedSound i (Related r) = isRelated i r.sounds


relatedPatterns : Related -> List (Index Pattern)
relatedPatterns (Related r) = asRelatedIndices r.patterns

relatedSamples : Related -> List (Index Sample)
relatedSamples (Related r) = asRelatedIndices r.samples

relatedSounds : Related -> List (Index Sound)
relatedSounds (Related r) = asRelatedIndices r.sounds


{-| This is the big cheese: For every pattern, sample, and sound, a `Related`
object.

Remember that items are related if they reference, or are referenced:

  * A patternRelated entry's `Related` has
      .pattern - always empty
      .samples - samples the pattern references
      .sounds  - sounds the pattern references

  * A smapleRelated entry's `Related` has
      .pattern - patterns that reference this sample
      .samples - always empty
      .sounds  - sounds that reference this sample

  * A soundRelated entry's `Related` has
      .pattern - patterns that reference this sound
      .samples - samples the sound references
      .sounds  - always empty

You can see that every reference from item a to item b is in there twice:
Once under a, for its reference to b, and once under b for being referenced
by a.
-}
type alias CrossReference =
  { patternRelated: Bank Pattern Related
  , sampleRelated: Bank Sample Related
  , soundRelated: Bank Sound Related
  }

nullCrossReference : CrossReference
nullCrossReference =
  CrossReference
    (Bank.initializeEmpty 0) (Bank.initializeEmpty 0) (Bank.initializeEmpty 0)


buildCrossReference :
  BankOf Pattern -> BankOf Sample -> BankOf Sound -> CrossReference
buildCrossReference patterns samplePool soundPool =
  let
    soundSample : Index Sound -> Maybe (Index Sample)
    soundSample i =
      Bank.get i soundPool |> Maybe.map .sampleSlot

    patternRelations : Pattern -> (RelationArray Sample, RelationArray Sound)
    patternRelations pattern =
      let
        samp1 = List.map .sampleSlot <| Array.toList pattern.trackSounds
        samp2 =
          List.filterMap identity
          <| List.concat
          <| List.filterMap (Maybe.map Array.toList)
          <| Array.toList pattern.samplePlocks
        sounds =
          List.filterMap identity
          <| List.concat
          <| List.map Array.toList
          <| Array.toList pattern.soundPlocks
        samp3 = List.filterMap soundSample sounds
        samples = samp1 ++ samp2 ++ samp3
      in
        (asRelationArray samples, asRelationArray sounds)

    soundRelation : Sound -> RelationArray Sample
    soundRelation sound =
      asRelationArray [sound.sampleSlot]

    (patternToSample, patternToSound) =
      Bank.toIndexedList patterns
      |> List.map
        (\(i, mPattern) ->
          mPattern
          |> Maybe.andThen (\p ->
            if isEmptyItem p
              then Nothing
              else Just (patternRelations p)
            )
          |> Maybe.withDefault (emptyRelationArray, emptyRelationArray)
        )
      |> List.unzip
      |> (\(a, b) -> (Array.fromList a, Array.fromList b))

    soundToSample =
      Bank.toIndexedList soundPool
      |> List.map
        (\(i, mSound) ->
          case mSound of
            Nothing -> emptyRelationArray
            Just sound -> soundRelation sound
        )
      |> Array.fromList


    sampleToPattern = transpose patternToSample
    soundToPattern = transpose patternToSound
    sampleToSound = transpose soundToSample

    buildRelated mPatRefs mSampRefs mSoundRefs =
      Related
        { patterns = Maybe.withDefault emptyRelationArray mPatRefs
        , samples = Maybe.withDefault emptyRelationArray mSampRefs
        , sounds = Maybe.withDefault emptyRelationArray mSoundRefs
        }

    patternR i _ =
      Just
      <| buildRelated
          Nothing (Array.get i patternToSample) (Array.get i patternToSound)
    sampleR i _ =
      Just
      <| buildRelated
          (Array.get i sampleToPattern) Nothing (Array.get i sampleToSound)
    soundR i _ =
      Just
      <| buildRelated
          (Array.get i soundToPattern) (Array.get i soundToSample) Nothing

  in
    { patternRelated   = Bank.indexedMapUpdate patternR patterns
    , sampleRelated   = Bank.indexedMapUpdate sampleR samplePool
    , soundRelated  = Bank.indexedMapUpdate soundR soundPool
    }


{-| Does anything refer to this sample? Patterns and sounds point to samples,
so we check those two arrays.
-}
sampleHasNoRelations : Related -> Bool
sampleHasNoRelations (Related r) =
  isEmptyRelationArray r.patterns && isEmptyRelationArray r.sounds

{-| Does anything refer to this sound? Only patterns refer to sounds, so we
check that Array.
-}
soundHasNoRelations : Related -> Bool
soundHasNoRelations (Related r) =
  isEmptyRelationArray r.patterns


isFree : (Related -> Bool) -> Bank i Related -> Index i -> Bool
isFree hasNoRelations relBank idx =
  Bank.get idx relBank |> Maybe.unwrap True hasNoRelations

unrelatedItems :
  (Related -> Bool) -> Bank i (BankItem a) -> Bank i Related
  -> List (Index i)
unrelatedItems hasNoRelations itemBank relBank =
  let
    test pair =
      case pair of
        (idx, Just a) ->
          if isEmptyItem a
            then Nothing
            else
              if isFree hasNoRelations relBank idx
                then Just idx
                else Nothing
        _ -> Nothing
  in
    Bank.toIndexedList itemBank |> List.filterMap test


itemRelated : Bank i Related -> Index i -> Related
itemRelated bank index = Bank.get index bank |> Maybe.withDefault noRelations

patternRelated : CrossReference -> Index Pattern -> Related
patternRelated cr = itemRelated cr.patternRelated

sampleRelated : CrossReference -> Index Sample -> Related
sampleRelated cr = itemRelated cr.sampleRelated

soundRelated : CrossReference -> Index Sound -> Related
soundRelated cr = itemRelated cr.soundRelated


unusedPatterns : BankOf Pattern -> CrossReference -> List (Index Pattern)
unusedPatterns bank cr = []

unusedSamples : BankOf Sample -> CrossReference -> List (Index Sample)
unusedSamples bank cr =
  unrelatedItems sampleHasNoRelations bank cr.sampleRelated
  |> List.filter (isZeroSampleIndex >> not)

unusedSounds : BankOf Sound -> CrossReference -> List (Index Sound)
unusedSounds bank cr =
  unrelatedItems soundHasNoRelations bank cr.soundRelated

freePattern : Index Pattern -> CrossReference -> Bool
freePattern idx cr = True

freeSample : Index Sample -> CrossReference -> Bool
freeSample idx cr =
  not (isZeroSampleIndex idx)
  && isFree sampleHasNoRelations cr.sampleRelated idx

freeSound : Index Sound -> CrossReference -> Bool
freeSound idx cr = isFree soundHasNoRelations cr.soundRelated idx

