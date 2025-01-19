module Elektron.Digitakt.Verify exposing
  ( validateProject
  )

{-| Checks to make sure that the data received (or opened from a file) is
consistent and valid.
-}

import Array exposing (Array)

import Bank exposing (Bank, Index)
import Bank.IndexSet as IndexSet
import Elektron.Digitakt.HighLevel exposing (..)
import Elektron.Digitakt.Types exposing (..)
import Missing.Maybe as Maybe

uniqueIndicies : List (Index a) -> List (Index a)
uniqueIndicies = IndexSet.fromList >> IndexSet.toList


allSoundPLocks : Project -> List (Index Sound)
allSoundPLocks proj =
  let
    allPatternSoundPlocks pat =
      Array.toList pat.soundPlocks
      |> List.map Array.toList
      |> List.concat
      |> List.filterMap identity
  in
    Bank.toIndexedList proj.patterns
    |> List.filterMap Tuple.second
    |> List.map allPatternSoundPlocks
    |> List.concat
    |> uniqueIndicies



validateProject : Project -> (Maybe String, Bool)
validateProject proj =
  let
    failing str = (Just str, False)
    warning str = (Just str, True)
    good = (Nothing, True)

    test bank indicies resultIfMissing =
      List.foldl (\i -> Bank.get i bank |> Maybe.isJust |> (&&)) True indicies
      |> (\allThere -> if allThere then Nothing else Just resultIfMissing)

    testAllThere bank resultIfMissing =
      Bank.toArray bank
      |> Array.foldl (Maybe.isJust >> (&&)) True
      |> (\allThere -> if allThere then Nothing else Just resultIfMissing)


    orElse fmb ma =
      case ma of
        Nothing -> fmb ()
        Just a -> Just a

    patternsMissing =
      failing "Project dump does not have all the patterns."
    samplesMissing =
      failing "Project dump is missing final project SysEx message."
      -- because the sample pool is in the projectSettings structure, sent at
      -- the end of a whole project dump
    soundsMissing =
      warning
      """
Project dump sound pool is missing some sounds referenced in pattern
sound plocks. The project is playable, but those plocks will not
sound until sounds are placed in those locations in the pool.
"""
  in
    testAllThere proj.patterns patternsMissing
    |> orElse (\_ -> testAllThere proj.samplePool samplesMissing)
    |> orElse (\_ -> test proj.soundPool (allSoundPLocks proj) soundsMissing)
    |> Maybe.withDefault good

