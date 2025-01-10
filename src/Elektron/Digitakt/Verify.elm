module Elektron.Digitakt.Verify exposing
  ( validateProject
  )

{-| Checks to make sure that the data received (or opened from a file) is
consistent and valid.
-}

import Array exposing (Array)

import Bank exposing (Bank, Index(..))
import Bank.IndexSet as IndexSet
import Elektron.Digitakt.HighLevel exposing (..)
import Elektron.Digitakt.Types exposing (..)

uniqueIndicies : List (Index a) -> List (Index a)
uniqueIndicies = IndexSet.fromList >> IndexSet.toList

allIndicies : List (Index a)
allIndicies = List.map Index <| List.range 0 127



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


missingIndicies : Bank i a -> List (Index i) -> List (Index i)
missingIndicies bank indicies =
  let
    missingIndex i =
      case Bank.get i bank of
        Nothing -> Just i
        Just _ -> Nothing
  in
    List.filterMap missingIndex indicies

indiciesString : List (Index a) -> String
indiciesString indicies =
  let
    indexStr (Index i) = String.fromInt i
  in
    String.join ", " (List.map indexStr indicies)


validateProject : Project -> (Maybe String, Bool)
validateProject proj =
  let
    failing str = (Just str, False)
    warning str = (Just str, True)
    good = (Nothing, True)

    test bank indicies resultFn str =
      if List.isEmpty (missingIndicies bank indicies)
        then Nothing
        else Just (resultFn str)

    orElse fmb ma =
      case ma of
        Nothing -> fmb ()
        Just a -> Just a

    patternMessage =
      "Project dump does not have all the patterns."
    sampleMessage =
      "Project dump is missing final project SysEx message."
      -- because the sample pool is in the projectSettings structure, sent at
      -- the end of a whole project dump
    soundMessage =
      """
Project dump sound pool is missing some sounds referenced in pattern
sound plocks. The project is playable, but those plocks will not
sound until sounds are placed in those locations in the pool.
"""
  in
    test proj.patterns allIndicies failing patternMessage
    |> orElse (\_ -> test proj.samplePool allIndicies failing sampleMessage)
    |> orElse (\_ -> test proj.soundPool (allSoundPLocks proj) warning soundMessage)
    |> Maybe.withDefault good

