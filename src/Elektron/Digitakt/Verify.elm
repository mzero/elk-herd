module Elektron.Digitakt.Verify exposing
  ( validateProject
  )

{-| Checks to make sure that the data received (or opened from a file) is
consistent and valid.
-}

import Array

import Bank exposing (Index)
import Bank.IndexSet as IndexSet
import Elektron.Digitakt.HighLevel exposing (..)
import Elektron.Digitakt.Types exposing (..)
import Elektron.Instrument as EI
import Missing.List as List
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


type Test a = TestPass a | TestWarn a String | TestFail String

type alias TestChain a = (Maybe a, List String)

start : TestChain ()
start = (Just (), [])

andThen : Test a -> (a -> TestChain a -> TestChain c) -> TestChain b -> TestChain c
andThen test fChain ((result, msgs) as chain) =
  case test of
    TestPass a      -> (fChain a) (Maybe.map (always a) result, msgs)
    TestWarn a msg  -> (fChain a) (Maybe.map (always a) result, msgs ++ [msg])
    TestFail msg    ->            (Nothing,                     msgs ++ [msg])

andTest : Test b -> TestChain a -> TestChain b
andTest test ((result, msgs) as chain) =
  case test of
    TestPass b     -> (Maybe.map (always b) result,  msgs)
    TestWarn b msg -> (Maybe.map (always b) result,  msgs ++ [msg])
    TestFail msg   -> (Nothing,                      msgs ++ [msg])

finish : (a -> Maybe String -> r) -> TestChain a -> Result String r
finish fResult (result, msgs) =
  let
    msg = String.join "\n\n" msgs
  in
  case result of
    Just a -> Ok (fResult a (if List.isEmpty msgs then Nothing else Just msg))
    Nothing -> Err msg


type alias Validation =
  { device : EI.Device
  , versions: EI.StorageVersions

  , warning: Maybe String
  }

validateProject : EI.Instrument -> Project -> Result String Validation
validateProject inst proj =
  let
    projectSettingsVersion =
      case proj.binary of
        Just b -> TestPass b.version
        Nothing -> TestFail "Project dump is missing final project SysEx message."

    matchingDevice device =
      if device == inst.device
        then TestPass ()
        else
          let
            projDeviceName = EI.productName device
            instDeviceName = EI.productName inst.device
          in
            TestFail <| String.join ""
              [ "This project is for a ", projDeviceName, ", "
              , "but your instrument is a ", instDeviceName, ". "
              , "elk-herd can't convert it, but the Transfer application might "
              , "be able to."
              ]

    test bank indicies resultIfMissing =
      List.foldl (\i -> Bank.get i bank |> Maybe.isJust |> (&&)) True indicies
      |> (\allThere -> if allThere then TestPass () else TestWarn () resultIfMissing)

    testAllThere bank resultIfMissing =
      Bank.toArray bank
      |> Array.foldl (Maybe.isJust >> (&&)) True
      |> (\allThere -> if allThere then TestPass () else TestFail resultIfMissing)

    missingPatterns =
      testAllThere proj.patterns
        "Project dump does not have all the patterns."

    missingSamples =
      testAllThere proj.samplePool
        "Project dump does not have all the samples."
        -- Should never happen, since we've already tested the project.binary
        -- and the sample pool is in that structure in total.

    missingSounds =
      test proj.soundPool (allSoundPLocks proj)
        """
Project dump sound pool is missing some sounds referenced in pattern
sound plocks. The project is playable, but those plocks will not
sound until sounds are placed in those locations in the pool.
"""

    allPatternKitVersions =
      Bank.toArray proj.patterns
      |> Array.toList
      |> List.filterMap
        (Maybe.map (\p ->
          ( p.binary.pattern.version
          , p.binary.kit.version
          )
        )
      )

    patternKitVersion =
      case allPatternKitVersions of
        [ ] -> TestFail "All patterns are missing."
        ((pv, kv) :: rest) -> TestPass pv

    mismatchedPatternKitVersions v0 =
      if List.all (\(pv, kv) -> pv == v0 && kv == v0) allPatternKitVersions
        then TestPass ()
        else TestFail "Pattern & kit structures do not all match"

    instrumentCompatibility psv pkv =
      case inst.projectSpec of
        Nothing -> TestPass ()    -- we don't know!
        Just ps ->
          let
            instPsv = ps.storageVersions.projectSettings
            instPkv = ps.storageVersions.patternAndKit
          in
          if (psv.int > instPsv || pkv.int > instPkv)
            then TestFail """
This project is from a newer version of the Digitakt OS than your
machine has. Sending this project to your Digitakt will not work. Consider
upgrading your Digitakt to the lastest OS release.
"""
            else if (psv.int < instPsv || pkv.int < instPkv)
              then TestWarn () """
This project is from an older version of the Digitakt OS than your machine.
Sending this project to the Digitakt should upgrade the project, but you'll
want to check the patterns to makes sure everything is as you expect.
"""
              else TestPass ()

    tests =
      andThen projectSettingsVersion (\psv ->
        andThen (matchingDevice psv.device) (\_ ->
          andThen patternKitVersion (\pkv ->
            andTest missingPatterns
            >> andTest missingSamples
            >> andTest missingSounds
            >> andTest (mismatchedPatternKitVersions pkv)
            >> andTest (instrumentCompatibility psv pkv)
            >> andTest (TestPass (psv, pkv))
          )
        )
      )

    build (psv, pkv) msg =
      { device = psv.device
      , versions = { projectSettings = psv.int, patternAndKit = pkv.int }
      , warning = msg
      }

  in
    start |> tests |> finish build
