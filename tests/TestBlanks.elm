module TestBlanks exposing (tests)

import Expect
import Test as T

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import Elektron.Digitakt.Blank as Blank
import Elektron.Digitakt.CppStructs as CppStructs
import Elektron.Digitakt.Dump as Dump
import Elektron.Instrument exposing (Device(..))
import Elektron.Struct.Version exposing (Version)


tests : T.Test
tests = T.describe "Elektron.Digitakt.Blank"
  [ testPatternKit
  , testProjectSettings
  , testNoMoreVersions
  ]

testPatternKit : T.Test
testPatternKit = T.describe "patternKit" (
  List.range 0 9
  |> List.map (\v ->
    let
      md = Blank.blankPatternKitData <| Version Digitakt v
      ms = Maybe.map ByteArray.length md
      pk = Blank.blankPatternKit <| Version Digitakt v
      exepectedSize =
        Maybe.map2 (+)
          (CppStructs.patternStorage_sizeof v)
          (CppStructs.kitStorage_sizeof v)
    in
    T.describe ("version " ++ String.fromInt v)
    [ T.test "length" <|
      \_ -> ms |> Expect.equal exepectedSize
    , T.test "parses" <|
      \_ -> pk |> Expect.notEqual Nothing
    , T.test "pattern version match" <|
      \_ -> pk |> Maybe.map (.pattern >> .version >> .int)  |> Expect.equal (Just v)
    , T.test "kit version match" <|
      \_ -> pk |> Maybe.map (.kit >> .version >> .int)      |> Expect.equal (Just v)
    , T.test "round-trips" <|
      \_ -> Maybe.map (Dump.structPatternKit.encoder >> Builder.build >> ByteArray.toArray) pk
            |> Expect.equal (Maybe.map ByteArray.toArray md)
    ]
  ) )


testProjectSettings : T.Test
testProjectSettings = T.describe "projectSettings" (
  List.range 0 7
  |> List.map (\v ->
    let
      md = Blank.blankProjectSettingsData <| Version Digitakt v
      ms = Maybe.map ByteArray.length md
      ps = Blank.blankProjectSettings <| Version Digitakt v
    in
    T.describe ("version " ++ String.fromInt v)
    [ T.test "length" <|
      \_ -> ms |> Expect.equal (CppStructs.projectSettingsStorage_sizeof v)
    , T.test "parses" <|
      \_ -> ps |> Expect.notEqual Nothing
    , T.test "version match" <|
      \_ -> ps |> Maybe.map (.version >> .int) |> Expect.equal (Just v)
    , T.test "round-trips" <|
      \_ -> Maybe.map (Dump.structProjectSettings.encoder >> Builder.build >> ByteArray.toArray) ps
            |> Expect.equal (Maybe.map ByteArray.toArray md)
    ]
  ) )


testNoMoreVersions : T.Test
testNoMoreVersions = T.describe "no more versions"
  [ T.test "patternKit" <|
    \_ -> Blank.blankPatternKit (Version Digitakt 10) |> Expect.equal Nothing
  , T.test "projectSettings" <|
    \_ -> Blank.blankProjectSettings (Version Digitakt 8) |> Expect.equal Nothing
  ]
