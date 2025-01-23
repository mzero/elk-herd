module TestBlanks exposing (tests)

import Expect
import Test as T

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import Elektron.Digitakt.Blank as Blank
import Elektron.Digitakt.CppStructs as CppStructs
import Elektron.Digitakt.Dump as Dump
import Elektron.Instrument exposing (Device(..), Version, productName)


tests : T.Test
tests = T.describe "Elektron.Digitakt.Blank"
  [ testPatternKit Digitakt 9
  , testProjectSettings Digitakt 7

  , testPatternKit Digitakt2 0
  , testProjectSettings Digitakt2 0
  ]

testPatternKit : Device -> Int -> T.Test
testPatternKit device maxV =
  T.describe (productName device ++ " patternKit")
  <| (
    List.range 0 maxV
    |> List.map (\i ->
      let
        v = Version device i
        md = Blank.blankPatternKitData v
        ms = Maybe.map ByteArray.length md
        pk = Blank.blankPatternKit v
        exepectedSize =
          Maybe.map2 (+)
            (CppStructs.patternStorage_sizeof v)
            (CppStructs.kitStorage_sizeof v)
      in
      T.describe ("version " ++ String.fromInt i)
      [ T.test "length" <|
        \_ -> ms |> Expect.equal exepectedSize
      , T.test "parses" <|
        \_ -> pk |> Expect.notEqual Nothing
      , T.test "pattern version match" <|
        \_ -> pk |> Maybe.map (.pattern >> .version >> .int)  |> Expect.equal (Just i)
      , T.test "kit version match" <|
        \_ -> pk |> Maybe.map (.kit >> .version >> .int)      |> Expect.equal (Just i)
      , T.test "round-trips" <|
        \_ -> Maybe.map (Dump.structPatternKit.encoder >> Builder.build >> ByteArray.toArray) pk
              |> Expect.equal (Maybe.map ByteArray.toArray md)
      ]
    ) )
    ++
    [ T.test "patternKit" <|
      \_ -> Blank.blankPatternKit (Version device (maxV + 1)) |> Expect.equal Nothing
    ]



testProjectSettings : Device -> Int -> T.Test
testProjectSettings device maxV =
  T.describe (productName device ++ "projectSettings")
  <| (
    List.range 0 maxV
    |> List.map (\i ->
      let
        v = Version device i
        md = Blank.blankProjectSettingsData v
        ms = Maybe.map ByteArray.length md
        ps = Blank.blankProjectSettings v
      in
      T.describe ("version " ++ String.fromInt i)
      [ T.test "length" <|
        \_ -> ms |> Expect.equal (CppStructs.projectSettingsStorage_sizeof v)
      , T.test "parses" <|
        \_ -> ps |> Expect.notEqual Nothing
      , T.test "version match" <|
        \_ -> ps |> Maybe.map (.version >> .int) |> Expect.equal (Just i)
      , T.test "round-trips" <|
        \_ -> Maybe.map (Dump.structProjectSettings.encoder >> Builder.build >> ByteArray.toArray) ps
              |> Expect.equal (Maybe.map ByteArray.toArray md)
      ]
    ) )
    ++
    [ T.test "projectSettings" <|
        \_ -> Blank.blankProjectSettings (Version device (maxV + 1)) |> Expect.equal Nothing
    ]

