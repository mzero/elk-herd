module TestBuilder exposing (tests)

import Expect
import Test as T

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import Array


tests : T.Test
tests = T.describe "Builder"
  [ testEmpty, testByte, testUint, testString0
  , testAnd, testList, testMap
  ]


expectBuild : List Int -> Builder.Builder -> Expect.Expectation
expectBuild expected =
  Builder.build >> ByteArray.toList >> Expect.equal expected


testEmpty : T.Test
testEmpty = T.describe "empty"
  [ T.test "zero length" <|
    \_ -> Builder.empty |> expectBuild []
  ]

testByte : T.Test
testByte = T.describe "byte"
  [ T.test "has value"  <| \_ -> Builder.byte  42 |> expectBuild [42]
  , T.test "clamped lo" <| \_ -> Builder.byte  -9 |> expectBuild [0]
  , T.test "clamped hi" <| \_ -> Builder.byte 420 |> expectBuild [255]
  ]

testUint : T.Test
testUint = T.describe "uint"
  [ T.test "uint16be" <|
    \_ -> Builder.uint16be 0x1122     |> expectBuild [ 0x11, 0x22 ]
  , T.test "uint32be" <|
    \_ -> Builder.uint32be 0x11223344 |> expectBuild [ 0x11, 0x22, 0x33, 0x44 ]
  ]

testString0 : T.Test
testString0 = T.describe "string0"
  [ T.test "empty" <|
    \_ -> Builder.string0ascii ""    |> expectBuild [ 0 ]
  , T.test "alpha" <|
    \_ -> Builder.string0ascii "ABC" |> expectBuild [ 0x41, 0x42, 0x43, 0 ]
  ]

testAnd : T.Test
testAnd = T.describe "and"
  [ T.test "order" <|
    \_ -> Builder.byte 1 |> Builder.and (Builder.byte 2)
          |> expectBuild [1, 2]
  , T.test "left empty" <|
    \_ -> Builder.empty |> Builder.and (Builder.byte 2)
          |> expectBuild [2]
  , T.test "right empty" <|
    \_ -> Builder.byte 1 |> Builder.and (Builder.empty)
          |> expectBuild [1]
  ]

testList : T.Test
testList = T.describe "list"
  [ T.test "[uint16be]" <|
    \_ -> Builder.list Builder.uint16be [1, 2, 3, 4]
          |> expectBuild [0, 1, 0, 2, 0, 3, 0, 4]
  , T.test "[string0]" <|
    \_ -> Builder.list Builder.string0ascii ["a", "bi", "cat", "doge", ""]
          |> expectBuild [ 0x61, 0, 0x62, 0x69, 0, 0x63, 0x61, 0x74, 0
                         , 0x64, 0x6f, 0x67, 0x65, 0, 0]
  ]

testMap : T.Test
testMap =
  let
    shifter = ByteArray.toList >> List.map (\i -> i * 16) >> ByteArray.fromList
    expander = ByteArray.toArray
      >> Array.indexedMap (\i b -> List.repeat (i+1) b)
      >> Array.toList
      >> List.concat
      >> ByteArray.fromList
  in
  T.describe "map" <|
  [ T.test "converts" <|
    \_ -> Builder.byteList [ 1, 2, 3, 4] |> Builder.map shifter
          |> expectBuild [ 0x10, 0x20, 0x30, 0x40 ]
  , T.test "localized" <|
    \_ -> Builder.sequence
            [ Builder.byteList [1, 2]
            , Builder.byteList [3, 4] |> Builder.map shifter
            , Builder.byteList [5, 6]
            ]
          |> expectBuild [ 1, 2, 0x30, 0x40, 5, 6 ]
  , T.test "resize" <|
    \_ -> Builder.byteList [ 5, 6, 7, 8 ] |> Builder.map expander
          |> expectBuild [ 5, 6, 6, 7, 7, 7, 8, 8, 8, 8  ]
  ]


