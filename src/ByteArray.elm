module ByteArray exposing
  ( ByteArray

  , empty
  , fromList
  , fromArray
  , fromBytes

  , isEmpty
  , length
  , get

  , append
  , concat
  , section
  , splice
  , replace

  , toList
  , toArray
  , toBytes

  , same
  , sameSection

  , hexDump
  , elmIntList
  )

{-| A ByteArray is an indexable sequence of bytes. It is more like `Array Int`
than elm's `Bytes`.

A ByteArray's can be very efficiently sectioned: Taking a sub-section of a
`ByteArray` doesn't involve any copying of data. Instead, each `ByteArray`
object keeps a start index and length, and pointer to the original data.

The down side is that the original data is held in memory as long as any piece
of it is needed. This is a good design choic here, where we often get large data
dumps from the synthesizer, but need to manage many small parts of that data.

The operations here are patterend off of those in `Array` and generally have
the same semantics.

FIXME: This code does not rigourously prevent values outside of 0..255 from
ending up inside the `ByteArray`.  The functions `fromList` and `fromArray` will
admit any `Int`. It would be easy to fix this, but we need to think about the
performance implications: There are many calls to those, often with very big
arrays, and low likelihood of code injecting bad values.
-}

import Array exposing (Array)
import Bytes
import Bytes.Decode
import Bytes.Encode
import Char

import Util


type ByteArray = BA (Array Int) Int Int  -- data, start, length

empty : ByteArray
empty = BA Array.empty 0 0

fromList : List Int -> ByteArray
fromList = fromArray << Array.fromList

fromArray : Array Int -> ByteArray
fromArray data = BA data 0 (Array.length data)

fromBytes : Bytes.Bytes -> ByteArray
fromBytes b =
  let
    decodeIntList = Bytes.Decode.loop (Bytes.width b, []) decodeIntListStep
    decodeIntListStep (n, revList) =
      if (n <= 0)
        then Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse revList))
        else Bytes.Decode.unsignedInt8
          |> Bytes.Decode.map (\i -> Bytes.Decode.Loop (n - 1, i :: revList))
  in
    Bytes.Decode.decode decodeIntList b
    |> Maybe.withDefault []
    |> fromList




isEmpty : ByteArray -> Bool
isEmpty = (==) 0 << length

length : ByteArray -> Int
length (BA _ _ len) = len

get : Int -> ByteArray -> Maybe Int
get i (BA data start len) =
  if 0 <= i && i < len
    then Array.get (start + i) data
    else Nothing

append : ByteArray -> ByteArray -> ByteArray
append a b = concat [ a, b ]

concat : List ByteArray -> ByteArray
concat =
  fromArray << List.foldl (\ba a -> Array.append a (toArray ba)) Array.empty

{-| This is similar to `Array.slice`, but without the awkward treatment of
negative indeicies. Instead, the start and length will be clamped to the
bounds of the `ByteArray`:

    section  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2]
    section  3  9 (fromList [0,1,2,3,4]) == fromList [3,4]
    section -1  3 (fromList [0,1,2,3,4]) == fromList [0,1]
-}
section : Int -> Int -> ByteArray -> ByteArray
section sectStart sectLen (BA data start len) =
  let
    start_ = start + sectStart
    end_ = start_ + sectLen
    end = start + len
    len_ = (min end_ end) - (min start_ end)
  in
    BA data start_ len_

{-| Replaces a section with another `ByteArray`. Start and length are treated
as they are for `section`.
-}
splice : Int -> Int -> ByteArray -> ByteArray -> ByteArray
splice sectStart sectLen replaceBa ba =
  concat
    [ section 0 sectStart ba
    , replaceBa
    , section (sectStart + sectLen) (length ba) ba
    ]

{-| Replaces the bytes at the start index with those from another `ByteArray`

    replace 2 (fromList [ 10, 11, 12]) (fromList [ 0, 1, 2, 3, 4, 5 ])
      == fromList [ 0, 1, 10, 11, 12, 5 ]

FIXME: This doesn't check that the start is within range, or that the
whole replacement fits. If they don't, you'll end up with an incorrect
result.
-}
replace : Int -> ByteArray -> ByteArray -> ByteArray
replace sectStart replaceBa =
  splice sectStart (length replaceBa) replaceBa

toList : ByteArray -> List Int
toList = Array.toList << toArray

toArray : ByteArray -> Array Int
toArray (BA data start len) =
  -- Array.slice will already do this optimization, returning the original
  -- without copying if the slice is everything... but doing it early, here,
  -- still gains a 15% speedup!
  if (start == 0 && len == Array.length data)
    then data
    else Array.slice start (start + len) data

toBytes : ByteArray -> Bytes.Bytes
toBytes ba = toList ba
    |> List.map Bytes.Encode.unsignedInt8
    |> Bytes.Encode.sequence
    |> Bytes.Encode.encode


same : ByteArray -> ByteArray -> Bool
same (BA a1 i1 l1 as b1) (BA a2 i2 l2 as b2) =
  sameSection 0 (max l1 l2) b1 b2

sameSection : Int -> Int -> ByteArray -> ByteArray -> Bool
sameSection sectStart sectLen (BA a1 i1 l1) (BA a2 i2 l2) =
  let
    go j k n =
      if n > 0
        then if (Array.get j a1) == (Array.get k a2)
          then go (j + 1) (k + 1) (n - 1)
          else False
        else True
  in
    if i1 + sectStart + sectLen > l1
    || i2 + sectStart + sectLen > l2
      then False
      else go (i1 + sectStart) (i2 + sectStart) sectLen



asciiAt : ByteArray -> Int -> Char
asciiAt ba i = case get i ba of
  Just b ->
    if 0x20 <= b && b <= 0x7e
      then Char.fromCode b
      else '.'
  Nothing -> ' '

{-| Produces a nice looking hex dump of the `ByteArray`.
-}
hexDump : ByteArray -> String
hexDump ba =
  let
    hexAt i = case get i ba of
      Just b -> String.cons ' ' <| Util.hexByteString b
      Nothing -> "   "
    hexLine i =
      List.map hexAt (List.range i (i+7))
      ++ [" "]
      ++ List.map hexAt (List.range (i+8) (i+15))
    asciiLine i = String.fromList <| List.map (asciiAt ba) <| List.range i (i+15)

    line addr = String.concat
      <| [ Util.hexByteString (addr // 256)
         , Util.hexByteString (modBy 256 addr)
         , ": "
         ]
        ++ hexLine addr
        ++ ["  |", asciiLine addr, "|\n"]

    block addr =
      if addr == 0 || addr < length ba
        then line addr ++ block (addr + 16)
        else ""
  in
    block 0


exampleElmIntList : List Int
exampleElmIntList =
  {-    0 -} [ 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68 {- abcdefghi -}
  {-    8 -} , 0x69, 0x6a                                     {- jk        -}
  {-   10 -} ]

{-| Produces a hex dump that is suitable as elm code.
-}
elmIntList : ByteArray -> String
elmIntList ba =
  let
    addr i = String.padLeft 4 ' ' <| String.fromInt i
    hexAt i = case get i ba of
      Just b -> ", 0x" ++ Util.hexByteString b
      Nothing -> "      "
    hex i = String.concat <| List.map hexAt <| List.range i (i+7)
    hexFix i = if i == 0
      then "[" ++ String.dropLeft 1 (hex i)
      else hex i
    ascii i = String.fromList <| List.map (asciiAt ba) <| List.range i (i+7)

    line i = String.concat
      [ "  {- ", addr i, " -}  ", hexFix i, "  {- ", ascii i, " -}\n" ]

    block i =
      if i < length ba
        then line i ++ block (i + 8)
        else
          if i > 0
            then String.concat [ "  {- ", addr (length ba), " -}  ]\n" ]
            else "  [ ]\n"
  in
    block 0
