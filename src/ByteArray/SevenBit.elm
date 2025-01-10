module ByteArray.SevenBit exposing
  ( encode
  , decode
  )

{-| Implements the seven bit clean encoding used by Elektron in SysEx messages.

Data in MIDI SysEx messages must be "7-bit clean", that is always have the
high bit set to zero. Elektron encodes its messages over SysEx by this scheme:

Each group of 7 bytes is encoded with a starting byte that has the high bits of
all 7 bytes, followed by the bytes with the high bit set to zero:

    0abcdefg 0aaaaaaa 0bbbbbbb 0ccccccc 0ddddddd 0eeeeeee 0fffffff 0ggggggg

If the last group has fewer than 7 bytes, they are just omitted. For example:

    0ab00000 0aaaaaaa 0bbbbbbb
-}

import Array exposing (Array)
import Bitwise
import ByteArray exposing (ByteArray)


-- encode a byte array in 7-bit encoding
encode : ByteArray -> ByteArray
encode = ByteArray.fromArray << encodeArray << ByteArray.toArray

-- decode 7-bit encoded data
decode : ByteArray -> ByteArray
decode = ByteArray.fromArray << decodeArray << ByteArray.toArray


encodeArray : Array Int -> Array Int
encodeArray data8 =
  let
    len8 = Array.length data8
    len7 = len8 + (len8 + 6) // 7

    -- these functions are written for efficiency, not clarity

    lobits i =
      case Array.get i data8 of
        Nothing -> 0
        Just v  -> Bitwise.and v 0x7f

    hibits i j a =
      if i >= j
        then a
        else
          case Array.get i data8 of
            Nothing -> hibits (i + 1) j (a + a)
            Just v  -> hibits (i + 1) j (a + a + v // 128)

    byte i =
      let
        group = i // 8 * 7
        index = modBy 8 i
      in
        if index == 0
          then hibits group (group + 7) 0
          else lobits (group + index - 1)
  in
    Array.initialize len7 byte


decodeArray : Array Int -> Array Int
decodeArray ai7 =
  let
    inLen = Array.length ai7
    outLen = (inLen // 8 * 7) + (clamp 0 7 (modBy 8 inLen - 1))
    at i = Maybe.withDefault 0 (Array.get i ai7)

    i8 i =
      let
        j = i // 7 * 8
        k = modBy 7 i + 1
        b = Bitwise.shiftLeftBy k
        hi = Bitwise.and 128 (b <| at j)
        lo = at (j + k)
      in
        Bitwise.or hi lo
  in
    Array.initialize outLen i8

