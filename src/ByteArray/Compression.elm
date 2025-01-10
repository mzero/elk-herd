module ByteArray.Compression exposing
  ( compress
  , decompress
  )

{-| A simple wrapper around folkertdev/elm-flate's `Flate` module to make
it work with `ByteArray`.
-}

import ByteArray exposing (ByteArray)
import Flate


compress : ByteArray -> ByteArray
compress =
  ByteArray.toBytes
  >> Flate.deflate
  >> ByteArray.fromBytes

decompress : ByteArray -> Maybe ByteArray
decompress =
  ByteArray.toBytes
  >> Flate.inflate
  >> Maybe.map ByteArray.fromBytes

