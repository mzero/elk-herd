module ByteArray.Builder exposing
  ( Builder
  , build
  , buildSequence

  , and
  , list
  , array
  , sequence

  , byte
  , bytes
  , byteList
  , empty
  , zeros
  , uint8
  , uint16be
  , uint32be
  , string0ascii
  , string0win1252

  , map
  , fit
  , sevenBit

  , makeExample
  )

{-| Builds up `ByteArray` objects from other types. This is a standard
functional combinator style serialization library.
-}

import Array exposing (Array)
import Bitwise

import ByteArray exposing (ByteArray)
import ByteArray.SevenBit
import ByteArray.String


{-| Hidden state of the building process
-}
type alias State = Array Int

state0 : State
state0 = Array.empty

{-| A thing to be built

There are several choices for the design of Builder:
  form: eager (wrapped state) or lazy (wrapped function of state to state)
  state: list of segments or accumulated result

After much benchmrking, the winner in speed was:
  lazy, accumulated state

So, a Builder is a function that takes the array of bytes so far built, and
appends some more bytes on the end.
-}
type Builder = B (State -> State)


{-| Given a Builder, run it and return the ByteArray it builds
-}
build : Builder -> ByteArray
build (B bfn) = bfn state0 |> ByteArray.fromArray


{-| run a sequence of Builder objects and return the ByteArray

This is identical to
    build << sequence
-}
buildSequence : List Builder -> ByteArray
buildSequence = build << sequence

{-| Make a builder from two others. Like `andThen` meant to be used like so:

  point : Point -> Builder
  point (Point x y) = uint16be x |> and (uint16be y)
-}
and : Builder -> Builder -> Builder
and (B f2) (B f1) = B (f1 >> f2)


list : (a -> Builder) -> List a -> Builder
list f items = B <| \s0 ->
  let
    step bs s =
      case bs of
          []        -> s
          a :: rest -> case f a of (B fb) -> step rest (fb s)
  in
    step items s0

array : (a -> Builder) -> Array a -> Builder
array f items = B <| \s0 ->
  let
    step i s =
      case Array.get i items of
          Nothing -> s
          Just a  -> case f a of (B fb) -> step (i + 1) (fb s)
  in
    step 0 s0


sequence : List Builder -> Builder
sequence = list identity




segment : Array Int -> Builder
segment a = B <| \s -> Array.append s a

empty : Builder
empty = B identity

byte : Int -> Builder
byte b = B <| \s -> Array.push (clamp 0 255 b) s

bytes : ByteArray -> Builder
bytes = ByteArray.toArray >> segment

byteList : List Int -> Builder
byteList = List.map (clamp 0 255) >> Array.fromList >> segment

zeros : Int -> Builder
zeros n = segment <| Array.repeat n 0

uint8 : Int -> Builder
uint8 = byte

uint16be : Int -> Builder
uint16be w =
  let
    w_ = clamp 0 65535 w
  in
    segment <| Array.fromList [ w_ // 256, modBy 256 w_ ]

uint32be : Int -> Builder
uint32be w =
  let
    w_ = clamp 0 0xffffffff w
  in
    segment <| Array.fromList
      [ Bitwise.and 0xff <| Bitwise.shiftRightZfBy 24 w_
      , Bitwise.and 0xff <| Bitwise.shiftRightZfBy 16 w_
      , Bitwise.and 0xff <| Bitwise.shiftRightZfBy 8  w_
      , Bitwise.and 0xff <| Bitwise.shiftRightZfBy 0  w_
      ]

string0ascii : String -> Builder
string0ascii s =
  bytes (ByteArray.String.encodeAscii s)
  |> and (byte 0)

string0win1252 : String -> Builder
string0win1252 s =
  bytes (ByteArray.String.encodeWin1252 s)
  |> and (byte 0)

{-| Mutate the serialized bytes of another builder.  This makes it easy to
encode a serialized sequence.
-}
map : (ByteArray -> ByteArray) -> Builder -> Builder
map fbs b = bytes <| fbs <| build b

fit : Int -> Int -> Builder -> Builder
fit width pad = map <| \b ->
  if ByteArray.length b < width
    then ByteArray.append b
          (ByteArray.fromArray <| Array.repeat (width - ByteArray.length b) pad)
    else ByteArray.section 0 width b

sevenBit : Builder -> Builder
sevenBit = map ByteArray.SevenBit.encode




makeExample : Int -> Builder
makeExample n =
  let
    smolList = [1, 2, 3, 4]
    smolArray = Array.fromList [5, 6, 7, 8]
    motto = "cats like elm best"
    ramp = ByteArray.fromList <| List.range 0 255
  in
  sequence
    [ byteList smolList
    , list uint16be smolList
    , array uint16be smolArray
    , string0ascii motto
    , uint32be 0x10203040
    , zeros n
    , bytes ramp
    ]
