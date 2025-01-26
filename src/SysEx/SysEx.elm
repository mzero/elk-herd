module SysEx.SysEx exposing
  ( SysEx(..)

  , SysExBytes(..)
  , sysExFromBytes
  , sysExFromMidi
  , sysExToBytes
  , asRawBytes

  , viewSysEx

  , divideIntoSysExes
  , readNextSysEx
  , asByteArray
  )

{-| Parsing and building SysEx messages
-}

import Html
import Html.Attributes as Attr

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import ByteArray.Parser as Parser
import ByteArray.SevenBit
import Elektron.Instrument exposing (Device(..))
import Missing.Maybe as Maybe
import SysEx.Dump exposing (..)
import SysEx.Internal exposing (..)
import SysEx.Message exposing (..)


type SysEx
  = Undecoded String ByteArray
  | ElektronAPI { msgId: Int, respId: Maybe Int, msg: ElkMessage }
  | ElektronDump ElkDump

parseSysEx : Parser.Parser SysEx
parseSysEx =
  Parser.expectByte 0xF0
  |> Parser.andThen (\_ -> Parser.expectList [ 0x00, 0x20, 0x3C ])
  |> Parser.andThen (\_ -> Parser.byte)
  |> Parser.andThen (\s ->
      case s of
        0x10 -> parseAPI
        0x0A -> Parser.map ElektronDump <| parseDump Digitakt
        0x14 -> Parser.map ElektronDump <| parseDump Digitakt2
        _ -> Parser.fail ("unknown sysex type " ++ String.fromInt s)
    )

parseAPI : Parser.Parser SysEx
parseAPI =
  Parser.expectByte 0x00
  |> Parser.andThen (\_ -> Parser.upToByte 0xF7)
  |> Parser.andThen
      (\buf ->
        ByteArray.SevenBit.decode buf
        |> Parser.parse (
            let
              buildED msgId respId msg = ElektronAPI
                { msgId = msgId
                , respId = if respId > 0 then Just respId else Nothing
                , msg = msg
                }
            in
              Parser.map3 buildED Parser.uint16be Parser.uint16be parseMessage
          )
        |> Parser.result
      )

{-| While everything could be handed over to the JavaScript layer as just bytes,
for efficency, we pass the bytes of the API directly, and let the JavaScript
code do the 7bit encodeing and put the header & trailer bytes on.

FIXME: This optomization is probably no longer called for. Since we use the
elm code to 7bit encode dumps, which are much larger than most API calls,
seems like the Elm code is fast enough. This dual pathway to the JavaScript
should probably be eliminated, and just always send raw bytes... and this type
could be eliminated.
-}
type SysExBytes
  = RawMidiBytes ByteArray        -- as transfered on the wire
  | ElektronApiBytes ByteArray    -- decoded from the sysex payload

{-| As per the comment in `SysExBytes`, this code is mirror of what the
JavaScript code is doing.
-}
asRawBytes : SysExBytes -> ByteArray
asRawBytes sxbs = case sxbs of
  RawMidiBytes bs -> bs
  ElektronApiBytes bs ->
    Builder.buildSequence
      [ Builder.byte      0xF0                  -- sysex start
      , Builder.byteList  [ 0x00, 0x20, 0x3C ]  -- Elektron
      , Builder.byteList  [ 0x10, 0x00 ]        -- Elektron API
      , Builder.sevenBit  (Builder.bytes bs)
      , Builder.byte      0xF7
      ]

{-| When we know these bytes are supposed to be a SysEx...
-}
sysExFromBytes : ByteArray -> SysEx
sysExFromBytes ba = case Parser.parse parseSysEx ba of
  Ok sx -> sx
  Err msg -> Undecoded msg ba

{-| When we receive a bytes from WebMidi, they could be anything.
-}
sysExFromMidi : List Int -> Maybe SysEx
sysExFromMidi ai =
  case ai of
    0xF0 :: _ -> Just <| sysExFromBytes <| ByteArray.fromList ai
    _ -> Nothing

sysExToBytes : SysEx -> SysExBytes
sysExToBytes sx = case sx of
  Undecoded _ ba -> RawMidiBytes ba
  ElektronAPI { msgId, respId, msg } -> ElektronApiBytes <|
    Builder.buildSequence
      [ Builder.uint16be msgId
      , Builder.uint16be <| Maybe.withDefault 0 respId
      , messageBuilder msg
      ]
  ElektronDump ed ->
    let
      dumpCode =
        case ed.device of
          Digitakt -> Just 0x0A
          Digitakt2 -> Just 0x14
          _ -> Nothing
    in
    RawMidiBytes
    <| case dumpCode of
        Nothing -> ByteArray.empty
        Just v ->
          Builder.buildSequence
            [ Builder.byte      0xF0                  -- sysex start
            , Builder.byteList  [ 0x00, 0x20, 0x3C ]  -- Elektron
            , Builder.byte      v
            , dumpBuilder ed
            , Builder.byte      0xF7
            ]

viewSysEx : SysEx -> List (Html.Html Never)
viewSysEx sx = case sx of
  Undecoded msg ba ->
    [ Html.text msg
    , Html.div [ Attr.class "hexdump" ] [ Html.text <| ByteArray.hexDump ba ] ]
  ElektronAPI { msgId, respId, msg } ->
    [ fieldView "sysex" "Elek."
    , fieldView "id" <| String.fromInt msgId
    ]
    ++ Maybe.toList (Maybe.map (fieldView "resp" << String.fromInt) respId)
    ++ viewMessage msg
  ElektronDump ed ->
    [ fieldView "dump" <| Elektron.Instrument.productName ed.device ]
    ++ viewDump ed



divideIntoSysExes : ByteArray -> Result String (List SysEx)
divideIntoSysExes bytes =
  case Parser.parse (Parser.repeatToEnd parseSysEx) bytes of
    Ok v -> Ok v
    Err s -> Err ("Does not parse as SysEx messages (" ++ s ++ ")")

readNextSysEx : ByteArray -> Result String (SysEx, ByteArray)
readNextSysEx bytes =
  case Parser.parse (Parser.map2 Tuple.pair parseSysEx Parser.rest) bytes of
    Ok v -> Ok v
    Err s -> Err ("Does not parse as SysEx messages (" ++ s ++ ")")


asByteArray : SysEx -> ByteArray
asByteArray = asRawBytes << sysExToBytes

