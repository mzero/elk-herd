module SysEx.Dump exposing
  ( ElkDump, DumpMessage(..)

  , dumpBuilder
  , parseDump
  , viewDump
  )

{-| Elektron Dump messages.

These are actually Digitakt specific, hence all the DT prefixes. The code is
here as it pertains specifically to the SysEx message exchange, where as the
code in Digitakt.* is all about the content of these messages.

Note: ELektron's Sysex dump format doesn't use the Elektron API. It is a
separate mechanism.

The payloads of these messages are the storage formats of the parts of a
Digitakt project. Those structures can be found in `Elektron.Digitakt.Dump`.
In there are the `structXxxx` objects, which define encoders and decoders
of these structures.
-}

import Bitwise
import Html

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import ByteArray.Parser as Parser
import ByteArray.SevenBit
import Elektron.Digitakt.Dump as DT
import Elektron.Instrument exposing (Device(..))
import Elektron.Struct.Version as Version
import SysEx.Internal exposing (..)
import List exposing (sum)


type alias ElkDump = { device : Device, message : DumpMessage }
{-| Dump messages come in request and response pairs. These all refer to the
current loaded project. The reqeusts with `Int` specify the slot number to
dump.

`DTPatternKitResponse` response is literally the concatenation of a pattern and
the corresponding kit.

`DTWholeProjectRequest` results in a series of `DTPatternKitResponse` messages
than zero or more `DTSoundResponse` messages, and finally a
`DTProjectSettingsResponse` message. There is no other indication that it is
"done"... so we rely on that order of responses to show progress to the user.
-}
type DumpMessage
  = Unknown Int Int ByteArray
  | BadParse Int Int String ByteArray

  | DTPatternKitRequest Int
  | DTPatternKitResponse Int DT.PatternKit
  | DTPatternRequest Int
  | DTPatternResponse Int DT.Pattern
  | DTKitRequest Int
  | DTKitResponse Int DT.Kit
  | DTSoundRequest Int
  | DTSoundResponse Int DT.Sound
  | DTProjectSettingsRequest
  | DTProjectSettingsResponse DT.ProjectSettings
  | DTWholeProjectRequest


{- These messages use a very simple 14-bit checksum scheme. The checksum is
outside the seven-bit encoded area, hence only 14 bits.
-}

parseUint14be : Parser.Parser Int
parseUint14be =
  Parser.map2 (\hi lo -> hi * 128 + lo)
    Parser.byte
    Parser.byte

buildUint14be : Int -> Builder.Builder
buildUint14be v =
  Builder.byteList
    [ (Bitwise.and 0x7f (v // 128))
    , (Bitwise.and 0x7f v)
    ]

uint14equal : Int -> Int -> Bool
uint14equal a b = (Bitwise.and 0x3fff a) == (Bitwise.and 0x3fff b)


sumBytes : ByteArray -> Int
sumBytes ba =
  let
    sum i acc =
      case ByteArray.get i ba of
        Just v -> sum (i + 1) (acc + v)
        Nothing -> acc
  in
    sum 0 0


parsePayload : ByteArray -> Parser.Parser ByteArray
parsePayload payload =
  let
    n = ByteArray.length payload - 4
    p =
      Parser.bytes n      |> Parser.andThen (\encodedContent ->
      parseUint14be       |> Parser.andThen (\checksum ->
      parseUint14be       |> Parser.andThen (\count ->
        if not <| uint14equal count (n + 5)
          then Parser.fail "count was wrong"
          else
            if not <| uint14equal checksum (sumBytes encodedContent)
              then Parser.fail "checksum was wrong"
              else Parser.succeed <| ByteArray.SevenBit.decode encodedContent
      )))
  in
    Parser.parse p payload |> Parser.result

parseStruct : Device -> Int -> Int -> ByteArray -> Parser.Parser ElkDump
parseStruct device type_ index content =
  let
    parseResponse fn p = Parser.map fn p
      -- This code no longer checks for atEnd because the instrument can send
      -- more data than the structure. This happens if the size of the structure
      -- shrank from a previous version, as the SysEx sending code in the
      -- instrument always sends from a buffer that is the union of all prior
      -- versions.

    parseRequest v = Parser.map (always v) Parser.atEnd

    spec = Version.MatchDevice device

    parser = case type_ of
        0x50 -> parseResponse (DTPatternKitResponse index)  (DT.structPatternKit.decoder spec)
        0x60 -> parseRequest  (DTPatternKitRequest index)
        0x51 -> parseResponse (DTPatternResponse index)     (DT.structPattern.decoder spec)
        0x61 -> parseRequest  (DTPatternRequest index)
        0x52 -> parseResponse (DTKitResponse index)         (DT.structKit.decoder spec)
        0x62 -> parseRequest  (DTKitRequest index)
        0x53 -> parseResponse (DTSoundResponse index)       (DT.structSound.decoder spec)
        0x63 -> parseRequest  (DTSoundRequest index)
        0x54 -> parseResponse (DTProjectSettingsResponse)   (DT.structProjectSettings.decoder spec)
        0x64 -> parseRequest  (DTProjectSettingsRequest)
        0x6f -> parseRequest  (DTWholeProjectRequest)
        _ -> Parser.succeed (Unknown type_ index content)
  in
    Parser.map (ElkDump device)
    <| case Parser.parse parser content of
        Ok v -> Parser.succeed v
        Err msg -> Parser.succeed (BadParse type_ index msg content)

parseDump : Device -> Parser.Parser ElkDump
parseDump device =
  Parser.expectByte 0x00          |> Parser.andThen (\deviceId  ->
  Parser.byte                     |> Parser.andThen (\type_ ->
  Parser.expectList [0x01, 0x01]  |> Parser.andThen (\version ->
  Parser.byte                     |> Parser.andThen (\index ->
  Parser.upToByte 0xF7            |> Parser.andThen (\payload ->
  parsePayload payload            |> Parser.andThen (\content ->
  parseStruct device type_ index content
  ))))))

dumpBuilder : ElkDump -> Builder.Builder
dumpBuilder ed =
  let
    msgBuilder type_ index contentBuilder =
      let
        content = Builder.build contentBuilder
        payload = ByteArray.SevenBit.encode content
        checksum = sumBytes payload
        count = ByteArray.length payload + 5
      in
        Builder.sequence
          [ Builder.byte 0x00
          , Builder.byte type_
          , Builder.byte 0x01
          , Builder.byte 0x01
          , Builder.byte index
          , Builder.bytes payload
          , buildUint14be checksum
          , buildUint14be count
          ]
  in
    case ed.message of
      Unknown type_ index content ->
        msgBuilder type_ index (Builder.bytes content)

      BadParse type_ index msg content ->
        msgBuilder type_ index (Builder.bytes content)

      DTPatternKitRequest index ->
        msgBuilder 0x60 index Builder.empty

      DTPatternKitResponse index pattern ->
        msgBuilder 0x50 index (DT.structPatternKit.encoder pattern)

      DTPatternRequest index ->
        msgBuilder 0x61 index Builder.empty

      DTPatternResponse index sequence ->
        msgBuilder 0x51 index (DT.structPattern.encoder sequence)

      DTKitRequest index ->
        msgBuilder 0x62 index Builder.empty

      DTKitResponse index kit ->
        msgBuilder 0x52 index (DT.structKit.encoder kit)

      DTSoundRequest index ->
        msgBuilder 0x63 index Builder.empty

      DTSoundResponse index sound ->
        msgBuilder 0x53 index (DT.structSound.encoder sound)

      DTProjectSettingsRequest ->
        msgBuilder 0x64 0 Builder.empty

      DTProjectSettingsResponse project ->
        msgBuilder 0x54 0 (DT.structProjectSettings.encoder project)

      DTWholeProjectRequest ->
        msgBuilder 0x6f 0 Builder.empty


viewDump : ElkDump -> List (Html.Html Never)
viewDump ed =
  let
    buildView type_ name index html =
      [ fieldView "sysex" name
      , fieldView "type" <| String.fromInt type_
      , fieldView "index" <| String.fromInt index
      ]
      ++ html
  in
    case ed.message of
      Unknown type_ index content ->
        buildView type_ "Unknown" index [hexdumpFieldView "content" content]

      BadParse type_ index msg content ->
        buildView type_ ("Bad Parser: " ++ msg) index [hexdumpFieldView "content" content]

      DTPatternKitRequest index ->
        buildView 0x60 "Digitakt Pattern Kit Request" index []

      DTPatternKitResponse index patternKit ->
        buildView 0x50 "Digitakt Pattern Kit Response" index
          (DT.structPatternKit.view "patternKit" patternKit)

      DTPatternRequest index ->
        buildView 0x61 "Digitakt Pattern Request" index []

      DTPatternResponse index pattern ->
        buildView 0x51 "Digitakt Pattern Response" index
          (DT.structPattern.view "pattern" pattern)

      DTKitRequest index ->
        buildView 0x62 "Digitakt Kit Request" index []

      DTKitResponse index kit ->
        buildView 0x52 "Digitakt Kit Response" index
          (DT.structKit.view "kit" kit)

      DTSoundRequest index ->
        buildView 0x63 "Digitakt Sound Request" index []

      DTSoundResponse index sound ->
        buildView 0x53 "Digitakt Sound Response" index
          (DT.structSound.view "sound" sound)

      DTProjectSettingsRequest ->
        buildView 0x64 "Digitakt Project Settings Request" 0 []

      DTProjectSettingsResponse projectSettings ->
        buildView 0x54 "Digitakt Project Settings Response" 0
          (DT.structProjectSettings.view "projectSettings" projectSettings)

      DTWholeProjectRequest ->
        buildView 0x6f "Digitakt Whole Project Request" 0 []
