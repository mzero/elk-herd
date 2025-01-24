module SysEx.Debug exposing
  ( Model
  , init
  , addItem

  , Msg
  , update
  , commands
  , view
  )

{-| The MIDI Debugger is a panel for sending, receiving and examining SysEx
messages, and how they are decoded. It isn't in the normal build of elk-herd.
See the `Build` module for the constant the enables it.

Warning: This is code is janky, as it is mostly stuff thrown together to help
debug things.
-}

import Array
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Regex

import Build
import ByteArray
import ByteArray.Compression as Compression
import Commands as C
import Elektron.Instrument as EI
import Html.Aria as Aria
import Missing.List as List
import Missing.Regex as Regex
import SysEx.Dump as Dump exposing (ElkDump)
import SysEx.Message as Message exposing (ElkMessage)
import SysEx.SysEx as SysEx exposing (SysEx, Direction(..))
import Util


type alias Model =
  { show : Bool
  , items : List (Direction, SysEx)
  , path : String
  , path2 : String
  , device: EI.Device
  , hexDump : String
  , dumpIndex : String
  }

init : Model
init = Model False [] "/" "/" EI.Unknown "" "0"

addItem : SysEx.Direction -> SysEx -> Model -> Model
addItem d s m =
  if Build.midiDebugger
    then { m | items = m.items ++ [(d, s)] }
    else m

decodeHex : Model -> Model
decodeHex model =
  let
    ba =
      String.split "\n" model.hexDump
      |> List.concatMap (
        Regex.replaceAtMost 1 (Regex.regex "^[0-9A-Fa-f]+([^|]*)\\|.*$")
         (\m -> case m.submatches of
            Just s :: _ -> s
            _ -> ""
         )
        >> Regex.find (Regex.regex "[0-9A-Fa-f]+")
        >> List.filterMap (.match >> ((++) "0x") >> String.toInt)
        )
      |> ByteArray.fromList
  in
    addItem Decoded (SysEx.sysExFromBytes ba) { model | hexDump = "" }

type Msg
 = HideDebug
 | ShowDebug
 | ClearDebug
 | SetPath String
 | SetPath2 String
 | SetHexDump String
 | SwapPaths
 | SetDumpIndex String
 | SendMessage ElkMessage
 | SendDirMsg (String -> ElkMessage)
 | SendDir2Msg (String -> String -> ElkMessage)
 | SetDevice EI.Device
 | SendDumpRequest Int
 | SendSysEx SysEx
 | ProbeMessage Int
 | DecodeHex


requests : List (String, ElkMessage)
requests =
  [ ("Device", Message.DeviceRequest)
  , ("Version", Message.VersionRequest)
  ]

possibleMsgs : List Int
possibleMsgs = [ 0x30 ]

pathMsgs : List (String, String -> ElkMessage)
pathMsgs =
  [ ("ls", Message.DirListRequest)
  , ("mkdir", Message.DirCreateRequest)
  , ("rmdir", Message.DirDeleteRequest)
  , ("rm", Message.FileDeleteRequest)
  ]

path2Msgs : List (String, String -> String -> ElkMessage)
path2Msgs =
  [ ("mv", Message.ItemRenameRequest)
  ]

probeMessage : Int -> Model -> List ElkMessage
probeMessage msg model =
  let
    sxArg i = Message.TestRequestArgs msg (ByteArray.fromArray <| Array.repeat i 0)
    sxStr = Message.TestRequestString msg model.path
  in
    sxStr :: [ ]
          -- List.map sxArg <| List.range 0 8


update : Msg -> Model -> (Model, List SysEx, List ElkMessage)
update msg model = case msg of
  ShowDebug -> ({ model | show = True }, [], [])
  HideDebug -> ({ model | show = False }, [], [])
  ClearDebug -> ({ model | items = [] }, [], [])
  SetPath path -> ({ model | path = path }, [], [])
  SetPath2 path -> ({ model | path2 = path }, [], [])
  SetHexDump text -> ({ model | hexDump = text }, [], [])
  SwapPaths -> ({model | path = model.path2, path2 = model.path }, [], [])
  SetDumpIndex text -> ({ model | dumpIndex = text }, [], [])
  SendDirMsg msgFn -> (model, [], [msgFn model.path])
  SendDir2Msg msgFn -> (model, [], [msgFn model.path model.path2])
  SendMessage dt -> (model, [], [dt])
  SetDevice dev -> ({ model | device = dev }, [], [])
  SendDumpRequest type_ ->
    let
      index = String.toInt model.dumpIndex |> Maybe.withDefault 0
      sysEx = Dump.Unknown type_ index ByteArray.empty
      dump = ElkDump model.device sysEx
    in (model, [SysEx.ElektronDump dump], [])
  SendSysEx sx -> (model, [sx], [])
  ProbeMessage msg_ -> (model, [], probeMessage msg_ model)
  DecodeHex -> (decodeHex model, [], [])


commands : C.Commands Msg
commands =
  if Build.midiDebugger
    then
      [ C.Group "SysEx Commands"
        [ C.Button "cmd-sysex-debug"  "Debug MIDI"  ShowDebug
        ]
      ]
    else
      []


viewItem : (Direction, SysEx) -> Html.Html msg
viewItem (d, sx) =
  let
    extraView =
      case sx of
        SysEx.ElektronDump ed ->
          case ed.message of
            Dump.Unknown _ _ ba ->
              [ Html.div [ Attr.class "hexdump" ]
                [ Html.text <| ByteArray.elmIntList <| Compression.compress ba ]
              ]
            _ -> [ ]
        _ -> [ ]
  in
    Html.div
      [ Attr.classList
        [ ("sysex-item", True)
        , ("sysex-sent", d == Sent)
        , ("sysex-received", d == Received)
        ]
      ]
      <| List.map (Html.map never) <| (SysEx.viewSysEx sx ++ extraView)

view : Model -> Html.Html Msg
view model = if not Build.midiDebugger then Html.div [] [] else
  let
    buttons =
      Html.div
        [ Attr.class "btn-group btn-group-sm input-group input-group-sm"
        , Aria.role "group"
        , Aria.label "Send Requests"
        ]
        <| Html.span [ Attr.class "input-group-addon" ] [ Html.text "Request:" ]
          :: ( requests |> List.map (\(name, dtMsg) ->
               Html.button
                [ Attr.class "btn btn-outline-secondary"
                , Attr.type_ "button"
                , Events.onClick <| SendMessage dtMsg
                ]
                [ Html.text name ]
                )
            )

    dirs =
      Html.div
        [ Attr.class "btn-group btn-group-sm input-group input-group-sm"
        , Aria.role "group"
        , Aria.label "Send Requests"
        ]
        <| Html.span [ Attr.class "input-group-addon" ] [ Html.text "Path:" ]
          :: Html.input [ Attr.value model.path, Attr.tabindex 100, Events.onInput SetPath ] [ ]
          :: ( pathMsgs |> List.map (\(label, msgFn) ->
              Html.button
                [ Attr.class "btn btn-outline-secondary"
                , Attr.type_ "button"
                , Events.onClick <| SendDirMsg msgFn
                ]
                [ Html.text label ]
              ))

    dirs2 =
      Html.div
        [ Attr.class "btn-group btn-group-sm input-group input-group-sm"
        , Aria.role "group"
        , Aria.label "Send Requests"
        ]
        <| Html.button
            [ Attr.class "but btn-outline-secondary"
            , Attr.type_ "button"
            , Events.onClick SwapPaths
            ]
            [ Html.text "⇅" ]
          :: Html.span [ Attr.class "input-group-addon" ] [ Html.text "to:" ]
          :: Html.input [ Attr.value model.path2, Attr.tabindex 101, Events.onInput SetPath2 ] [ ]
          :: ( path2Msgs |> List.map (\(label, msgFn) ->
              Html.button
                [ Attr.class "btn btn-outline-secondary"
                , Attr.type_ "button"
                , Events.onClick <| SendDir2Msg msgFn
                ]
                [ Html.text label ]
              ))

    probes =
      Html.div
        [ Attr.class "btn-group btn-group-sm input-group input-group-sm"
        , Aria.role "group"
        , Aria.label "Probe Requests"
        ]
        <| Html.span [ Attr.class "input-group-addon" ] [ Html.text "Probe:" ]
          :: ( possibleMsgs |> List.map (\msg ->
                 Html.button
                  [ Attr.class "btn btn-outline-secondary"
                  , Attr.type_ "button"
                  , Events.onClick <| ProbeMessage msg
                  ]
                  [ Html.text <| "0x" ++ Util.hexUint8 msg ]
                  )
            )

    decode =
      Html.div
        [ Attr.class "btn-group btn-group-sm input-group input-group-sm"
        , Aria.role "group"
        , Aria.label "Decode hex dump"
        ]
        [ Html.span [ Attr.class "input-group-addon" ] [ Html.text "hex:" ]
        , Html.textarea [ Attr.value model.hexDump, Attr.tabindex 200, Attr.rows 1, Events.onInput SetHexDump ] [ ]
        , Html.button
          [ Attr.class "btn btn-outline-secondary"
          , Attr.type_ "button"
          , Events.onClick DecodeHex
          ]
          [ Html.text "Decode" ]
        ]

    dumpBtn (label, req) =
      Html.button
          [ Attr.class "btn btn-outline-secondary"
          , Attr.type_ "button"
          , Events.onClick <| SendDumpRequest req
          ]
          [ Html.text label ]

    dumps =
      Html.div
        [ Attr.class "btn-group btn-group-sm input-group input-group-sm"
        , Aria.role "group"
        , Aria.label "Dump"
        ]
        (
          Html.span [ Attr.class "input-group-addon" ] [ Html.text "Dump:" ]
          :: devices
          :: Html.textarea [ Attr.value model.dumpIndex, Attr.tabindex 300, Attr.rows 1, Events.onInput SetDumpIndex ] [ ]
          :: List.map dumpBtn
            [ ("patKit", 0x60)
            , ("pat", 0x61)
            , ("kit", 0x62)
            , ("snd", 0x63)
            , ("projSet", 0x64)
            , ("all", 0x6f)
            ]
        )

    deviceItem d =
      Html.a
        [ Attr.class "dropdown-item"
        , Attr.href "#"
        , Events.onClick <| SetDevice d
        ]
        [ Html.text <| EI.productName d]

    devices =
      Html.div [ Attr.class "dropdown" ]
        [ Html.button
          [ Attr.class "btn btn-outline-secondary dropdown-toggle"
          , Attr.attribute "data-toggle" "dropdown"
          , Attr.type_ "button"
          , Attr.id "device-picker"
          , Aria.hasPopUp True
          , Aria.expanded False
          ]
          [ Html.text (EI.productName model.device) ]
        , Html.div [ Attr.class "dropdown-menu", Aria.labeledBy "device-picker" ]
          <| List.map deviceItem [ EI.Digitakt, EI.Digitakt2, EI.Unknown ]
        ]
  in
    Html.div [ Attr.id "midi-debug" ]
      [ Html.div
        [ Attr.classList
          [ ("modal", True), ("fade", True), ("show", model.show) ]
        , Aria.role "dialog"
        , Aria.labeledBy "modal-title-midi"
        , Aria.hidden (not model.show)
        ]
        [ Html.div [ Attr.class "modal-dialog modal-lg", Aria.role "document" ]
          [ Html.div [ Attr.class "modal-content" ]
            [ Html.div [ Attr.class "modal-header" ]
              [ Html.h5
                [ Attr.class "modal-title"
                , Attr.id "modal-title-midi"
                ]
                [ Html.text "MIDI Debugger" ]
              , Html.button
                [ Attr.class "btn btn-outline-secondary btn-sm mr-2"
                , Attr.type_ "button"
                , Events.onClick ClearDebug
                ]
                [ Html.text "Clear" ]
              , Html.button
                [ Aria.label "Close"
                , Attr.class "close"
                , Attr.type_ "button"
                , Events.onClick HideDebug
                ]
                [ Html.span [ Aria.hidden True ] [ Html.text "×" ]
                ]
              ]
            , Html.div [ Attr.class "modal-body" ]
              <| List.map viewItem model.items
            , Html.div [ Attr.class "modal-footer flex-wrap" ]
              [ Html.div []
                [ buttons
                ]
              , Html.div []
                [ dirs
                , dirs2
                ]
              , Html.div []
                [ probes
                ]
              , Html.div []
                [ dumps
                ]
              , Html.div [ Attr.class "mt-1" ]
                [ decode
                ]
              ]
            ]
          ]
        ]
      , Html.div [ Attr.classList
              [ ("modal-backdrop", model.show)
              , ("fade", True)
              , ("show", model.show) ]
            ]
            []
      ]
