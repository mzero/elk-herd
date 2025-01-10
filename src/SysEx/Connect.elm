module SysEx.Connect exposing
  ( Model
  , init
  , isReady
  , instrument
  , report

  , Msg
  , check
  , reset
  , update
  , view
  )

{-| This is a separate little MUV component for probing and identifying
an instrument on the connection the user has choosen.
-}

import Html
import Html.Attributes as Attr
import Html.Events as Events

import Elektron.Instrument as EI exposing (Instrument)
import SysEx.Client exposing (..)
import SysEx.Message as M exposing (ElkMessage)


type Result
  = Unknown
  | NoResponse
  | WaitingForVersion Instrument
  | Incompatible Instrument
  | Good Instrument
  | UnexpectedResponse
  | LoopedInterface

type alias Model =
  { result : Result
  , deviceModel : String
  , probing : Bool
  , probeId : Int
  }

init : Model
init = Model Unknown "" False 0

isReady : Model -> Bool
isReady model = case model.result of
  Good _ -> not model.probing
  _ -> False

instrument : Model -> Maybe Instrument
instrument model = case model.result of
  Unknown -> Nothing
  NoResponse -> Nothing
  WaitingForVersion inst -> Just inst
  Incompatible inst -> Just inst
  Good inst -> Just inst
  UnexpectedResponse -> Nothing
  LoopedInterface -> Nothing

{-| This is a more information heavy string used to report back to the stats
server. See the `Report` module.
-}
report : Model -> Maybe String
report model =
  if model.probing
    then Nothing
    else case model.result of
      Unknown -> Nothing
      NoResponse -> Nothing
      WaitingForVersion s -> Nothing
      Incompatible i -> Just <| "incompatible," ++ EI.report i
      Good i -> Just <| EI.report i
      UnexpectedResponse -> Nothing
      LoopedInterface -> Nothing

type Msg
  = Check
  | Response Int ElkMessage
  | Looped Bool


makeRequest : ElkMessage -> Model -> (Model, Requests Msg)
makeRequest reqMsg model =
  let
    id_ = model.probeId + 1
    model_ = { model | probing = True, probeId = id_ }
    request = RequestMessage reqMsg (Response id_)
  in
    (model_, [request])


check : Model -> (Model, Requests Msg)
check model = ( { model | probing = True }, [LoopbackProbe Looped] )

reset : Model -> Model
reset model = { model | result = Unknown, probing = False }


update : Msg -> Model -> (Model, Requests Msg)
update msg model =
  let
    outcome r = ( { model | result = r, probing = False }, [] )
    onProbeResponse id res =
      if id == model.probeId then res else (model, [])
  in
    case msg of
      Check -> check model

      Looped looped ->
        if looped
          then outcome LoopedInterface
          else makeRequest M.DeviceRequest model

      Response id (M.DeviceResponse productId msgs deviceName) ->
        onProbeResponse id <|
          let
            inst = Instrument productId deviceName msgs "????" "?.??"
            model_ = { model | result = WaitingForVersion inst }
          in
            makeRequest M.VersionRequest model_

      Response id (M.VersionResponse build version) -> onProbeResponse id <|
        case model.result of
          WaitingForVersion inst ->
            let
              inst_ = { inst | build = build, version = version }
            in
              if EI.hasDriveSamples inst_
                then outcome <| Good inst_
                else outcome <| Incompatible inst_
          _ ->
            outcome UnexpectedResponse

      Response id (M.TimeOut) -> onProbeResponse id <|
        outcome NoResponse

      Response _ _ ->
        outcome UnexpectedResponse



instrumentView : String -> Instrument -> Html.Html msg
instrumentView prefix inst =
  Html.span [ ]
    [ Html.text prefix
    , Html.b [ ] [ Html.text inst.deviceName ]
    , Html.text " "
    , Html.small [] [ Html.text inst.version ]
    ]

view : Model -> Html.Html Msg
view model =
  Html.div [ Attr.class "device-probe mt-2" ]
    [ Html.button
      [ Attr.class "btn btn-outline-primary btn-sm mr-2"
      , Events.onClick Check
      ]
      [ Html.text " Reprobe" ]
    , Html.span []
      [ if model.probing
        then Html.i [ ] [ Html.text "probing…" ]
        else case model.result of
          Unknown -> Html.text ""
          NoResponse -> Html.text "no compatible device found"
          WaitingForVersion inst -> instrumentView "found " inst
          Incompatible inst -> instrumentView "incompatible: " inst
          Good inst -> instrumentView "found " inst
          UnexpectedResponse -> Html.text """
Instrument is present, but doing something else? Close other apps using it.
"""
          LoopedInterface -> Html.text """
I'm hearing myself! This MIDI interface seems to loop back to itself.
"""
      ]
    ]

