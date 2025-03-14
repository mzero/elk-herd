module Samples exposing
  ( Model
  , init
  , drive

  , Msg
  , update
  , kickOff
  , subscriptions
  , commands
  , view
  )

import Html

import Commands as C
import Elektron.Drive as Drive exposing (Drive)
import Elektron.Instrument as EI
import Missing.Maybe as Maybe
import Samples.Base
import Samples.Update
import Samples.View
import SysEx.Client


type alias Model = Samples.Base.Model

init : EI.Instrument -> Bool -> Model
init = Samples.Base.init

drive : Maybe Model -> Drive
drive = Maybe.unwrap Drive.emptyDrive .drive


type alias Msg = Samples.Base.Msg


update : Msg -> Model -> (Model, Cmd Msg, SysEx.Client.Requests Msg)
update = Samples.Update.update

kickOff : Model -> (Model, Cmd Msg, SysEx.Client.Requests Msg)
kickOff = Samples.Update.update Samples.Base.KickOff

subscriptions : Model -> Sub Msg
subscriptions = Samples.Update.subscriptions

commands : C.Commands Msg
commands = Samples.View.commands

view : Model -> Html.Html Msg
view = Samples.View.view

