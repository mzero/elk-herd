module Project exposing
  ( Model
  , init

  , Msg
  , update
  , subscriptions
  , commands
  , view
  )

import Html

import Commands as C
import Elektron.Drive exposing (Drive)
import Elektron.Instrument exposing (Instrument, ProjectSpec)
import Project.Base
import Project.Update
import Project.View
import SysEx.Client


type alias Model = Project.Base.Model

init : Instrument -> ProjectSpec -> Model
init = Project.Base.init


type alias Msg = Project.Base.Msg


update : Msg -> Drive -> Model -> (Model, Cmd Msg, SysEx.Client.Requests Msg)
update = Project.Update.update

subscriptions : Model -> Sub Msg
subscriptions = Project.Update.subscriptions

commands : Model -> C.Commands Msg
commands = Project.View.commands

view : Model -> Html.Html Msg
view = Project.View.view

