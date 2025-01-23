module Main.View exposing
  ( titlebar
  , view
  )

{-| Top level view.

Shows the current screen.

If on the main screen, builds the view with the commands on the left,
and the current page filling the primary area.
-}

import Html exposing (div, text)
import Html.Attributes as Attr
import Html.Events as Events
import Markdown

import Alert
import Build
import Commands as C
import Elektron.Instrument exposing (Instrument)
import Html.Aria as Aria
import Project
import Samples
import SysEx
import SysEx.Connect

import Main.Base exposing (..)
import Main.SettingsView
import Main.MidiSetupView



appCommands : C.Commands Msg
appCommands =
  [ C.Group "Help"
    [ C.Anchor "cmd-help" "Help" "help.html"
    ]
  , C.Group "App Commands"
    [ C.Button "cmd-app-settings"    "App Settings"  (GoToScreen SettingsScreen)
    , C.Button "cmd-midi-settings"   "MIDI Settings" (GoToScreen MidiScreen)
    , C.Button "cmd-close-midi"      "Close MIDI"    (CloseMidi)
    ]
  ]

instrumentView : Maybe Instrument -> Html.Html msg
instrumentView mInst = case mInst of
  Just inst ->
    div [ Attr.id "titlebar-instrument" ]
      [ Html.text inst.deviceName
      , Html.span [ Attr.class "small text-muted" ]
        [ Html.text " — "
        , Html.text inst.version
        ]
      ]
  Nothing -> div [] []


pageDescriptors : List (Page, String, String)
pageDescriptors =
  [ (SamplesPage, "Samples", "page-samples")
  , (ProjectPage, "Project", "page-project")
  ]


mainView : Page -> Model -> Html.Html Msg
mainView page model =
  let
    hasProjectPage =
      SysEx.Connect.instrument model.connectModel
        |> Maybe.map Elektron.Instrument.hasProjects
        |> Maybe.withDefault False

    pageContent =
      case page of
        SamplesPage ->
          case model.samplesModel of
            Just sm -> Html.map OnSamples <| Samples.view sm
            Nothing -> Html.div [] []
        ProjectPage ->
          case model.projectModel of
            Just pm -> Html.map OnProject <| Project.view pm
            Nothing -> Html.div [] []

    pageCommands =
      case page of
        SamplesPage -> C.map OnSamples Samples.commands
        ProjectPage ->
          C.map OnProject
          <| Maybe.withDefault []
          <| Maybe.map Project.commands
          <| model.projectModel

    row left right =
      div [ Attr.class "row" ]
        [ div [ Attr.id "page-left", Attr.class "col-2 col-lg-3" ] left
        , div [ Attr.id "page-right", Attr.class "col" ] right
        ]

    pageNavItem (p, s, id) =
      Html.button
        [ Attr.class "btn btn-outline-dark"
        , Attr.classList [ ("active", p == page) ]
        , Attr.id id
        , Events.onClick (GoToScreen (MainScreen p))
        ]
        [ Html.span [ Attr.class "page-icon d-inline-block d-lg-none" ] [ ]
        , Html.span [ Attr.class "page-text d-none d-lg-inline" ] [ Html.text s ]
        ]

    pagePanel =
      if hasProjectPage
        then
          div [ Attr.id "page-panel" ]
            [ div [ Attr.class "btn-group w-75" ]
                (List.map pageNavItem pageDescriptors)
            ]
        else
          div [] []
  in
    div [ Attr.id "main-view", Attr.class "container" ]
      [ row
          [ pagePanel
          , div [ Attr.id "cmd-panel" ]
              <| List.map C.view
                [ pageCommands
                , appCommands
                , C.map OnSysEx SysEx.commands
                ]
          ]
          [ Html.map OnAlert <| Alert.view model.alertModel
          , pageContent
          ]
      , Html.map OnSysEx <| SysEx.view model.sysExModel
      ]

deadView : String -> String -> Html.Html Msg
deadView title msg =
  Html.div []
    [ Html.div
      [ Attr.class "modal fade show"
      , Aria.role "dialog"
      , Aria.hidden False
      , Attr.tabindex -1
      ]
      [ Html.div [ Attr.class "modal-dialog modal-lg", Aria.role "document" ]
        [ Html.div [ Attr.class "modal-content" ]
          [ Html.div [ Attr.class "modal-header bg-warning" ]
            [ Html.h5 [ Attr.class "modal-title" ] [ Html.text title ] ]
          , Html.div [ Attr.class "modal-body" ]
            [ Markdown.toHtml [ Attr.class "markdown" ] msg ]
          , Html.div [ Attr.class "modal-footer" ]
            [ Html.button
              [ Attr.class "btn btn-secondary"
              , Attr.type_ "button"
              , Events.onClick ReloadApp
              ]
              [ Html.text "Reload" ]
            ]
          ]
        ]
      ]
    , Html.div [ Attr.class "modal-backdrop fade show" ] []
    ]


titlebarMiddle : Model -> Html.Html Msg
titlebarMiddle model =
  case model.screen of
        MainScreen _ -> instrumentView <| SysEx.Connect.instrument model.connectModel
        _ -> div [] []

titlebar : Html.Html msg -> Html.Html msg
titlebar middle =
  Html.nav [ Attr.id "titlebar", Attr.class "bg-dark" ]
    [
      div [ Attr.class "left" ]
        [ div [ Attr.id "titlebar-app" ]
          [ Html.span [ Attr.class "text-warning"] [ text "crunch/" ]
          , text "elk-herd"
          ]
        , div [ Attr.id "titlebar-elk"] []
        ]
    , div [ Attr.class "middle" ] [ middle ]
    , div [ Attr.class "right"]
      [ div [ Attr.id "titlebar-info" ]
        [ Html.p [ ] [ text ("version " ++ Build.appVersionDisplay) ]
        , Html.p [ ]
          [ text "by "
          , Html.a [ Attr.href "https://www.elektronauts.com/u/mzero/"]
            [ text "mzero"]
          ]
        ]
      , div [ Attr.id "titlebar-beer" ]
        [ Html.a
          [ Attr.href "https://www.paypal.me/MtnViewMark"
          , Attr.title "By me a beer!"
          , Attr.attribute "data-toggle" "tooltip"
          , Attr.attribute "data-placement" "left"
          ]
          [ text "$" ]
        ]
      ]
    ]

viewContent : Model -> Html.Html Msg
viewContent model = case model.screen of
  SplashScreen -> Html.div [] []
  SettingsScreen -> Main.SettingsView.view model
  MidiScreen -> Main.MidiSetupView.view model
  MainScreen page -> mainView page model
  DeadScreen title msg -> deadView title msg

view : Model -> Html.Html Msg
view model =
  div []
    [ titlebar (titlebarMiddle model)
    , viewContent model
    ]
