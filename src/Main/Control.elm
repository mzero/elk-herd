module Main.Control exposing
  ( update
  , subscriptions
  )

{-| Top level update and subscriptions.

Handles all the switching between screens and pages.

Several component updaters also return requests for the SysEx module,
which are relayed here.
-}

import Browser.Navigation
import Task

import Alert
import Build
import Elektron.Instrument as EI
import Portage
import Project
import Report
import Samples
import SysEx
import SysEx.Client
import SysEx.Connect
import WebMidi

import Main.Base exposing (..)
import Missing.Maybe as Maybe


later : List Msg -> List (Cmd Msg)
later = List.map (\m -> Task.perform (always m) <| Task.succeed ())


prepForMain : Model -> (Model, Cmd Msg)
prepForMain model0 =
  let
    prepProjects inst model =
      case inst.projectSpec of
        Just spec -> { model | projectModel = Just <| Project.init inst spec }
        Nothing -> model

    prepSamples inst model =
      if EI.hasDriveSamples inst
        then
          let
            debugSamples = List.member "samples" model.appSettings.flags
            samplesModel = Samples.init inst debugSamples
          in
          onSamplesUpdate (Samples.kickOff samplesModel) model
        else (model, Cmd.none)
  in
    SysEx.Connect.instrument model0.connectModel
    |> Maybe.map (\inst -> model0 |> prepProjects inst |> prepSamples inst)
    |> Maybe.withDefault ( model0, Cmd.none )


makeSysExRequests :
  (msg -> Msg) -> SysEx.Client.Requests msg
  -> Model -> Cmd Msg -> (Model, Cmd Msg)
makeSysExRequests msgF reqs model cmd =
  let
    (sysExModel_, reqCmd) =
      SysEx.makeRequests msgF reqs model.sysExModel
  in
    ( { model | sysExModel = sysExModel_ }
    , Cmd.batch [ cmd, Cmd.map OnSysEx reqCmd ]
    )

onSamplesUpdate :
  (Samples.Model, Cmd Samples.Msg, SysEx.Client.Requests Samples.Msg)
  -> Model -> (Model, Cmd Msg)
onSamplesUpdate (samplesModel_, cmd, reqs) model =
  makeSysExRequests OnSamples reqs { model | samplesModel = Just samplesModel_ }
    <| Cmd.map OnSamples cmd


onProjectUpdate :
  (Project.Model, Cmd Project.Msg, SysEx.Client.Requests Project.Msg)
  -> Model -> (Model, Cmd Msg)
onProjectUpdate (projectModel_, cmd, reqs) model =
  makeSysExRequests OnProject reqs { model | projectModel = Just projectModel_ }
    <| Cmd.map OnProject cmd


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnSamples sMsg ->
      case model.samplesModel of
        Just sm ->
         onSamplesUpdate (Samples.update sMsg sm) model
        Nothing ->
          ( model, Cmd.none )

    OnProject pMsg ->
      case model.projectModel of
        Just pm ->
          onProjectUpdate
            (Project.update pMsg (Samples.drive model.samplesModel) pm)
            model
        Nothing ->
          ( model, Cmd.none)

    OnAlert alMsg ->
        ( { model | alertModel = Alert.update alMsg model.alertModel }
        , Cmd.none
        )

    OnWebMidi wmMsg ->
      let
        (webMidiModel_, wmCmd) = WebMidi.update wmMsg model.webMidiModel
        model_ = { model | webMidiModel = webMidiModel_ }
        cmd = Cmd.map OnWebMidi wmCmd
      in
        if WebMidi.openPortsDiffer model.webMidiModel webMidiModel_
          then if WebMidi.ready webMidiModel_
            then
              let
                (connectModel_, reqs) = SysEx.Connect.check model_.connectModel
              in
                makeSysExRequests OnSysExConnect reqs
                    { model_ | connectModel = connectModel_ } cmd
            else
              let
                connectModel_ = SysEx.Connect.reset model_.connectModel
              in
                ( { model_ | connectModel = connectModel_}, cmd )
          else
            (model_ , cmd)

    OnSysEx sxMsg ->
      let
        (sysExModel_, msgs, sxCmd) =
          SysEx.update sxMsg model.sysExModel
      in
        ( { model | sysExModel = sysExModel_ }
        , Cmd.batch <| Cmd.map OnSysEx sxCmd :: later msgs
        )

    OnSysExConnect cnMsg ->
      let
        (connectModel_, reqs) = SysEx.Connect.update cnMsg model.connectModel
        model_ = { model | connectModel = connectModel_ }
      in
        makeSysExRequests OnSysExConnect reqs model_ Cmd.none

    MidiError midiErr ->
      let
        message = WebMidi.midiErrorMessage midiErr
      in
        ( { model | screen = DeadScreen "MIDI Error" message }, Cmd.none )

    StoredAppInfo (ver, mOptIn) ->
      if ver < Build.minCompatibleAppVersion
        then
          let
            model_ = { model | screen = SettingsScreen }
          in
            ( model_, Portage.resetAppVersion Build.appVersion )
        else
          let
            optIn = mOptIn |> Maybe.withDefault Build.statsReporting
            appSettings = model.appSettings
            appSettings_ = { appSettings | reportingOptIn = optIn }
            screen_ =
              if mOptIn == Nothing || ver < Build.lastMajorVersion
                then SettingsScreen
                else MidiScreen
            model_ = { model
              | appSettings = appSettings_
              , screen = screen_
              }
            cmd = if ver < Build.appVersion
              then Portage.setAppVersion Build.appVersion
              else Cmd.none
          in
            ( model_, Cmd.batch [ Report.appVersion, cmd ] )

    ReportingOptIn optIn ->
      let
        appSettings = model.appSettings
        appSettings_ = { appSettings | reportingOptIn = optIn }
        model_ = { model | appSettings = appSettings_ }
      in
        ( model_, Cmd.none )

    AnotherInstanceStarted ->
      let
        message = """
This app was started in another browser window. That window now has control
of the instrument. This window is stopped.

You should close this one, or, if you prefer, reload it, and the app will run
here again.
"""
      in
        ( { model | screen = DeadScreen "Stopped" message }, Cmd.none )

    GoToScreen toScreen ->
      let
        fromScreen = model.screen
        model_1 = { model | screen = toScreen }
        cmd_1 = case fromScreen of
          SettingsScreen -> Portage.setOptIn model.appSettings.reportingOptIn
          MidiScreen -> Report.deviceConnect model.connectModel model.webMidiModel
          _ -> Cmd.none
      in
        case (fromScreen, toScreen) of
          (MidiScreen, MainScreen _) ->
            let
              (model_2, cmd_2) = prepForMain model_1
            in
              (model_2, Cmd.batch [cmd_1, cmd_2])
          _ ->
            (model_1, cmd_1)

    CloseMidi ->
      let
        message = """
The MIDI ports are now closed, and should be available to other applications.

On Windows systems, some MIDI interface drivers only allow one application at
a time to use a particular MIDI port. It is confusing, as these drivers still
allow the port to be selected in a second app, they just won't work.

Linux and OS X systems do not implement exclusive access, and there is no need
to close the MIDI ports in this app. However, you still should be sure to not
access the device at the same time from two applications.
"""
      in
        ( { model | screen = DeadScreen "Midi Closed" message }
        , Portage.closeMidi ()
        )

    ReloadApp -> (model, Browser.Navigation.reload)


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    pageSubs =
      case model.screen of
        MainScreen SamplesPage ->
          Maybe.unwrap Sub.none
          (Sub.map OnSamples << Samples.subscriptions)
          model.samplesModel

        MainScreen ProjectPage ->
          Maybe.unwrap Sub.none
            (Sub.map OnProject << Project.subscriptions)
            model.projectModel

        _ -> Sub.none
  in
    Sub.batch
      [ pageSubs
      , Sub.map OnAlert <| Alert.subscriptions model.alertModel
      , Sub.map OnWebMidi <| WebMidi.subscriptions model.webMidiModel
      , Sub.map OnSysEx <| SysEx.subscriptions model.sysExModel
      , Portage.storedAppVersionAndOptIn StoredAppInfo
      , Portage.onlyInstanceMinder (always AnotherInstanceStarted)
      , Portage.midiError MidiError
      ]
