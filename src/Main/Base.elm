module Main.Base exposing
  ( AppSettings

  , Page(..)
  , Screen(..)
  , FileDisposition(..)

  , Model
  , init

  , alert

  , Msg(..)
  )

{-| Top level `Model` and `Msg` of the whole app.
-}

import Alert
import Project
import Samples
import SysEx
import SysEx.Connect
import WebMidi



type alias AppSettings =
  { reportingOptIn : Bool
  }

initAppSettings : AppSettings
initAppSettings = { reportingOptIn = True }

{-| Which view of the instrument the user is active.
Somewhat misnamed: Should be "Panel" or "Tab" or "Section" or ???
-}
type Page
  = SamplesPage | ProjectPage

{-| Where in the app flow the user is.
Misnamed: This should be called "Page"
-}
type Screen
  = SplashScreen
  | SettingsScreen
  | MidiScreen
  | MainScreen Page
  | DeadScreen String String

type FileDisposition = FileUnexpected | FileLoad | FileImport

type alias Model =
  { appSettings : AppSettings
  , screen : Screen
  , samplesModel : Samples.Model
  , projectModel : Maybe Project.Model

  , alertModel : Alert.Model
  , webMidiModel : WebMidi.Model
  , sysExModel : SysEx.Model Msg
  , connectModel : SysEx.Connect.Model

  , fileDisposition : FileDisposition
  }

alert : Alert.Level -> String -> String -> Model -> Model
alert lvl s1 s2 model = { model | alertModel = Alert.alert lvl s1 s2 }

init : String -> Model
init flags =
  let
    debugFlags = String.split "," flags
  in
    { appSettings = initAppSettings
    , screen = SplashScreen
    , samplesModel = Samples.init (List.member "samples" debugFlags)
    , projectModel = Nothing
    , alertModel = Alert.noAlert
    , webMidiModel = WebMidi.init
    , sysExModel = SysEx.init (List.member "slow" debugFlags)
    , connectModel = SysEx.Connect.init
    , fileDisposition = FileUnexpected
  }

type Msg
  = OnSamples Samples.Msg
  | OnProject Project.Msg
  | OnAlert Alert.Msg
  | OnWebMidi WebMidi.Msg
  | OnSysEx SysEx.Msg
  | OnSysExConnect SysEx.Connect.Msg
  | StoredAppInfo (Int, Maybe Bool)
  | ReportingOptIn Bool
  | AnotherInstanceStarted
  | GoToScreen Screen
  | CloseMidi
  | MidiError (String, String)
  | ReloadApp

