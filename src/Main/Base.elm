module Main.Base exposing
  ( Page(..)
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
import AppFlags
import Portage
import Project
import Samples
import SysEx
import SysEx.Connect
import WebMidi



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
  { reportingOptIn : Bool
  , flags : AppFlags.AppFlags

  , screen : Screen
  , samplesModel : Maybe Samples.Model
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
  -- Note: The flags argument comes from the html code that starts the app.
  -- It is not the flags that user can set on the Settings page. That is stored
  -- in local storage.
  { reportingOptIn = True
  , flags = AppFlags.init ""
  , screen = SplashScreen
  , samplesModel = Nothing
  , projectModel = Nothing
  , alertModel = Alert.noAlert
  , webMidiModel = WebMidi.init
  , sysExModel = SysEx.init
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
  | StoredAppInfo Portage.AppInfo
  | ReportingOptIn Bool
  | SetAppFlags String
  | AnotherInstanceStarted
  | GoToScreen Screen
  | CloseMidi
  | MidiError (String, String)
  | ReloadApp

