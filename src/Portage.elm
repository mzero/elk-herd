port module Portage exposing
  ( readBinaryFile
  , binaryFileContents
  , binaryFileError

  , writeBinaryFile

  , droppedFile
  , readAudioFile
  , sampleData, sampleDataError

  , writeAudioFile

  , MidiPort
  , midiAccess
  , selectMidiIn, selectMidiOut
  , sendMidi, sendMidiElectronApi
  , recvMidi
  , midiError, genMidiError
  , closeMidi

  , startUpApp
  , onlyInstanceMinder

  , AppInfo
  , storedAppInfo
  , setAppVersion
  , setOptIn
  , setFlags

  , report
  , log
  )

import Array exposing (Array)
import Json.Encode as E

import ByteArray exposing (ByteArray)

byteArrayToPort : ByteArray -> E.Value
byteArrayToPort =  E.array E.int << ByteArray.toArray

byteArrayFromPort : Array Int -> ByteArray
byteArrayFromPort = ByteArray.fromArray

-- File operations

port readBinaryFile : E.Value -> Cmd msg
binaryFileContents : (ByteArray -> msg) -> Sub msg
binaryFileContents f = binaryFileContents_ (byteArrayFromPort >> f)

port binaryFileContents_ : (Array Int -> msg) -> Sub msg
port binaryFileError : (String -> msg) -> Sub msg


writeBinaryFile : String -> String -> List ByteArray -> Cmd msg
writeBinaryFile name mimetype bss =
  writeBinaryFile_ (name, mimetype, E.list byteArrayToPort bss)


port writeBinaryFile_ : (String, String, E.Value) -> Cmd msg

-- read audio file

port droppedFile : ((List String, E.Value) -> msg) -> Sub msg
port readAudioFile : (Bool, E.Value) -> Cmd msg
port sampleData : (List (List Int) -> msg) -> Sub msg
port sampleDataError : (String -> msg) -> Sub msg

-- write audio file

writeAudioFile : String -> List ByteArray -> Cmd msg
writeAudioFile name bss =
  writeAudioFile_ (name, E.list byteArrayToPort bss)

port writeAudioFile_ : (String, E.Value) -> Cmd msg



-- Web MIDI

type alias MidiPort =
  { id : String
  , manufacturer : String
  , name : String
  , state : String
  , connection : String
  }

port midiAccess : ((List MidiPort, List MidiPort) -> msg) -> Sub msg
port selectMidiIn : Maybe String -> Cmd msg
port selectMidiOut : Maybe String -> Cmd msg

sendMidi : ByteArray -> Cmd msg
sendMidi = sendMidi_ << byteArrayToPort

port sendMidi_ : E.Value -> Cmd msg
port recvMidi : (List Int -> msg) -> Sub msg

sendMidiElectronApi : ByteArray -> Cmd msg
sendMidiElectronApi = sendMidiElectronApi_ << byteArrayToPort

port sendMidiElectronApi_ : E.Value -> Cmd msg

port midiError : ((String, String) -> msg) -> Sub msg
port genMidiError : (String, String) -> Cmd msg

port closeMidi : () -> Cmd msg


-- Application Tooling

type alias AppInfo =
  { optIn : Maybe Bool
  , appVersion : Int
  , flags : String
  }
port startUpApp : () -> Cmd msg
port storedAppInfo : (AppInfo -> msg) -> Sub msg
port onlyInstanceMinder : (() -> msg) -> Sub msg

port setAppVersion : Int -> Cmd msg
port setOptIn : Bool -> Cmd msg
port setFlags : String -> Cmd msg

port report : (String, String) -> Cmd msg

port log : String -> Cmd msg

