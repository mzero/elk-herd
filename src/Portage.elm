port module Portage exposing
  ( readFile, fileContents, fileCancel

  , readBinaryFile
  , binaryFileContents
  , binaryFileError

  , writeBinaryFile

  , droppedFile
  , readAudioFile
  , sampleData, sampleDataError

  , writeAudioFile

  , readLocalStorage, localStorage, writeLocalStorage

  , MidiPort
  , midiAccess
  , selectMidiIn, selectMidiOut
  , sendMidi, sendMidiElectronApi
  , recvMidi
  , midiError, genMidiError
  , closeMidi

  , startUpApp
  , storedAppVersionAndOptIn
  , onlyInstanceMinder

  , resetAppVersion
  , setAppVersion
  , setOptIn

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

-- save a file, given the value and suggested download name
port writeFile : (String, String, String) -> Cmd msg
    -- the tuple is: file name, mime type, contents


-- read a file
    -- This proceeds in two steps:
    -- 1) After getting some event from a user UI action, pass it to readJsonFile
    -- 2) When (if) that works, the string value will be passed back via fileContents
    -- 3) If the user cancels, fileCancel will be triggered

port readFile : E.Value -> Cmd msg
port fileContents : (String -> msg) -> Sub msg
port fileCancel : (() -> msg) -> Sub msg


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
port readAudioFile : E.Value -> Cmd msg
port sampleData : (List (List Int) -> msg) -> Sub msg
port sampleDataError : (String -> msg) -> Sub msg

-- write audio file

writeAudioFile : String -> List ByteArray -> Cmd msg
writeAudioFile name bss =
  writeAudioFile_ (name, E.list byteArrayToPort bss)

port writeAudioFile_ : (String, E.Value) -> Cmd msg

-- Local Storage

port readLocalStorage : String -> Cmd msg
port localStorage : ((String, Maybe String) -> msg) -> Sub msg

port writeLocalStorage : (String, String) -> Cmd msg



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

port startUpApp : () -> Cmd msg
port storedAppVersionAndOptIn : ((Int, Maybe Bool) -> msg) -> Sub msg
port onlyInstanceMinder : (() -> msg) -> Sub msg

port resetAppVersion : Int -> Cmd msg
port setAppVersion : Int -> Cmd msg
port setOptIn : Bool -> Cmd msg

port report : (String, String) -> Cmd msg

port log : String -> Cmd msg

