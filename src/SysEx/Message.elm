module SysEx.Message exposing
  ( ElkMessage(..)
  , DirEntry
  , Fd

  , messageBuilder
  , parseMessage
  , viewMessage
  )

import Dict
import Html

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import ByteArray.Parser as Parser
import SysEx.ApiUtil exposing (..)
import SysEx.Internal exposing (..)
import Util


type alias DirEntry = SysEx.ApiUtil.DirEntry
type alias Fd = Int

{-| The messages that can be sent and received with the Elektron API.

In addition to the defined ones, there are a few extras here: `Unknown` captures
received messages with an unknown id. `TestRequest*` messages let the debugger
send some arbitrary messages. `TimeOut` is a synthetic message generated if
there is no response received to a request.
-}
type ElkMessage
  = Unknown Int ByteArray

  | DeviceRequest
  | DeviceResponse Int (List Int) String
  | VersionRequest
  | VersionResponse String String

  | DirListRequest String
  | DirListResponse (List DirEntry)
  | DirCreateRequest String
  | DirCreateResponse Bool
  | DirDeleteRequest String
  | DirDeleteResponse Bool

  | FileDeleteRequest String
  | FileDeleteResponse Bool
  | ItemRenameRequest String String
  | ItemRenameResponse Bool

  | SampleFileInfoRequest Int Int
  | SampleFileInfoResponse Bool Int Int String

  | FileReadOpenRequest String
  | FileReadOpenResponse Bool Fd Int
  | FileReadCloseRequest Fd
  | FileReadCloseResponse Fd Int
  | FileReadRequest Fd Int Int
  | FileReadResponse Bool Fd Int Int Int ByteArray

  | FileWriteOpenRequest Int String
  | FileWriteOpenResponse Bool Fd
  | FileWriteCloseRequest Fd Int
  | FileWriteCloseResponse Bool Fd Int
  | FileWriteRequest Fd Int Int ByteArray
  | FileWriteResponse Bool Int

  | TestRequest Int
  | TestRequestArgs Int ByteArray
  | TestRequestString Int String
  | TestRequest2String Int String String
  | TimeOut


{- APIs come in pairs of request and response messages. Each `api*` value is
description of such a pair. It is built from two `Msg*` objects which
descirbe the arguments to the two messages.
-}

-- 0x0n Messages: Device information --

apiDevice : Api (Msg0 ElkMessage) (Msg3 ElkMessage Int (List Int) String)
apiDevice = buildApi 0x01 "Device"
  (msg0 DeviceRequest)
  (msg3 DeviceResponse
    (argByte "productId")
    (argByteList "msgs")
    (argString0win1252 "deviceName"))

apiVersion : Api (Msg0 ElkMessage) (Msg2 ElkMessage String String)
apiVersion = buildApi 0x02 "Version"
  (msg0 VersionRequest)
  (msg2 VersionResponse
    (argString0win1252 "build")
    (argString0win1252 "version"))

-- 0x1n Messages: Directory operations --

apiDirList : Api (Msg1 ElkMessage String) (Msg1 ElkMessage (List DirEntry))
apiDirList = buildApi 0x10 "DirList"
  (msg1 DirListRequest (argString0win1252 "path"))
  (msg1 DirListResponse (argListAll argDirEntry))

apiDirCreate : Api (Msg1 ElkMessage String) (Msg1 ElkMessage Bool)
apiDirCreate = buildApi 0x11 "DirCreate"
  (msg1 DirCreateRequest (argString0win1252 "path"))
  (msg1 DirCreateResponse (argBool "ok"))

apiDirDelete : Api (Msg1 ElkMessage String) (Msg1 ElkMessage Bool)
apiDirDelete = buildApi 0x12 "DirDelete"
  (msg1 DirDeleteRequest (argString0win1252 "path"))
  (msg1 DirDeleteResponse (argBool "ok"))

-- 0x13 Enumerate sample files, deprecated

-- 0x2n Messages: File operations --

apiFileDelete : Api (Msg1 ElkMessage String) (Msg1 ElkMessage Bool)
apiFileDelete = buildApi 0x20 "FileDelete"
  (msg1 FileDeleteRequest (argString0win1252 "path"))
  (msg1 FileDeleteResponse (argBool "ok"))

apiItemRename : Api (Msg2 ElkMessage String String) (Msg1 ElkMessage Bool)
apiItemRename = buildApi 0x21 "ItemRename"
  (msg2 ItemRenameRequest
    (argString0win1252 "from")
    (argString0win1252 "to"))
  (msg1 ItemRenameResponse (argBool "ok"))

-- 0x22 Get Sample File Info From Path (V1), deprecated

apiSampleFileInfo :
  Api (Msg2 ElkMessage Int Int) (Msg4 ElkMessage Bool Int Int String)
apiSampleFileInfo = buildApi 0x23 "SampleFileInfo"
 (msg2 SampleFileInfoRequest
   (argUint32be "hash")
   (argUint32be "len"))
 (msg4 SampleFileInfoResponse
   (argBool "ok")
   (argUint32be "len")        -- yes, these are backwards
   (argUint32be "hash")
   (argString0win1252 "path"))

-- 0x3n Messages: Transfer files from device --

apiFileReadOpen : Api (Msg1 ElkMessage String) (Msg3 ElkMessage Bool Fd Int)
apiFileReadOpen = buildApi 0x30 "FileReadOpen"
  (msg1 FileReadOpenRequest
    (argString0win1252 "path"))
  (msg3 FileReadOpenResponse
    (argBool "ok")
    (argUint32be "fd")
    (argUint32be "total-len"))

apiFileReadClose : Api (Msg1 ElkMessage Fd) (Msg2 ElkMessage Fd Int)
apiFileReadClose = buildApi 0x31 "FileReadClose"
  (msg1 FileReadCloseRequest
    (argUint32be "fd"))
  (msg2 FileReadCloseResponse
    (argUint32be "fd")
    (argUint32be "total-len"))

apiFileRead :
  Api (Msg3 ElkMessage Fd Int Int) (Msg6 ElkMessage Bool Fd Int Int Int ByteArray)
apiFileRead = buildApi 0x32 "FileRead"
  (msg3 FileReadRequest
    (argUint32be "fd")
    (argUint32be "chunk-len")
    (argUint32be "chunk-start"))
  (msg6 FileReadResponse
    (argBool "ok")
    (argUint32be "fd")
    (argUint32be "chunk-len")
    (argUint32be "chunk-start")
    (argUint32be "chunk-end")
    (argBytes "data"))

-- 0x4n Messages: Transfer files to device --

apiFileWriteOpen : Api (Msg2 ElkMessage Int String) (Msg2 ElkMessage Bool Fd)
apiFileWriteOpen = buildApi 0x40 "FileWriteOpen"
  (msg2 FileWriteOpenRequest
    (argUint32be "total-len")
    (argString0win1252 "path"))
  (msg2 FileWriteOpenResponse
    (argBool "ok")
    (argUint32be "fd"))

apiFileWriteClose : Api (Msg2 ElkMessage Fd Int) (Msg3 ElkMessage Bool Fd Int)
apiFileWriteClose = buildApi 0x41 "FileWriteClose"
  (msg2 FileWriteCloseRequest
    (argUint32be "fd")
    (argUint32be "total-len"))
  (msg3 FileWriteCloseResponse
    (argBool "ok")
    (argUint32be "fd")
    (argUint32be "total-len"))

apiFileWrite :
  Api (Msg4 ElkMessage Fd Int Int ByteArray) (Msg2 ElkMessage Bool Int)
apiFileWrite = buildApi 0x42 "FileWrite"
  (msg4 FileWriteRequest
    (argUint32be "fd")
    (argUint32be "chunk-len")
    (argUint32be "chunk-start")
    (argBytes "data"))
  (msg2 FileWriteResponse
    (argBool "ok")
    (argUint32be "written-len"))


{- These final three exported functions, `messageBuilder`, `parseMessage`,
and `viewMessage`, make use of the API descriptors above to be able to
build, parse, or create a view for an `ElkMessage`.
-}


messageBuilder : ElkMessage -> Builder.Builder
messageBuilder msg = case msg of
  Unknown id data                   -> Builder.sequence
                                       [ Builder.byte id, Builder.bytes data ]

  DeviceRequest                     -> apiDevice.request.build
  DeviceResponse dev msgs model     -> apiDevice.response.build dev msgs model
  VersionRequest                    -> apiVersion.request.build
  VersionResponse build version     -> apiVersion.response.build build version
  DirListRequest path               -> apiDirList.request.build path
  DirListResponse entries           -> apiDirList.response.build entries
  DirCreateRequest path             -> apiDirCreate.request.build path
  DirCreateResponse ok              -> apiDirCreate.response.build ok
  DirDeleteRequest path             -> apiDirDelete.request.build path
  DirDeleteResponse ok              -> apiDirDelete.response.build ok
  FileDeleteRequest path            -> apiFileDelete.request.build path
  FileDeleteResponse ok             -> apiFileDelete.response.build ok
  ItemRenameRequest from to         -> apiItemRename.request.build from to
  ItemRenameResponse ok             -> apiItemRename.response.build ok
  SampleFileInfoRequest h s         -> apiSampleFileInfo.request.build h s
  SampleFileInfoResponse ok s h n   -> apiSampleFileInfo.response.build ok s h n
  FileReadOpenRequest path          -> apiFileReadOpen.request.build path
  FileReadOpenResponse ok fd len    -> apiFileReadOpen.response.build ok fd len
  FileReadCloseRequest fd           -> apiFileReadClose.request.build fd
  FileReadCloseResponse fd len      -> apiFileReadClose.response.build fd len
  FileReadRequest fd len offset     -> apiFileRead.request.build fd len offset
  FileReadResponse ok fd len offset end data
                                    -> apiFileRead.response.build ok fd len offset end data
  FileWriteOpenRequest len path     -> apiFileWriteOpen.request.build len path
  FileWriteOpenResponse ok len      -> apiFileWriteOpen.response.build ok len
  FileWriteCloseRequest fd len      -> apiFileWriteClose.request.build fd len
  FileWriteCloseResponse ok fd len  -> apiFileWriteClose.response.build ok fd len
  FileWriteRequest fd len start bs  -> apiFileWrite.request.build fd len start bs
  FileWriteResponse ok len          -> apiFileWrite.response.build ok len

  TestRequest id                    -> Builder.byte id
  TestRequestArgs id args           -> Builder.sequence
                                       [ Builder.byte id, Builder.bytes args ]
  TestRequestString id arg          -> Builder.sequence
                                       [ Builder.byte id
                                       , Builder.string0win1252 arg
                                       ]
  TestRequest2String id s1 s2       -> Builder.sequence
                                       [ Builder.byte id
                                       , Builder.string0win1252 s1
                                       , Builder.string0win1252 s2
                                       ]
  TimeOut                           -> Builder.empty


parserApiTable : Dict.Dict Int (Parser.Parser ElkMessage)
parserApiTable =
  let
    msg m = [ (m.id, m.request.parse), (m.id + 0x80, m.response.parse) ]
  in
    Dict.fromList <| List.concat
      [ msg apiDevice
      , msg apiVersion
      , msg apiDirList
      , msg apiDirCreate
      , msg apiDirDelete
      , msg apiFileDelete
      , msg apiItemRename
      , msg apiSampleFileInfo
      , msg apiFileReadOpen
      , msg apiFileReadClose
      , msg apiFileRead
      , msg apiFileWriteOpen
      , msg apiFileWriteClose
      , msg apiFileWrite
      ]

parseMessage : Parser.Parser ElkMessage
parseMessage =
  Parser.byte |> Parser.andThen (\id ->
    case Dict.get id parserApiTable of
      Just parse -> parse
      Nothing ->
        Parser.rest |> Parser.andThen (\ba -> Parser.succeed <| Unknown id ba )
    )

viewMessage : ElkMessage -> List (Html.Html msg)
viewMessage msg = List.map (Html.map never) <| case msg of
  Unknown id data ->
    [ fieldView "message" <| Util.hexUint8 id
    , hexdumpFieldView "data" data
    ]

  DeviceRequest                     -> apiDevice.request.view
  DeviceResponse dev msgs model     -> apiDevice.response.view dev msgs model
  VersionRequest                    -> apiVersion.request.view
  VersionResponse build version     -> apiVersion.response.view build version
  DirListRequest path               -> apiDirList.request.view path
  DirListResponse entries           -> apiDirList.response.view entries
  DirCreateRequest path             -> apiDirCreate.request.view path
  DirCreateResponse ok              -> apiDirCreate.response.view ok
  DirDeleteRequest path             -> apiDirDelete.request.view path
  DirDeleteResponse ok              -> apiDirDelete.response.view ok
  FileDeleteRequest path            -> apiFileDelete.request.view path
  FileDeleteResponse ok             -> apiFileDelete.response.view ok
  ItemRenameRequest from to         -> apiItemRename.request.view from to
  ItemRenameResponse ok             -> apiItemRename.response.view ok
  SampleFileInfoRequest h s         -> apiSampleFileInfo.request.view h s
  SampleFileInfoResponse ok s h n   -> apiSampleFileInfo.response.view ok s h n
  FileReadOpenRequest path          -> apiFileReadOpen.request.view path
  FileReadOpenResponse ok fd len    -> apiFileReadOpen.response.view ok fd len
  FileReadCloseRequest fd           -> apiFileReadClose.request.view fd
  FileReadCloseResponse fd len      -> apiFileReadClose.response.view fd len
  FileReadRequest fd len offset     -> apiFileRead.request.view fd len offset
  FileReadResponse ok fd len offset end data
                                    -> apiFileRead.response.view ok fd len offset end data
  FileWriteOpenRequest len path     -> apiFileWriteOpen.request.view len path
  FileWriteOpenResponse ok len      -> apiFileWriteOpen.response.view ok len
  FileWriteCloseRequest fd len      -> apiFileWriteClose.request.view fd len
  FileWriteCloseResponse ok fd len  -> apiFileWriteClose.response.view ok fd len
  FileWriteRequest fd len start bs  -> apiFileWrite.request.view fd len start bs
  FileWriteResponse ok len          -> apiFileWrite.response.view ok len

  TestRequest id ->
    [ fieldView "message" "TestRequest"
    , fieldView "id" <| Util.hexUint8 id
    ]
  TestRequestArgs id args ->
    [ fieldView "message" "TestRequestArgs"
    , fieldView "id" <| Util.hexUint8 id
    , hexdumpFieldView "args" args
    ]
  TestRequestString id arg ->
    [ fieldView "message" "TestRequestArgs"
    , fieldView "id" <| Util.hexUint8 id
    , fieldView "arg" arg
    ]
  TestRequest2String id s1 s2 ->
    [ fieldView "message" "TestRequestArgs"
    , fieldView "id" <| Util.hexUint8 id
    , fieldView "s1" s1
    , fieldView "s2" s2
    ]
  TimeOut ->
    [ fieldView "error" "time out"
    ]

