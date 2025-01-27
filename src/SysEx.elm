module SysEx exposing
  ( Model
  , init

  , Msg
  , update
  , subscriptions
  , commands
  , view

  , makeRequests
  )

{-| The SysEx "service": It follows the MUV pattern, but it is primarily there
to handle the SysEx requests of other subsystems in the code like the Pattern
view and the Samples view.

The concept of service in an elm application can be a little funky. It works
like this:

1. Other components return from their `update` functions, not just their
   new model, and any `Cmd`s, but also `SysEx.Client.Requests msg`. These are
   operations they'd like this module to do, and include messages of their
   message type (`msg`) they'd like to get back when the operation is done.

2. The main dispatcher (in the module `Client`) will then pass those to this
   module's `makeRequests` function, along with a function to turn the client's
   message type into the main message type.

3. This whole module's `Model` is parameterized by the main module's message
   type so that it can store, in it's model, operations that are in flight and
   what messages of the model's message type to generate when operations finish.

4. In this module's `update` function, getting messages back from the low-level
   MIDI operations, the state of these operations is updated, and perhaps new
   `Cmd` objects are returned, but also a list of `msgM` objects, which are
   messages for the main module to dispatch.

This component also contains the `SysEx.Debug` module as a sub-component, and
handles the MUV interaction with it.
-}

import Dict
import Html
import Process
import Task
import Time

import Build
import ByteArray exposing (ByteArray)
import Commands as C
import Missing.Maybe as Maybe
import Missing.Time as Time
import Portage
import SysEx.Client exposing (..)
import SysEx.Debug as Debug
import SysEx.Dump exposing (ElkDump)
import SysEx.Message as M exposing (ElkMessage)
import SysEx.Emulation as Emulation
import SysEx.SysEx exposing (..)
import WebMidi


type alias Entry msgM =
  { msg: ElkMessage
  , try: Int
  , rsp: ElkMessage -> msgM
  }

retries : Int
retries = 2

normalRetryInterval : Time.Time
normalRetryInterval = 5 * Time.second

slowRetryInterval : Time.Time
slowRetryInterval = 30 * Time.second

probeMessage : ByteArray
probeMessage = ByteArray.fromList
  [ 0xf0, 0x7e  -- start SysEx, non-realtime universal
  , 0x67        -- device ID, randomly picked, unlikely to collide
  , 0x7d        -- cancel (handshake) anything in progress
  , 0x57        -- packet number, randomly picked, unlikely to collide
  , 0xf7        -- end of SysEx
  ]

type alias Model msgM =
  { nextMsgId : Int
  , inFlight : Dict.Dict Int (Entry msgM)
  , dumpRespF : Maybe (ElkDump -> msgM)
  , loopbackResp : Maybe (Bool -> msgM)
  , debug : Debug.Model
  , emulation : Emulation.Model
  , retryInterval : Time.Time
  }

init : Bool -> Model msgM
init slow =
  Model startMsgId Dict.empty Nothing Nothing Debug.init Emulation.init
    (if slow then slowRetryInterval else normalRetryInterval)

startMsgId : Int
startMsgId = 20000  -- try to stay out of Transfer's way

bumpMsgId : Int -> Int
bumpMsgId n = if n < 65535 then n + 1 else startMsgId

debugSysEx : Bool -> Debug.Direction -> SysEx -> Debug.Model -> Debug.Model
debugSysEx forceShow direction sysEx debug =
  let
    show =
      case sysEx of
        -- match things here that should always be debugged
        ElektronAPI { msg } ->
          case msg of
            -- M.SampleFileInfoRequest _ _ -> True
            -- M.SampleFileInfoResponse _ _ _ _ -> True
            _ -> False
        _ -> False

    extra _ =
      case sysExToBytes sysEx of
        RawMidiBytes bs -> Just bs
        ElektronApiBytes bs ->
          case sysEx of
            ElektronAPI h -> Just bs
            _ -> Nothing

    extraDebug _ = extra () |> Maybe.unwrap identity Debug.addHexDump
  in
    if forceShow || show
      then
        Debug.addSysEx direction sysEx debug
        -- |> extraDebug ()    -- uncomment for a binary dump as well
      else debug

{-| See the comment in `SysEx.SysEx.SysExBytes` for why there are two ways to
send MIDI.
-}
sendSysEx : SysEx -> Cmd Msg
sendSysEx sysEx =
  case sysExToBytes sysEx of
    RawMidiBytes bs -> Portage.sendMidi bs
    ElektronApiBytes bs -> Portage.sendMidiElectronApi bs

sendMessage : Bool -> ElkMessage -> Model msgM -> (Model msgM, Int, Cmd Msg)
sendMessage log message model =
  let
    msgId = model.nextMsgId
    sysEx = ElektronAPI { msgId = msgId, respId = Nothing, msg = message }
    debug_ = debugSysEx log Debug.Sent sysEx model.debug

    (emulation1, sendCmd) = if Build.emulateDigitakt
      then
        let
          (emulation2, mRespMsg) = Emulation.respond message model.emulation
        in
          ( emulation2
          , case mRespMsg of
              Just respMsg ->
                Task.perform RecvMsg (
                  Process.sleep (1 * Time.millisecond)
                  |> Task.andThen (\_ -> Task.succeed (msgId, respMsg))
                )
              Nothing -> Cmd.none
          )
      else
       ( model.emulation
       , sendSysEx sysEx
       )

    timeCmd = Task.perform (\_ -> TimeOut msgId)
      <| Process.sleep model.retryInterval

  in
    ( { model | nextMsgId = bumpMsgId msgId
              , debug = debug_
              , emulation = emulation1
      }
    , msgId
    , Cmd.batch [ sendCmd, timeCmd ]
    )


sendMessageRequest : Entry msgM -> Model msgM -> (Model msgM, Cmd Msg)
sendMessageRequest entry model =
  let
    (model_, msgId, cmd) = sendMessage False entry.msg model
    entry_ = { entry | try = entry.try + 1 }
  in
    ( { model_ | inFlight = Dict.insert msgId entry_ model.inFlight}
    , cmd
    )

sendDumpRequest : ElkDump -> (ElkDump -> msgM) -> Model msgM -> (Model msgM, Cmd Msg)
sendDumpRequest dump respF model =
  let
    sysEx = ElektronDump dump
    debug_ = debugSysEx False Debug.Sent sysEx model.debug
    model_ = { model | dumpRespF = Just respF, debug = debug_ }
    cmd = sendSysEx sysEx
  in
    (model_, cmd)

sendLoopbackProbe : (Bool -> msgM) -> Model msgM -> (Model msgM, Cmd Msg)
sendLoopbackProbe msg model =
  ( { model | loopbackResp = Just msg }
  , Cmd.batch
    [ Portage.sendMidi probeMessage
    , Task.perform (\_ -> ProbeTimeout)
      <| Process.sleep (500 * Time.millisecond)
    ]
  )

loopbackProbeResult : Bool -> Model msgM -> (Model msgM, List msgM, Cmd Msg)
loopbackProbeResult result model =
  case model.loopbackResp of
    Just msgM -> ( { model | loopbackResp = Nothing }, [msgM result], Cmd.none)
    Nothing -> ( model, [], Cmd.none )

clearDumpRequest : Model msgM -> Model msgM
clearDumpRequest model =
    { model | dumpRespF = Nothing }


recv : SysEx -> Model msgM -> (Model msgM, List msgM, Cmd Msg)
recv sysEx model =
  let
    unexpectedRecv = (model, [], WebMidi.genUnexpectedMessageError)
    ignoreRecv = (model, [], Cmd.none)

    onInFlight f m = { m | inFlight = f m.inFlight }

    (model_, resps, cmd) = case sysEx of
      ElektronAPI { respId, msg } ->
        case respId of
          Just r ->
            case Dict.get r model.inFlight of
              Just entry ->
                ( onInFlight (Dict.remove r) model
                , [entry.rsp msg]
                , Cmd.none
                )
              Nothing -> ignoreRecv -- unexpectedRecv  {- some other app's message -}
          _ -> unexpectedRecv {- some other app's message -}
      ElektronDump dump ->
        case model.dumpRespF of
          Just respF -> (model, [respF dump], Cmd.none)
          Nothing -> ignoreRecv
      Undecoded msg bytes ->
        if ByteArray.same bytes probeMessage
          then loopbackProbeResult True model
          else ignoreRecv {- some other MIDI message -}

    debug_ = debugSysEx (List.isEmpty resps) Debug.Received sysEx model.debug
      -- always show received messages without an expected response handler
  in
   ( { model_ | debug = debug_ } , resps , cmd )

timeOut : Int -> Model msgM -> (Model msgM, List msgM, Cmd Msg)
timeOut msgId model =
    case Dict.get msgId model.inFlight of
      Nothing -> (model, [], Cmd.none)    -- ignore, already responded
      Just entry ->
        if entry.try < retries
          then
            let
              model_ = { model | inFlight = Dict.remove msgId model.inFlight }
              send_ e = (\(m, c) -> (m, [], c)) << sendMessageRequest entry
            in
              send_ entry model_          -- try again
          else
            let
              sysEx = ElektronAPI { msgId = 99, respId = Just msgId, msg = M.TimeOut }
            in
              recv sysEx model           -- give up, reply with timeout



type Msg
  = RecvMidi (List Int)
  | RecvMsg (Int, ElkMessage)
  | OnDebug Debug.Msg
  | TimeOut Int
  | ProbeTimeout


update : Msg -> Model msgM -> (Model msgM, List msgM, Cmd Msg)
update msg model = case msg of
  RecvMidi ai ->
    case sysExFromMidi ai of
      Just sysEx -> recv sysEx model
      Nothing -> (model, [], Cmd.none)

  RecvMsg (respId, msg_) ->
    let
      sysEx = ElektronAPI { msgId = 99, respId = Just respId, msg = msg_ }
    in
      recv sysEx model

  TimeOut respId ->
    timeOut respId model

  ProbeTimeout ->
    loopbackProbeResult False model

  OnDebug dMsg ->
    let
      (debug_, sysExs, msgs) = Debug.update dMsg model.debug
      cmds0 = List.map sendSysEx sysExs
      debug0 = List.foldl (Debug.addSysEx Debug.Sent) debug_ sysExs

      sendOne msg_ (m, cs) =
        let
          (m_, _, c_) = sendMessage True msg_ m
        in
          (m_, c_ :: cs)

      (model_, cmds) = List.foldl sendOne ({ model | debug = debug0 }, cmds0) msgs
    in
      (model_, [], Cmd.batch cmds)



subscriptions : Model msgM -> Sub Msg
subscriptions model = Portage.recvMidi RecvMidi


commands : C.Commands Msg
commands = []
  ++ C.map OnDebug Debug.commands


view : Model msgM -> Html.Html Msg
view model = Html.map OnDebug <| Debug.view model.debug




makeRequests :
  (msgC -> msgM)
  -> List (Request msgC)
  -> Model msgM
  -> (Model msgM, Cmd Msg)
makeRequests msgF reqs m0 =
  let
    makeOne req m =
      case req of
        RequestMessage msg respF ->
          sendMessageRequest { msg = msg, try = 0, rsp = respF >> msgF } m
        StartDump dump respF ->
          sendDumpRequest dump (respF >> msgF) m
        FinishDump ->
          (clearDumpRequest m, Cmd.none)
        LoopbackProbe msg ->
          sendLoopbackProbe (msg >> msgF) m


    chain req (m, cs) =
      let
        (m_, c) = makeOne req m
      in
        (m_, c :: cs)
  in
    Tuple.mapSecond Cmd.batch <| List.foldl chain (m0, []) reqs

