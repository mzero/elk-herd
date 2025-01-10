module Samples.Transfer exposing
  ( prepSamples
  , prepSampleName
  , skipSampleName

  , Pump
  , PumpNext(..)
  , startRead
  , startWrite
  , pump

  , cancelPump
  , pumpProgress
  )

{-| When sending or receiving sample files with the instrument, it must be
done in a series of API messages: open, transfer chunks, close. This code
manages that sequence of operations, and things that could go wrong.
-}

import Bitwise
import Regex

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import Missing.List as List
import Missing.Regex as Regex
import SysEx.Message exposing (ElkMessage(..), Fd)


{--
-- This code is for testing that the sample preparation code works.
-- in particular that sample values make it through js and elm unscathed.

isBigRamp : String -> List Float -> Bool
isBigRamp fileName floats =
  fileName == "big-ramp.wav" && List.length floats == 65536

testBigRamp : List Int -> Bool
testBigRamp samples =
  let
    indexed_samples = List.zip (List.range -32768 32767) samples
    bad_samples = Debug.log "bad samples"
      <| List.filter (\(i,s) -> i /= s) indexed_samples
  in
    List.isEmpty bad_samples
--}

{-| This operation is now done in a port, see readAudioFile.
-}
prepSamples : String -> List Float -> ByteArray
prepSamples fileName floats =
  let
    to16bitPcm f = floor (f * 32767.5)
    samples = List.map to16bitPcm floats
    length = List.length samples

    header = Builder.buildSequence
      [ Builder.byte 0                 -- type
      , Builder.zeros 3                -- reserved
      , Builder.uint32be (length * 2)  -- sample data length in bytes
      , Builder.uint32be 48000         -- sample rate
      , Builder.uint32be 0             -- first sample index
      , Builder.uint32be (length - 1)  -- last sample index
      , Builder.byte 0x7f              -- loop type (= no loop)
      , Builder.zeros 43               -- reserved
      ]

    body = Builder.buildSequence
      <| List.map (Builder.uint16be << Bitwise.and 0xffff) samples

  in
    ByteArray.append header body
  {--
    if length > 0 && (not (isBigRamp fileName floats) || testBigRamp samples)
      then Builder.build fileParts
      else Debug.log "ramp failed" <| Builder.build fileParts
    --}


audioFileExtensionRegex : Regex.Regex
audioFileExtensionRegex = Regex.regex "\\.(aac|flac|mp3|mp4|ogg|wav)$"


prepSampleName : String -> String
prepSampleName =
  Regex.replace audioFileExtensionRegex (always "")

skipFilesRegex : Regex.Regex
skipFilesRegex = Regex.regex "(^\\.)|(\\.asd$)"

skipSampleName : String -> Bool
skipSampleName = Regex.contains skipFilesRegex


chunkSize : Int
chunkSize = 0x2000


{-| The state of the pump. This reflects the operation that has been sent
to the instrument, waiting for the instruments response.
-}
type Pump
  = ReadOpening
  | ReadReading Fd Int (List ByteArray) Int
  | ReadClosing Fd Int (List ByteArray)

  | WriteOpening    Int (List ByteArray)
  | WriteWriting Fd Int (List ByteArray) Int
  | WriteClosing Fd Int

  | CancelingReadOpen   String
  | CancelingRead       String Fd
  | CancelingWriteOpen  String
  | CancelingWrite      String Fd
  | Canceling           String

type PumpNext
  = Continue Pump ElkMessage
  | ReadDone (List ByteArray)
  | WriteDone
  | Error String


startRead : String -> (Pump, ElkMessage)
startRead path =
  ( ReadOpening
  , FileReadOpenRequest path
  )

startWrite : String -> List (List Int) -> (Pump, ElkMessage)
startWrite path chunks =
  let
    bss = List.map (Builder.build << Builder.byteList) chunks
    size = List.sum <| List.map ByteArray.length bss
  in
    ( WriteOpening size bss
    , FileWriteOpenRequest size path
    )

{-| Given the current state of the pump, handles a response message, and
returns what's next.
-}
pump : Pump -> ElkMessage -> PumpNext
pump p msg =
  let
    guard test message next =
      if test
        then next
        else Error message

    guardOk ok = guard ok "Instrument returned error."
    guardFd fd fd_ = guard (fd == fd_) "Instrument is confused."

    readNextChunk fd size bss read =
      if read < size
        then
          let
            chunkLen = clamp 0 chunkSize (size - read)
          in
            Continue
              (ReadReading fd size bss (read + chunkLen))
              (FileReadRequest fd chunkLen read)
        else
          Continue
            (ReadClosing fd size bss)
            (FileReadCloseRequest fd)
    writeNextChunk fd size bss offset =
      case bss of
        chunk :: bss_ ->
          let
            chunkLen = ByteArray.length chunk
          in
            Continue
              (WriteWriting fd size bss_ (offset + chunkLen))
              (FileWriteRequest fd chunkLen offset chunk)
        [] ->
          Continue
            (WriteClosing fd size)
            (FileWriteCloseRequest fd size)
  in
    case (p, msg) of
      (ReadOpening, FileReadOpenResponse ok fd size) ->
        guardOk ok
        <| readNextChunk fd size [] 0
      (ReadReading fd size bss read, FileReadResponse ok fd_ len start end chunk) ->
        guardOk ok
        <| guardFd fd fd_
        <| guard (len + start == end) "read chunk response error"
        <| guard (read == end) "read chunk end error"
        <| readNextChunk fd size (chunk :: bss) end
      (ReadClosing fd size bss, FileReadCloseResponse fd_ size_) ->
        guardFd fd fd_
        <| guard (size == size_ || size == size_ - 0x10) "read total size rror"
              -- prior to DT 1.08, size_ was sent 16 too long
        <| ReadDone (List.reverse bss)

      (WriteOpening size bss, FileWriteOpenResponse ok fd) ->
        guardOk ok
        <| writeNextChunk fd size bss 0
      (WriteWriting fd size bss offset, FileWriteResponse ok offset_) ->
        guardOk ok
        <| guard (offset == offset_) "write returned wrong size"
        <| writeNextChunk fd size bss offset
      (WriteClosing fd size, FileWriteCloseResponse ok fd_ size_) ->
        guardOk ok
        <| guardFd fd fd_
        <| guard (size == size_) "close returned wrong size"
        <| WriteDone

      (CancelingReadOpen msg_, FileReadOpenResponse ok fd _) ->
        Continue (Canceling msg_) (FileReadCloseRequest fd)
      (CancelingRead msg_ fd, _) ->
        Continue (Canceling msg_) (FileReadCloseRequest fd)
      (CancelingWriteOpen msg_, FileWriteOpenResponse ok fd) ->
        Continue (Canceling msg_) (FileWriteCloseRequest fd 0)
      (CancelingWrite msg_ fd, _) ->
        Continue (Canceling msg_) (FileWriteCloseRequest fd 0)
      (Canceling msg_, _) ->
          Error msg_

      (_, TimeOut) ->
        Error "Timed out waiting for instrument response."
      _ ->
        Error "Unexpected message from instrument."

{-| There are different cancelling pump states for the open vs. read/write.
This is because if cancelling during open, we must wait for the open to
succeed to get the fd... so that we can close it!
-}
cancelPump : String -> Pump -> Pump
cancelPump msg p = case p of
  ReadOpening           -> CancelingReadOpen msg
  ReadReading fd _ _ _  -> CancelingRead msg fd
  WriteOpening _ _      -> CancelingWriteOpen msg
  WriteWriting fd _ _ _ -> CancelingWrite msg fd
  _ -> p


pumpProgress : Pump -> Float
pumpProgress p =
  let
    start = 0.0
    pct t n = toFloat n / toFloat (max 1 t)
    done = 1.0
  in
    case p of
      ReadOpening -> start
      ReadReading _ t _ n -> pct t n
      ReadClosing _ _ _ -> done

      WriteOpening _ _ -> start
      WriteWriting _ t _ n -> pct t n
      WriteClosing _ _ -> done

      _ -> done

