module Samples.Ops exposing
  ( bumpScanDrive
  , bumpRename
  , bumpCreateDir
  , bumpMoveToTrash
  , bumpEmptyTrash
  , bumpMove
  , bumpArrowKey
  , bumpSendSamples
  , bumpReceiveSamples

  , subscriptions
  , reportAndZero
  )

{-| Keeps count of the number of times the user does a particular operation
and reports the counts periodically to the stats server.

TODO: This should probably be generalized outside of Samples for the whole
app to use.

TODO: This whole facility should be able to be compiled out via a flag in
Build
-}

import Dict
import Time

import Missing.Maybe as Maybe
import Missing.Time as Time
import Report
import Samples.Base exposing (..)
import Samples.UpdateAction exposing (..)


bump : String -> UpdateAction
bump key = updateModel <| \m ->
  { m | opCounts =
    Dict.update key (Maybe.unwrap 1 ((+) 1) >> Just) m.opCounts }


bumpScanDrive : UpdateAction
bumpScanDrive = bump "scan-drive"

bumpRename : Bool -> UpdateAction
bumpRename isDir = bump <| if isDir then "rename-dir" else "rename-file"

bumpCreateDir : UpdateAction
bumpCreateDir = bump "create-dir"

bumpMoveToTrash : UpdateAction
bumpMoveToTrash = bump "move-to-trash"

bumpEmptyTrash : UpdateAction
bumpEmptyTrash = bump "empty-trash"

bumpMove : Bool -> UpdateAction
bumpMove isDir = bump <| if isDir then "move-dir" else "move-file"

bumpArrowKey : UpdateAction
bumpArrowKey = bump "arrow-key"

bumpSendSamples : UpdateAction
bumpSendSamples = bump "send-samples"

bumpReceiveSamples : UpdateAction
bumpReceiveSamples = bump "receive-samples"


reportInterval : Time.Time
reportInterval = 1 * Time.minute

subscriptions : Sub Msg
subscriptions = Time.every reportInterval (always ReportOps)

reportAndZero : UpdateAction
reportAndZero = withModel <| \model ->
  if List.any ((<) 0) (Dict.values model.opCounts)
    then
      addCmd (Report.opCounts model.opCounts)
      >> updateModel (\m -> { m | opCounts = Dict.empty })
    else
      noop

