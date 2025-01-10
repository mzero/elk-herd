module Samples.UpdateAction exposing
  ( UpdateAction
  , run

  , noop
  , guard
  , done
  , updateModel
  , addCmd
  , addRequest

  , withModel

  , sequence
  )

{-| A small monoid for combining update results. Almost as good as a true
monad...

Note that `UpdateResult` is not exported from this module, only `UpdateAction`
is. Hence, other code can build up updates from the primitive actions exported
here, they can combine them like normal functions... but the final computation
of the update result (in `run`) is controlled by code here. This allows things
like the `done` flag to be enforced.

TODO: This should really be generalized over the type of the Model and Msg...
and then used in Project.* as well.
-}

import Samples.Base exposing (Model, Msg)
import SysEx.Client exposing (Request(..), Requests)
import SysEx.Message exposing (ElkMessage)

{-| The results gathered so far during an action run.

The done flag is used to inhibit any further actions from taking place.
-}
type UpdateResult = UR
  { done : Bool
  , model : Model
  , cmds : List (Cmd Msg)
  , reqs : Requests Msg
  }

type alias UpdateAction = UpdateResult -> UpdateResult

{-| Run an update action
-}
run : Model -> UpdateAction -> (Model, Cmd Msg, Requests Msg)
run model act =
  let
    (UR um) = act (UR { model = model, done = False, cmds = [], reqs = [] })
  in
    (um.model, Cmd.batch (List.reverse um.cmds), List.reverse um.reqs)


noop : UpdateAction
noop = identity

{-| skip all subsequent actions if the boolean is false
-}
guard : Bool -> UpdateAction
guard b (UR u) = UR { u | done = u.done || not b }

{-| skip all subsequent actions
-}
done : UpdateAction
done (UR u) = UR { u | done = True }

ifNotDone : UpdateAction -> UpdateAction
ifNotDone f = \(UR u as um) -> if u.done then um else f um

updateModel : (Model -> Model) -> UpdateAction
updateModel f = ifNotDone <| \(UR u) -> UR { u | model = f u.model }

addCmd : Cmd Msg -> UpdateAction
addCmd cmd = ifNotDone <| \(UR u) -> UR { u | cmds = cmd :: u.cmds }

addRequest : ElkMessage -> (ElkMessage -> Msg) -> UpdateAction
addRequest msg respF =
  ifNotDone <| \(UR u) -> UR { u | reqs = RequestMessage msg respF  :: u.reqs }

{-| let's you access the model at this point in the computation
-}
withModel : (Model -> UpdateAction) -> UpdateAction
withModel f = ifNotDone <| \(UR u as um) -> f u.model um

{-| combines a list of actions in an action
-}
sequence : List UpdateAction -> UpdateAction
sequence acts um = List.foldl identity um acts
