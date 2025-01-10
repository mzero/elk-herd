module SysEx.Client exposing
  ( Request(..)
  , Requests
  )

{-| For clients of the SysEx system.
-}

import SysEx.Dump exposing (ElkDump)
import SysEx.Message exposing (ElkMessage)

{-| This type is returned from a sub-system's update function, all the way to
the top level `Main.Control.update`. There it is passed to `SysEx.makeRequests`
which will cause the message to be sent, and when the response comes, the
coressponding msg sent.
-}
type Request msg
  = RequestMessage ElkMessage (ElkMessage -> msg)
  | StartDump ElkDump (ElkDump -> msg)
  | FinishDump
  | LoopbackProbe (Bool -> msg)

type alias Requests msg = List (Request msg)
