module SysEx.Client exposing
  ( Request(..)
  , Requests
  )

{-| For clients of the SysEx system.
-}

import SysEx.Dump exposing (ElkDump)
import SysEx.Message exposing (ElkMessage)
import SysEx.SysEx exposing (SysEx)

{-| This type is returned from a sub-system's update function, all the way to
the top level `Main.Control.update`. There it is passed to `SysEx.makeRequests`
which will cause the message to be sent, and when the response comes, the
coressponding msg sent.
-}
type Request msg
  = SendSysEx SysEx
    -- Just sends the SysEx

  | RequestMessage ElkMessage (ElkMessage -> msg)
    -- Sends an API request,
    -- and will respond with the matching response or Timeout

  | SendDump ElkDump msg
    -- Sends a Dump Response, and responds when it is probably done (!)
    -- NB: Not really designed to have more than one in flight, though it will
    -- work, but might overwhelm the device.

  | StartDump ElkDump (ElkDump -> msg)
    -- Sends a Dump Request, and will respond with each of Dump Response that
    -- comes back until FinishDump is requested.
    -- NB: Not designed to have more than one of these in process at a time.

  | FinishDump
    -- Ends any responses for Dump Response messages

  | LoopbackProbe (Bool -> msg)
    -- Perform a loopback probe on the MIDI ports and return True if it is.

type alias Requests msg = List (Request msg)
