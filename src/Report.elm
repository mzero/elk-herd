module Report exposing
  ( appVersion
  , connectedPorts
  , deviceConnect
  , driveStats
  , trashStats
  , startDriveScan
  , opCounts
  )

{-| elk-herd can be configured to send statistics on usage to a server.
This information is anonymized, and contains info on what instruments,
OS versions, and MIDI port names are people using. It also takes some stats
on the size of +Drives, and which elk-heard features are being used.

The feature is enabled with the flag `Build.statsReporting`, which is
`False` by default. If enabled, this code becomes active, and the first
screen gives the user notice and the option to opt-out (which is preserved
in local storage). To actually send the stats, the URL of the stats-collector
must be added to the call to `hookup_ports` in index.html.

This module provides functions for formatting that information. The actual
sending is relegated to a port.
-}

import Dict
import Json.Encode as E

import Build
import Elektron.Drive as Drive
import Portage
import SysEx.Connect
import WebMidi


report : String -> String -> Cmd msg
report k v =
  if Build.statsReporting then Portage.report (k, v) else Cmd.none


appVersion : Cmd msg
appVersion = report "app-version" (String.fromInt Build.appVersion)

connectedPorts : WebMidi.Model -> Cmd msg
connectedPorts model =
  report "connected-ports" <| WebMidi.portsString model

deviceConnect : SysEx.Connect.Model -> WebMidi.Model -> Cmd msg
deviceConnect scModel wmModel =
  case SysEx.Connect.report scModel of
    Nothing -> Cmd.none
    Just r -> Cmd.batch [connectedPorts wmModel, report "device-connect" r]

reportStats : String -> Drive.Stats -> Cmd msg
reportStats key stats =
  let
    value = String.join ","
      <| List.map String.fromInt
        [stats.dirs, stats.files, stats.others, stats.totalSize]
  in
    report key value

driveStats : Drive.Drive -> Cmd msg
driveStats drive = Drive.driveStats drive |> reportStats "drive-stats"

trashStats : Drive.Drive -> Cmd msg
trashStats drive = Drive.trashStats drive |> reportStats "empty-trash-stats"

startDriveScan : Cmd msg
startDriveScan = report "drive-scan" ""


opCounts : Dict.Dict String Int -> Cmd msg
opCounts =
  Dict.toList >> List.map (Tuple.mapSecond E.int)
  >> E.object >> E.encode 0 >> report "op-counts"
