module Samples.View exposing
  ( commands
  , view
  )


import Html
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy as Lazy
import Json.Decode as D
import Set

import Alert
import Commands as C
import Elektron.Drive as Drive
import Elektron.Path as Path exposing (Path)
import Html.Aria as Aria
import Missing.Html.Events as Events
import Missing.Maybe as Maybe
import Samples.Base exposing (..)
import Samples.Transfer exposing (..)
import Util


commands : C.Commands Msg
commands =
  [ C.Group "Selected Item Commands"
    [ C.Button "cmd-rename-item" "Rename" True BeginRename
    , C.Button "cmd-move-item" "Move" True BeginMove
    , C.Button "cmd-create-dir" "New Directory" True CreateDir
    , C.Button "cmd-delete-item" "Move to Trash" True MoveToTrash
    , C.Button "cmd-empty-trash" "Empty Trash" True EmptyTrash
    ]
  , C.Group "Sample Commands"
    [ C.LoadFile "cmd-send-sample" "Send Sample File" True
        SendSampleFiles "audio/*" True
    , C.Button "cmd-receive-sample" "Get Sample File" True ReceiveSampleFiles
    ]
  , C.Group "+Drive Commands"
    [ C.Button "cmd-scan-drive" "Scan +Drive" True KickOff
    , C.Button "cmd-find-collisions" "Find Duplicates" True FindDuplicates
    ]
  ]



onEditShortcutKeys : msg -> msg -> Html.Attribute msg
onEditShortcutKeys commitMsg cancelMsg =
  Events.onExclusive "keydown"
    (Events.keyCode |> D.andThen
      ( \kc -> case kc of
        0x0d -> D.succeed commitMsg    -- return key
        0x1b -> D.succeed cancelMsg    -- escape key
        _ -> D.fail "non-shortuct-key"
      )
    )


field : String -> String -> Html.Html msg
field c t =
  Html.span
    [ Attr.classList [ ("field", True), (c, True) ] ]
    [ Html.text t ]

itemAttrs : ViewMode -> Bool -> Path -> Maybe Selection -> List (Html.Attribute Msg)
itemAttrs vm isDir path mSel =
  let
    ms = case mSel of
      Just sel ->
        if isPathSelected path sel then sel.mouseState else NoInteraction
      _ -> NoInteraction
    inDrag = dragging ms
    attrs = case ms of
      Dragging p0 pNow ->
        [ ]
      _ ->
        [ Events.onMouseDownExtended (MouseDownOn path isDir)
        , Events.onMouseUpExtended (MouseUpOn path isDir)
        ] ++ (if vm == ViewHover
          then
            [ Events.onMouseEnter (HoverMouseEnter path isDir)
            , Events.onMouseLeave (HoverMouseLeave path isDir)
            ]
          else []
          )
        ++ (if isDir
          then
            [ Events.onDragEnter <| DragEnter path
            , Events.onDragLeave <| DragLeave path
            ]
          else []
          )
  in
    Attr.classList
      [ ("item", True)
      , ("dragging", inDrag)
      , ("drop-items", isDir)
      ]
    :: attrs


depthClass : Path -> String
depthClass p = case Path.pathDepth p of
  0 -> "depth-zero"
  n -> if modBy 2 n == 0 then "depth-even" else "depth-odd"

dirFilesView : ViewMode -> Maybe Selection -> List Drive.Entry -> List (Html.Html Msg)
dirFilesView vm sel entries =
  let
    (_, files) = List.partition Drive.isDirEntry entries
    eView e = entryView vm (pathRelevantSelection e.path sel) e
  in
    if files == []
      then [ ]
      else [ Html.ul [ Attr.class "files" ] <| List.map eView files ]

dirView : ViewMode -> Maybe Selection -> List Drive.Entry -> List (Html.Html Msg)
dirView vm sel entries =
  let
    (subDirs, files) = List.partition Drive.isDirEntry entries
    eView e = entryView vm (pathRelevantSelection e.path sel) e
    subList cls es =
      if es == []
        then [ ]
        else [ Html.ul [ Attr.class cls ] <| List.map eView es ]
    fileList =
      if vm == ViewAll
        then [ Html.div [ Attr.class "file-list" ] <| subList "files" files ]
        else []
  in
    fileList ++ subList "dirs" subDirs

entryView : ViewMode -> Maybe Selection -> Drive.Entry -> Html.Html Msg
entryView = Lazy.lazy3 <| \vm sel entry ->
  let
    (cls, itemView) = case entry.item of
      Drive.D entries        -> ("dir", dirView vm sel entries)
      Drive.F { size, hash } -> ("file", [])
      Drive.X raw            -> ("raw", [field "rawEntry"
          <| String.cons raw.type_ ": " ++ raw.name
        ])
    input = case sel |> Maybe.andThen (pathPendingEdit entry.path) of
      Just pe ->
        [ Html.div [ Attr.class "input-group input-group-sm" ]
          [ Html.input
            [ Attr.id renameInputId
            , Attr.class "form-control"
            , Attr.value pe
            , Events.onInput EditSelected
            , onEditShortcutKeys CommitRename CancelRename
            ]
            []
          , Html.div [ Attr.class "input-group-append input-group-btn" ]
            [ Html.button
              [ Attr.class "btn btn-outline-secondary"
              , Events.onClick CommitRename
              ]
              [ Html.text "OK" ]
            , Html.button
              [ Attr.class "btn btn-outline-secondary"
              , Events.onClick CancelRename
              ]
              [ Html.text "✖" ]
            ]
          ]
        ]
      Nothing -> []
    (toggle, itemView_) = if entry.path == Path.factoryPath
      then
        let
          t =
            Html.a
              [ Attr.class "field show-btn collapsed"
              , Attr.attribute "data-toggle" "collapse"
              , Attr.href "#factory-content"
              , Aria.role "button"
              , Aria.expanded False
              , Aria.controls "factory-content"
              ]
              [ Html.text "show/hide"]
          iv =
            Html.div
              [ Attr.class "collapse"
              , Attr.id "factory-content"]
              itemView
        in
          (t, [iv])
      else
        (Html.text "", itemView)
    withEntry f = Maybe.unwrap False (f entry.path) sel
  in
    Html.li
      [ Attr.classList
        [ (cls,                     True)
        , (depthClass entry.path,   True)
        , ("selected",              withEntry isPathSelected)
        , ("focused",               withEntry isPathFocused)
        , ("focus-parent",          withEntry isPathFocusParent)
        ]
      ]
      ( ( Html.div (itemAttrs vm (Drive.isDirEntry entry) entry.path sel)
            [ field "name" entry.name
            , toggle
            , field "locked" (if entry.locked then "🔒" else "")
            , field "itemSize" (Util.siString entry.itemSize)
            ]
        )
      :: (input ++ itemView_)
      )

draggableView :  Maybe Selection -> Html.Html msg
draggableView = Maybe.unwrap (Html.div [] []) <| \sel ->
  let
    box n p =
      Html.div
        [ Attr.class "item"
        , Attr.style "top"     (String.fromInt (n * 8) ++ "px")
        , Attr.style "left"    (String.fromInt (n * 5) ++ "px")
        , Attr.style "z-index" (String.fromInt (1000 - n))
        ]
        [ Html.text <| Path.baseName p ]
    draggable xy =
      Html.div
        [ Attr.id "draggable"
        , Attr.style "top"  (String.fromInt (xy.y + 10) ++ "px")
        , Attr.style "left" (String.fromInt (xy.x + 10) ++ "px")
        ]
        (List.indexedMap box
          <| List.take 20  -- too many is sluggish and looks silly
          <| Set.toList
          <| selectedPaths sel)
  in
    case sel.mouseState of
      Dragging _ xy -> draggable xy
      Moving -> draggable { x = 100, y = 65 }
      _ -> Html.div [] []


progressModal : Queue -> String -> Bool -> Maybe Pump -> Html.Html Msg
progressModal q title cancelable pump =
  let
    show = True
    n = max 1 q.started
    qPct = 100 * (n - 1) // (max 1 <| n + List.length q.actions)
    (verb, msg, twobar) = case q.inFlight of
      Just (QAListDir _ p) -> ("Scanning", Path.pathString p, False)
      Just (QACreateDir p) -> ("Creating", Path.pathString p, False)
      Just (QADeleteDir p) -> ("Deleting", Path.pathString p, False)
      Just (QADeleteFile p) -> ("Deleting", Path.pathString p, False)
      Just (QARenameItem p _) -> ("Moving", Path.pathString p, False)
      Just (QAReadFile p _) -> ("Receiving", Path.pathString p, True)
      Just (QAWriteFile p _ _) -> ("Sending", Path.pathString p, True)
      Nothing -> ("Working", "", False)

    pPct = Maybe.unwrap 0 (\p -> floor (100 * pumpProgress p)) pump

    bar pct =
      Html.div [ Attr.class "progress" ]
        [ Html.div
          [ Attr.class "progress-bar progress-bar-striped progress-bar-animated bg-warning"
          , Attr.style "width" (String.fromInt pct ++ "%")
          -- aria-valuenow="50" aria-valuemin="0" aria-valuemax="100"
          ]
          [ ]
        ]

    header =
      Html.div [ Attr.class "modal-header" ]
        [ Html.h5 [ Attr.class "modal-title" ]
          [ Html.text title ]
        ]
    body =
      Html.div [ Attr.class "modal-body" ]
        ([ Html.p []
          [ Html.text (verb ++ " ")
          , Html.span [ Attr.class "name" ] [ Html.text msg ]
          , Html.text " ..."
          ]
        , bar qPct
        ]
        ++ if twobar then [ bar pPct ] else []
        )
    footer = if cancelable
      then
        [ Html.div [ Attr.class "modal-footer" ]
          [ Html.button
            [ Attr.class "btn btn-secondary"
            , Attr.type_ "button"
            , Events.onClick CancelQueue
            ]
            [ Html.text "Cancel" ]
          ]
        ]
      else [ ]
    content = header :: body :: footer
  in
    Html.div []
      [ Html.div
        [ Attr.classList
          [ ("modal", True), ("fade", True), ("show", show) ]
        , Aria.role "dialog"
        , Aria.hidden (not show)
        , Attr.tabindex -1
        ]
        [ Html.div [ Attr.class "modal-dialog modal-lg", Aria.role "document" ]
          [ Html.div [ Attr.class "modal-content" ] content ]
        ]
      , Html.div
        [ Attr.classList
          [ ("modal-backdrop", show), ("fade", True), ("show", show) ]
        ]
        []
      ]


view : Model -> Html.Html Msg
view model =
  let
    entry0 = Drive.driveEntry model.drive
    driveView = if Drive.isEmptyDrive model.drive
      then
        Html.button
        [ Attr.class "btn btn-primary btn-lg"
        , Attr.type_ "button"
        , Events.onClick KickOff
        ]
        [ Html.text "Scan +Drive" ]
      else
        Html.div [ Attr.class "dir-list drive-list" ]
          [ Html.ul [ Attr.class "drive"]
            [ entryView model.viewMode model.selection entry0 ]
          ]

    filesView = case model.selection of
      Just sel ->
        let
          parentPath = focusedDirPath sel
          entries =
            Drive.getEntry parentPath model.drive
            |> Maybe.andThen Drive.subEntries
          list =
            Maybe.unwrap [] (dirFilesView model.viewMode model.selection) entries
        in
          if List.isEmpty list
            then []
            else
              [ Html.div
                [ Attr.class "file-list drive-list"
                --, onDragEnter (DragEnter parentPath)
                --, onDragLeave (DragLeave parentPath)
                ]
                list
              ]
      _ -> []


    progressSmall = case model.queue of
      Nothing -> Html.span [] []
      Just q -> case q.modal of
        Just (title, cancelable) -> Html.span [] []
        Nothing ->
            Html.div [ Attr.class "progress", Attr.id "progress-small" ]
              [ Html.div
                [ Attr.class "progress-bar progress-bar-striped progress-bar-animated bg-warning"
                , Attr.style "width" "100%"
                ]
                [ ]
              ]

    progressBig = case model.queue of
      Nothing -> Html.div [] []
      Just q -> case q.modal of
        Just (title, cancelable) -> progressModal q title cancelable model.pump
        Nothing -> Html.div [] []


    viewButton title mode =
      Html.button
        [ Attr.classList
          [ ("btn", True)
          , ("btn-light", True)
          , ("btn-sm", True)
          , ("active", model.viewMode == mode)
          ]
        , Attr.type_ "button"
        , Events.onClick (SetView mode)
        ]
        [ Html.text title ]

    viewToolBar =
      Html.div [ Attr.class "btn-toolbar" ]
        [ Html.div
          [ Attr.class "btn-group btn-group-sm"
          , Attr.title "Sample List View"
          , Attr.attribute "data-toggle" "tooltip"
          , Attr.attribute "data-placement" "right"
          ]
          [ viewButton "hover" ViewHover
          , viewButton "selected" ViewSelected
          , viewButton "all" ViewAll
          ]
        ]

  in
    Html.div
      [ Attr.id "drive"
      , Attr.classList
        [ ("all",       model.viewMode == ViewAll)
        , ("selected",  model.viewMode == ViewSelected)
        , ("hover",     model.viewMode == ViewHover)
        , ("drag",
          Maybe.unwrap False (dragging << .mouseState) model.selection)
        , ("drop",      Maybe.isJust model.inDrop)
        , ("section",   True)
        ]
      , Attr.tabindex 1
      ]
      [ Html.map OnAlert <| Alert.view model.alertModel
      , progressBig
      , Html.div
        [ Attr.class "section-header" ]
        [ Html.h3 [ ] [ Html.text "+Drive" ]
        , viewToolBar
        ]
      , Html.div [ Attr.class "row" ]
        [ Html.div [ Attr.class "col" ]
          [ driveView
          ]
        , Html.div [ Attr.class "col" ]
          ( progressSmall
          :: filesView
          )
        ]
      , draggableView model.selection
      ]

