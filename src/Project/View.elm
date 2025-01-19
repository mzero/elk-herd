module Project.View exposing
  ( commands
  , view
  )

import Html
import Html.Attributes as Attr
import Html.Events as Events

import Alert
import Bank exposing (BankOf, Index(..))
import Commands as C
import Elektron.Digitakt.HighLevel as DT
import Elektron.Digitakt.Types as DT
import Html.Aria as Aria
import Missing.Html.Events as Events
import Missing.Maybe as Maybe
import Progress
import Project.Base exposing (..)
import Project.Import as Import
import Project.Selection.Project as Sel
import Project.Util exposing (..)
import Undo
import Elektron.Digitakt.Types exposing (Sample)


commands : Model -> C.Commands Msg
commands model =
  [ C.Group "Device Commands"
    [ C.Button "cmd-proj-dev-fetch"  "Fetch Project"  (RequestProject LoadProject)
    , C.Button "cmd-proj-dev-send"   "Send Project"    SendProject
    , C.Button "cmd-proj-dev-import" "Import Project" (RequestProject StartImport)
    ]
  , C.Group "Project Files"
    [ C.LoadFile "cmd-proj-file-open"   "Open File"   (SelectProjectFile LoadProject)
      ".syx" False
    , C.Button   "cmd-proj-file-save"   "Save File"    StartWriteProjectFile
    , C.LoadFile "cmd-proj-file-import" "Import File" (SelectProjectFile StartImport)
      ".syx" False
    ]
  , C.Group "Project Commands"
    [ C.Button "cmd-proj-clear" "Clear Project" ClearProject
    ]
  , C.Group "Undo"
    [ C.Subpanel <| Html.map UndoMsg <| Undo.view model.undoStack ]
  ]


projectNameDialog : Model -> Html.Html Msg
projectNameDialog model =
  Html.div []
  <| if model.aboutToWriteFile
    then
      [ Html.div
          [ Attr.class "modal fade show"
          , Attr.attribute "role" "dialog"
          , Attr.attribute "tabindex" "-1"
          ]
          [ Html.div [ Attr.class "modal-dialog", Attr.attribute "role" "document" ]
            [ Html.div [ Attr.class "modal-content" ]
              [ Html.div [ Attr.class "modal-header" ]
                [ Html.h5 [ Attr.class "modal-title" ]
                    [ Html.text "Name for Project File" ]
                ]
              , Html.div [ Attr.class "modal-body" ]
                [ Html.div [ Attr.class "input-group" ]
                  [ Html.input
                    [ Aria.label "Project file name"
                    , Attr.class "form-control"
                    , Attr.value model.projectName
                    , Attr.type_ "text"
                    , Events.onInput UpdateProjectName
                    ]
                    []
                  , Html.div [ Attr.class "input-group-append" ]
                    [ Html.span [ Attr.class "input-group-text" ]
                      [ Html.text ".syx" ]
                    ]
                  ]
                ]
              , Html.div [ Attr.class "modal-footer" ]
                [ Html.button
                  [ Attr.class "btn btn-secondary"
                  , Attr.type_ "button"
                  , Events.onClick CancelWriteProjectFile ]
                  [ Html.text "Cancel" ]
                , Html.button
                  [ Attr.class "btn btn-primary"
                  , Attr.type_ "button"
                  , Events.onClick WriteProjectFile ]
                  [ Html.text "Write File" ]
                ]
              ]
            ]
          ]
        , Html.div [ Attr.class "modal-backdrop fade show"] [ ]
        ]
    else
      []


sectionToolBar : String -> Kind -> Bool -> Sel.Selection -> Html.Html Msg
sectionToolBar label k editing sel =
  let
    isSel = Sel.kindStatus k sel == Sel.Selected
    isRel = Sel.kindStatus k sel == Sel.Related

    button btn =
      Just
      <| Html.button
        [ Attr.class "btn btn-light btn-sm"
        , Attr.classList [ ("active", btn.active) ]
        , Attr.type_ "button"
        , Attr.title btn.title
        , Attr.attribute "data-toggle" "tooltip"
        , Attr.attribute "data-placement" "right"
        , Attr.disabled (not btn.enable)
        , Events.onClick (btn.msg k)
        ]
        [ Html.text btn.text ]

    buttonIfNot kSkip btn =
      if k == kSkip
        then Nothing
        else button btn
    group grp =
      case List.filterMap identity grp of
        [] -> Nothing
        btns ->
          Just
          <| Html.div
            [ Attr.class "btn-group btn-group-sm"
            , Aria.role "group"
            ]
            btns
  in
    Html.div
      [ Attr.class "btn-toolbar section-commands"
      , Aria.role "toolbar"
      , Aria.label label
      ]
      <| List.filterMap group
        [ [ button
              { title = "Select Related"
              , text = "\u{21E2}"  -- arrow
              , active = False
              , enable = isRel
              , msg = SelectRelated
              }
          , buttonIfNot KPattern
              { title = "Select Unused"
              , text = "\u{2205}"  -- empty set
              , active = False
              , enable = True
              , msg = SelectUnused
              }
          ]
        , [ buttonIfNot KSample
              { title = "Rename"
              , text = "\u{270E}"  -- pencil
              , active = isSel && editing
              , enable = isSel
              , msg = RenameItem
              }
          ]
        , [ buttonIfNot KPattern
              { title = "Compact Items"
              , text = "\u{2AE9}"  -- tack thing
              , active = False
              , enable = True
              , msg = CompactItems
              }
          ]
        , [ button
              { title = "Delete Selected Items"
              , text = "\u{00D7}"  -- multiply
              , active = False
              , enable = isSel
              , msg = DeleteItems
              }
          ]
        ]

bankSelector : Model -> Kind -> Html.Html Msg
bankSelector model k =
  let
    sampleBankButton n =
      let
        start = n * 128
        end = start + 127
        range = List.range start end
        itemEmpty i = Bank.get (Index i) model.project.samplePool |> Maybe.unwrap True DT.isEmptyItem
        itemStatus i = Sel.itemStatus k i model.selection model.related
        empty = List.all itemEmpty range
        status = List.foldl (\i b -> if b == Sel.Plain then itemStatus i else b) Sel.Plain range

        onDragEvents =
          if Sel.kindStatus k model.selection == Sel.Dragged
            then [Events.onMouseEnter (SetSamplePoolOffset start)]
            else []
      in
        Html.button
          ([ Attr.class "btn btn-light btn-sm"
          , Attr.classList
              [ ("active", (model.samplePoolOffset == start))
              , ("empty", empty)
              , ("selected", status == Sel.Selected)
              , ("dragged", status == Sel.Dragged)
              , ("related", status == Sel.Related)
              ]
          , Attr.type_ "button"
          , Attr.disabled False
          , Events.onClick (SetSamplePoolOffset start)
          ]
          ++ onDragEvents
          )
          [ Html.text <| String.slice n (n + 1) "ABCDEFGHI" ]
  in
  case k of
    KSample ->
      if model.projectSpec.numSampleSlots > 128
        then
          Html.div
            [ Attr.class "btn-toolbar section-selector"
            , Aria.role "toolbar"
            , Attr.title "Bank Selector"
            , Attr.attribute "data-toggle" "tooltip"
            , Attr.attribute "data-placement" "right"
            ]
            ( List.map sampleBankButton <| List.range 0 7 )
        else Html.text ""
    _ -> Html.text ""


view : Model -> Html.Html Msg
view model =
  let
    itemView : Kind -> Int -> Maybe (Index (DT.BankItem a)) -> Maybe (DT.BankItem a) -> Html.Html Msg
    itemView k i mSrc mItem =
      let
        name = Maybe.unwrap "" .name mItem
        empty = Maybe.unwrap True DT.isEmptyItem mItem
        phantom = Maybe.unwrap False DT.isPhantomItem mItem
        status =
          case mSrc of
            Nothing -> Sel.Plain
            Just (Index j) -> Sel.itemStatus k j model.selection model.related
        doEdit = model.nameEditing && status == Sel.Selected && k /= KSample
      in
        Html.div
          ( [ Attr.id (itemId k i)
            , Attr.class "bank-item"
            , Attr.classList
              [ ("empty", empty)
              , ("zero", modBy 128 i == 0)
              , ("phantom", phantom)
              , ("selected", status == Sel.Selected)
              , ("dragged", status == Sel.Dragged)
              , ("related", status == Sel.Related)
              ]
            , Attr.tabindex -1
            ]
            ++ List.map
                (Attr.map (SelectionItemMsg k i))
                (Sel.itemHandlers k i empty)
          )
          <| if doEdit
            then
              [ Html.input
                [ Attr.id (itemInputId k i)
                , Attr.class "bank-input digitakt-charset"
                , Attr.type_ "text"
                , Attr.value name
                , Events.onInput (ChangeItemName k i)
                ]
                [ ]
              ]
            else
              [ Html.span [ Attr.class "bank-label" ] [ Html.text (slotLabel k i) ]
              , Html.span
                [ Attr.class "bank-name" ]
                [ Html.text name ]
              ]

    dragBox : Maybe Sel.DragInfo -> BankOf (DT.BankItem a) -> List (Html.Html Msg)
    dragBox mDragInfo bank =
      let
        dragItem n i =
          let
            name = Maybe.unwrap "" .name (Bank.get (Index i) bank)
          in
            Html.div
            [ Attr.class "bank-item"
            , Attr.style "top"     (String.fromInt (n * 8) ++ "px")
            , Attr.style "left"    (String.fromInt (n * 5) ++ "px")
            , Attr.style "z-index" (String.fromInt (2000 - n))
            ]
            [ Html.span
              [ Attr.class "bank-name" ]
              [ Html.text name ]
            ]
      in
        case mDragInfo of
          Just { pos, srcs } -> List.singleton <|
            Html.div
              [ Attr.class "draggable"
              , Attr.style "top"  (String.fromInt (pos.y + 10) ++ "px")
              , Attr.style "left" (String.fromInt (pos.x + 10) ++ "px")
              ]
              ( List.indexedMap dragItem
                <| List.take 20 srcs    -- too many is sluggish and looks silly
              )
          Nothing -> []

    bankView : String -> String -> Kind -> BankOf (DT.BankItem a) -> Int -> Html.Html Msg
    bankView id label k bank offset =
      let
        status = Sel.kindStatus k model.selection
        mDragInfo = Sel.dragInfo k model.selection

        srcFn =
          if status == Sel.Dragged
            then
              case mDragInfo of
                Just di ->
                  case di.dst of
                    Just dst ->
                      let
                        srcs = List.map Index di.srcs
                        shuffle = Bank.dragAndDrop DT.isEmptyItem srcs (Index dst) bank
                      in
                        (\idx -> Bank.source shuffle idx)
                    Nothing -> Just
                Nothing -> Just
            else
              Just

        column c =
          Html.div
            [ Attr.class "bank-column" ]
            <| List.map (\r -> item (c * 16 + r + offset)) <| List.range 0 15
        columns = List.map column <| List.range 0 7
        item i =
          let
            mSrc = srcFn (Index i)
            mItem = mSrc |> Maybe.andThen (\s -> Bank.get s bank)
          in
            itemView k i mSrc mItem
      in
        Html.div
          [ Attr.class "section"
          , Attr.class id
          ]
          [ Html.div
              [ Attr.class "section-header" ]
              [ Html.h3 [] [ Html.text label ]
              , bankSelector model k
              , sectionToolBar (label ++ " Toolbar") k model.nameEditing model.selection
              ]
          , Html.div
            [ Attr.id (bankId k)
            , Attr.class "bank"
            , Attr.classList
              [ ("selected", status == Sel.Selected)
              , ("dragged", status == Sel.Dragged)
              , ("related", status == Sel.Related)
              ]
            , Attr.tabindex -1
            , Events.onKeyDown (KeyDown k)
            ]
            (columns ++ dragBox mDragInfo bank)
          ]
    frontMatter =
      [ Html.div [ Attr.id "project-alert", Attr.tabindex -1 ]
        [ Html.map AlertMsg (Alert.view model.alert) ]
      , Progress.modalView model.progress Nothing
      , projectNameDialog model
      ]

    projectView =
      [ bankView "patterns" "Patterns"    KPattern model.project.patterns   0
      , bankView "samples"  "Sample Pool" KSample  model.project.samplePool model.samplePoolOffset
      , bankView "sounds"   "Sound Pool"  KSound   model.project.soundPool  0
      ]

    content =
      case model.importing of
        Just i -> [ Html.map ImportMsg <| Import.view i ]
        Nothing -> projectView
  in
    Html.div [ ] (frontMatter ++ content)

