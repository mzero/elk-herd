port module Help exposing
  ( main
  )

{-| Implments  a simple help app with a table of contents on the side,
and articles in the main area.
-}

import Browser

import Html exposing (div)
import Html.Attributes as Attr exposing (class)
import Html.Keyed

import Help.Contents exposing (..)
import Html.Aria as Aria
import Main.View


type alias Model = Article Msg

type Msg
  = ShowSection String


idFromHash : String -> String
idFromHash hash =
  if String.startsWith "#" hash
    then String.dropLeft 1 hash
    else hash

articleFromHash : String -> Article msg
articleFromHash hash =
  idFromHash hash |> findArticle |> Maybe.withDefault helpStart

init : String -> (Model, Cmd Msg)
init hash =
    ( articleFromHash hash, Cmd.none )

tocId : Article msg -> String
tocId art = "toc-" ++  art.id


helpIndex : Article Msg -> Html.Html Msg
helpIndex currentArt =
  let
    helpItem h =
      case h of
        Entry art ->
          let
            open = currentArt.id == art.id
          in
            Html.li []
              [ Html.a
                  [ Attr.id (tocId art)
                  , Attr.classList [ ( "collapsed", not open) ]
                  --, Attr.attribute "data-toggle" "collapse"
                  , Attr.href ("#" ++ art.id)
                  , Aria.expanded open
                  , Aria.controls art.id
                  ]
                  [ Html.text art.title ]
              ]
        Section title contents ->
          Html.li []
            [ Html.text title
            , helpList contents
            ]

    helpList items =
       Html.ul [ ]
          <| List.map helpItem items

  in
    Html.div [ Attr.class "card", Attr.id "help-panel" ]
      [ Html.div [ Attr.class "card-body" ]
         [ helpList help ]
      ]

helpContent : Article Msg -> Html.Html Msg
helpContent currentArt =
  let
    helpItem h =
      case h of
        Entry art ->
          let
            open = currentArt.id == art.id
          in
            List.singleton
            <| Tuple.pair art.id
            <| Html.div
              [ Attr.class "help-section markdown"
              , Attr.classList [ ("show", open) ]
              , Attr.id art.id
              --, Attr.attribute "data-parent" "#help-accordian"
              , Aria.labeledBy (tocId art)
              ]
              [ Html.h2 [ ] [ Html.text art.title ]
              , art.body
              ]

        Section title contents ->
          helpList contents

    helpList items =
      List.concatMap helpItem items

  in
    Html.Keyed.node "div" [ Attr.id "help-accordian" ]
      <| helpList help


view : Model -> Browser.Document Msg
view article =
  { title = "crunch/elk-herd - Help: " ++ article.title
  , body =
    [ Main.View.titlebar
      (Html.h2 [ Attr.class "text-warning" ] [ Html.text "Help" ])
    , div [ Attr.id "main-view", class "container" ]
      [ div [ class "row" ]
        [ div [ class "col-3" ] [ helpIndex article ]
        , div [ class "col-9" ] [ helpContent article ]
        ]
      ]
    ]
  }

update : Msg -> Model -> (Model, Cmd none)
update msg model =
  case msg of
    ShowSection hash -> ( articleFromHash hash, Cmd.none )


port showSection : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ = showSection ShowSection


main : Program String Model Msg
main = Browser.document
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
