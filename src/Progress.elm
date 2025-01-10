module Progress exposing
  ( Progress
  , start
  , update
  , updateMessage
  , finish
  , init

  , modalView
  )

{-| A simple progress bar component.

To use this, first include the model in your model:

    type alias Model =
      { ...
      , progress : Progress.Progress
      ...
      }

Add it into your view:

    view : Model -> Html.Html Msg
    view model =
      let
        content = [ ... ]

      progressBar = Progress.modalView model.progress Nothing

      in
        Html.div [ ] (progressBar :: content)

To start the progress bar:

    { model | progress = Progress.start "Swizzling items" }

To update the progress bar:

    { model | progress = Progress.update amt_done total_size model.progress }

To close the progress bar:

    { model | progress = Progress.finish model.progress }
-}


import Html
import Html.Attributes as Attr
import Html.Events as Events

import Html.Aria as Aria


type Progress
  = Progress
    { title : String
    , message : Maybe String
    , done : Int
    , todo : Int
    , complete : Bool
    }

start : String -> Progress
start title =
  Progress
    { title = title
    , message = Nothing
    , done = 0
    , todo = 0
    , complete = False
    }

update : (Int, Int) -> Progress -> Progress
update (i, n) (Progress p) = Progress { p | done = i, todo = n }

updateMessage : String -> (Int, Int) -> Progress -> Progress
updateMessage msg (i, n) (Progress p) =
  Progress { p | message = Just msg, done = i, todo = n }

finish : Progress -> Progress
finish (Progress p) = Progress { p | complete = True }

init : Progress
init = start "" |> finish



bar : Int -> Int -> Html.Html msg
bar num denom =
  let
    determinate = denom > 0

    pctInt =
      if determinate
        then 100 * num // denom
        else 0
    aria =
      if determinate
        then Aria.values 0 100 pctInt
        else []
  in
    Html.div [ Attr.class "progress" ]
      [ Html.div
        ( [ Attr.class "progress-bar bg-warning"
          , Attr.class "progress-bar-striped progress-bar-animated"
          , Attr.style "width" (String.fromInt pctInt ++ "%")
          ]
        ++ aria
        )
        [ ]
      ]


message : String -> Html.Html msg
message text =
  Html.p []
    [ Html.text text
    , Html.text " ..."
    ]

--actionMessage : String -> String -> Html.Html msg
--actionMessage verb item =
--  Html.p []
--    [ Html.text (verb ++ " ")
--    , Html.span [ Attr.class "name" ] [ Html.text item ]
--    , Html.text " ..."
--    ]


modalView : Progress -> Maybe msg -> Html.Html msg
modalView (Progress p) mCancelMsg =
  let
    show = not p.complete

    header =
      Html.div [ Attr.class "modal-header" ]
        [ Html.h5 [ Attr.class "modal-title" ]
          [ Html.text p.title ]
        ]
    body =
      Html.div [ Attr.class "modal-body" ]
        <| List.filterMap identity
          [ Maybe.map message p.message
          , Just (bar p.done p.todo )
          ]
    footer =
      case mCancelMsg of
        Just cancelMsg ->
          [ Html.div [ Attr.class "modal-footer" ]
            [ Html.button
              [ Attr.class "btn btn-secondary"
              , Attr.type_ "button"
              , Events.onClick cancelMsg
              ]
              [ Html.text "Cancel" ]
            ]
          ]
        Nothing -> [ ]

    backdrop =
      if show
        then [ Html.div [ Attr.class "modal-backdrop fade show" ] [] ]
        else []

    content = header :: body :: footer
  in
    Html.div []
      <|
        [ Html.div
          [ Attr.class "modal fade"
          , Attr.classList [("show", show)]
          , Aria.role "dialog"
          , Aria.hidden (not show)
          , Attr.tabindex -1
          ]
          [ Html.div [ Attr.class "modal-dialog modal-lg", Aria.role "document" ]
            [ Html.div [ Attr.class "modal-content" ] content ]
          ]
        ]
        ++ backdrop
