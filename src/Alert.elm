module Alert exposing
  ( Level(..)
  , Model
  , noAlert
  , alert
  , alertMarkdown

  , Msg
  , update
  , subscriptions
  , view
  )

{-| A simple alert box component.

To use this, first include the model in your model:

    type alias Model =
      { ...
      , alert : Alert.Model
      ...
      }

Then add a message wrapper, and add it to your update function:

    type Msg =
      ...
      | AlertMsg Alert.Msg
      ...

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
      case msg of
        ...
        AlertMsg amsg ->
          ({ model | alert = Alert.update alertMsg model.alert }, Cmd.none)
        ...

And finally, add it into your view:

    view : Model -> Html.Html Msg
    view model =
      let
        content = [ ... ]

      alertView =
        [ Html.div [ Attr.id "project-alert", Attr.tabindex -1 ]
          [ Html.map AlertMsg (Alert.view model.alert) ]
        ]

      in
        Html.div [ ] (alertView :: content)

To pop-up an alert, user `alert` or `alertMarkdown`

    if List.length itemsToSwizzle < 8
      then { model | alert = Alert.alert Alert.Info "Swizzle Failure"
                      "Need at least 8 items to swizzle!"
            }
      else ...
-}

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Markdown
import Time

import Html.Aria as Aria
import Missing.Time as Time


type Level = Success | Info | Warning | Danger | Fatal

type Model = NoAlert | Alert Level (Html.Html Msg)

noAlert : Model
noAlert = NoAlert

alertMarkdown : Level -> String -> Model
alertMarkdown lvl md = Alert lvl (Markdown.toHtml [ Attr.class "markdown" ] md)

alert : Level -> String -> String -> Model
alert lvl s1 s2 =
  let
    md = "## " ++ s1 ++ "\n\n" ++ s2
  in
    alertMarkdown lvl md

type Msg = Dismiss

update : Msg -> Model -> Model
update msg model = case msg of
  Dismiss -> NoAlert

subscriptions : Model -> Sub Msg
subscriptions model = case model of
  NoAlert -> Sub.none
  Alert Info _ -> Sub.none
  Alert Fatal _ -> Sub.none
  Alert _ _ -> Time.every (60 * Time.second) (always Dismiss)

view : Model -> Html.Html Msg
view model =
  let
    (show, level, body) = case model of
      NoAlert -> (False, Success, Html.text "")
      Alert l md -> (True, l, md)
  in
    Html.div
      [ Attr.classList
        [ ("alert",              True)
        , ("alert-dismissible",  True)
        , ("fade",               True)
        , ("show",               show)
        , ("zero",               not show)
        , ("alert-success",      level == Success)
        , ("alert-info",         level == Info)
        , ("alert-warning",      level == Warning)
        , ("alert-danger",       level == Danger || level == Fatal)
        ]
      , Aria.role "alert"
      ]
      [ Html.button
        [ Attr.type_ "button"
        , Attr.class "close"
        , Attr.classList [ ("d-none", not show) ]
        , Attr.attribute "data-dismis" "alert"
        , Attr.hidden (level == Fatal)
        , Events.onClick Dismiss
        , Aria.label "Close"
        ]
        [ Html.span [ Aria.hidden True ]
          [ Html.text "×"  -- times symbol
          ]
        ]
      , body
      ]
