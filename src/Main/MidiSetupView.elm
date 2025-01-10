module Main.MidiSetupView exposing
  ( view
  )

import Html exposing (div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Events
import Markdown

import Alert
import Main.Base exposing (..)
import Missing.Maybe as Maybe
import SysEx
import SysEx.Connect as Connect
import WebMidi


fromMarkdown : String -> Html.Html msg
fromMarkdown = Markdown.toHtml [ Attr.class "markdown" ]



helpText : Html.Html msg
helpText = fromMarkdown """
**Where's your instrument?**

  * USB connected instruments should be listed in the MIDI Devices section.
  * DIN MIDI port pairs will appear in the MIDI Devices section, too. Select
    one to probe for any instrument connected to it.
  * Uncommon situations may require you explicitly pick _both_ a MIDI In port
    and a MIDI Out port.
  * Have two of the same instrument? **Lucky dog!**
    * But, if both are connected by USB, the ports all end up with the same
      name, so you'll have to pick the in and out ports yourself.
    * Or you can just turn off or disconnect one.
    * OS X users can use Audio MIDI Setup to rename one, but you'll need to
      restart your browser after.

**Not Working?** Check to see if your instrument appears in other apps:

  * Once you see the instrument in other music apps..
  * If still not showing up here, quit and restart the browser.
  * **Firefox users:** Firefox only shows MIDI devices that are still connected
    from when the browser started. Restarting Firefox usually fixes things.
"""

instrumentName : Model -> String
instrumentName model =
  Maybe.unwrap "Instrument" .deviceName <| Connect.instrument model.connectModel

view : Model -> Html.Html Msg
view model =
  div [ class "container-fluid" ]
    [ div [ class "row" ]
      [ div [ class "col" ]
        [ div [ class "jumbotron m-5", Attr.id "midi-setup" ]
          [ Html.div [ class "screen-nav" ]
            [ Html.button
              [ class "btn btn-outline-primary btn-sm"
              , Events.onClick (GoToScreen SettingsScreen)
              ]
              [ text "< Settings" ]
            ]
          , Html.h1 [ class "display-3" ] [ text "MIDI Setup" ]
          , Html.p [ ] [ text "Choose your instrument's MIDI connection:" ]

          , (Html.map OnAlert <| Alert.view model.alertModel)

          , Html.div [ Attr.class "row" ]
            [ Html.div [ Attr.class "col-4"]
              [ Html.map OnWebMidi <| WebMidi.view model.webMidiModel
              ]
            , Html.div [ Attr.class "col"]
              [ Html.button
                [ class "btn btn-primary btn-lg mt-3"
                , Attr.disabled (not <| Connect.isReady model.connectModel)
                , Events.onClick (GoToScreen (MainScreen SamplesPage))
                ]
                [ text <| "Manage Your " ++ instrumentName model ]
              , Html.map OnSysExConnect <| Connect.view model.connectModel
              , Html.hr [ class "my-4" ] []
              , helpText
              ]
            ]
          ]
        ]
      ]
    , Html.map OnSysEx <| SysEx.view model.sysExModel
    ]
