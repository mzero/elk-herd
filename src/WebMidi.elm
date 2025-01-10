module WebMidi exposing
    ( Model
    , init
    , ready
    , openPortsDiffer
    , portsString

    , Msg
    , update
    , subscriptions
    , view

    , genUnexpectedMessageError
    , midiErrorMessage
    )

{-| Displays the available ports in a useful way, and lets the user pick
which ports to use.
-}

import Dict
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Regex

import Missing.List as List
import Missing.Maybe as Maybe
import Missing.Regex as Regex
import Portage exposing (MidiPort)

{-
  Port names come in many forms:

    Elektron <dev> <dev> <in/out> 1       -- OS X, Android
    <user-dev> <dev> <in/out> 1           -- OS X
    Elektron <dev>                        -- Windows, Android
    Elektron <dev> MIDI 1                 -- Linux
    <user-dev>                            -- all
    <dev> Elektron MIDI                   -- Overbridge OS X

    <dev> : name of device: "Digitakt", "Analog Rytm MKII", etc...
    <in/out> : either "in" or "out"
    <user-dev> : if the user has renamed the device in Audio MIDI Setup
-}

portNameTrailer : Regex.Regex
portNameTrailer = Regex.regex "( (in|out|MIDI)( 1|))$"
  -- remove the " in|out|MIDI 1" trailer; match 1 explicitly incase some user
  -- port and multiple, so we don't end make making them all the same

portNameDeDup : Regex.Regex
portNameDeDup = Regex.regex "( .{6,}?)(\\1)$"
  -- remove any large repeated section at the end, probably the device name
  -- listed twice

simplifyPortName : String -> String
simplifyPortName =
  let
     trim = Regex.replaceAtMost 1 portNameTrailer (\_ -> "")
     dedup = Regex.replaceAtMost 1 portNameDeDup
      (\{match, submatches} -> case submatches of
        Just m :: _ -> m
        _ -> match  -- should never happen: if it matches, it has submatches
      )
  in
    trim >> dedup


type alias Model =
  { inPorts : List (String, MidiPort)
  , outPorts : List (String, MidiPort)
  , pairPorts : List (String, (MidiPort, MidiPort))
  , inOpen : Maybe MidiPort
  , outOpen : Maybe MidiPort
  }

init : Model
init = Model [] [] [] Nothing Nothing

buildModel : List MidiPort -> List MidiPort -> Model
buildModel allInPorts allOutPorts =
  let
    prepPorts = List.sortBy Tuple.first
      << List.map (\p -> (simplifyPortName p.name, p))
    prepOpen = List.toMaybe << List.filter ((==) "open" << .connection)

    partitionOne : a -> (List (a, b)) -> Maybe (b, List (a, b))
    partitionOne a abs = case List.partition (\(x, y) -> x == a) abs
      of
        ([(_, b)], abs_) -> Just (b, abs_)
        _ -> Nothing

    pair n ((ins, outs, pairs) as s) =
      case (partitionOne n ins, partitionOne n outs) of
        (Just (pIn, ins_), Just (pOut, outs_)) ->
          (ins_, outs_, pairs ++ [(n, (pIn, pOut))])
        _ -> s

    allIns = prepPorts allInPorts
    allOuts = prepPorts allOutPorts

    (fliterdIns, filteredOuts, foundPairs) =
      List.foldl pair (allIns, allOuts, []) <| List.map Tuple.first allIns

  in
    { inPorts = fliterdIns
    , outPorts = filteredOuts
    , pairPorts = foundPairs
    , inOpen = (prepOpen allInPorts)
    , outOpen = (prepOpen allOutPorts)
    }


ready : Model -> Bool
ready model = model.inOpen /= Nothing && model.outOpen /= Nothing

openPortsDiffer : Model -> Model -> Bool
openPortsDiffer ma mb = ma.inOpen /= mb.inOpen || ma.outOpen /= mb.outOpen

portsString : Model -> String
portsString model =
  let
    openPortName = Maybe.unwrap "--none--" .name
  in
    openPortName model.inOpen ++ "," ++ openPortName model.outOpen



type Msg
  = PortUpdate (List MidiPort, List MidiPort)
  | SelectInPort String
  | SelectOutPort String
  | SelectPairPorts String String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    prepPorts = Dict.fromList << List.map (\p -> (simplifyPortName p.name, p))
    prepOpen = List.toMaybe << List.filter ((==) "open" << .connection)
  in
    case msg of
      PortUpdate (newInPorts, newOutPorts) ->
        ( buildModel newInPorts newOutPorts
        , Cmd.none
        )

      SelectInPort id -> (model, Portage.selectMidiIn <| Just id)
      SelectOutPort id -> (model, Portage.selectMidiOut <| Just id)
      SelectPairPorts inId outId ->
        ( model
        , Cmd.batch
          [ Portage.selectMidiIn (Just inId)
          , Portage.selectMidiOut (Just outId)
          ]
        )

subscriptions : Model -> Sub Msg
subscriptions model = Portage.midiAccess PortUpdate





viewSelect :
  String -> String -> (a -> Msg) -> (a -> Bool)
  -> List (String, a)
  -> Html.Html Msg
viewSelect id name msgFn checkFn items =
  let
    label = Html.h4 [ Attr.class "col-form-label h5 text-danger" ]
      [ Html.text name ]

    option (n, item) =
       Html.div [ Attr.class "form-check" ]
        [ Html.label [ Attr.class "form-check-label" ]
          [ Html.input
            [ Attr.class "form-check-input"
            , Attr.type_ "radio"
            , Attr.name id
            , Attr.checked (checkFn item)
            , Events.onClick (msgFn item)
            ]
            [ ]
          , Html.text (" " ++ n)
          ]
        ]

    noOption =
       Html.div [ Attr.class "form-check disabled" ]
        [ Html.label [ Attr.class "form-check-label" ]
          [ Html.input
            [ Attr.class "form-check-input"
            , Attr.type_ "radio"
            , Attr.name id
            , Attr.disabled True
            ]
            [ ]
          , Html.text " -- no ports --"
          ]
        ]

    options = case items of
      [] -> [noOption]
      _ -> List.map option items
  in
    Html.div [ Attr.class "form-group" ]
      <| label :: options


checkOne : Maybe MidiPort -> MidiPort -> Bool
checkOne mSel = case mSel of
  Just sel -> \p -> p.id == sel.id
  Nothing -> \_ -> False

checkTwo : Maybe MidiPort -> Maybe MidiPort -> (MidiPort, MidiPort) -> Bool
checkTwo mInSel mOutSel =
  let
    checkIn = checkOne mInSel
    checkOut = checkOne mOutSel
  in
    \(pIn, pOut) -> checkIn pIn && checkOut pOut

viewInSelect : Model -> Html.Html Msg
viewInSelect model =
  viewSelect "in-ports-radio" "MIDI In Ports"
    (.id >> SelectInPort) (checkOne model.inOpen)
    model.inPorts

viewOutSelect : Model -> Html.Html Msg
viewOutSelect model =
  viewSelect "out-ports-radio" "MIDI Out Ports"
  (.id >> SelectOutPort) (checkOne model.outOpen)
  model.outPorts

viewPairSelect : Model -> Html.Html Msg
viewPairSelect model =
  viewSelect "pair-ports-radio" "MIDI Devices"
    (\(pIn, pOut) -> SelectPairPorts pIn.id pOut.id)
    (checkTwo model.inOpen model.outOpen)
    model.pairPorts

view : Model -> Html.Html Msg
view model =
  Html.div [ ]
    [ viewPairSelect model
    , Html.hr [ Attr.class "my-4" ] []
    , viewInSelect model
    , viewOutSelect model
    ]

genUnexpectedMessageError : Cmd msg
genUnexpectedMessageError =
  Portage.genMidiError ("ElektronUnexpectedResponse", "")

midiErrorMessage : (String, String) -> String
midiErrorMessage (name, message) = case name of
  "SecurityError" -> """
MIDI device access is disabled for this site.

You need to re-enable it for this this page to be able to reach your device.

* Click the little keyboard icon in the right side of the browser's
address bar above to clear the setting.
* Then reload this page.
"""

  "NoWebMidi" -> """
This browser doesn't support the WebMIDI standard. Try using Chrome, which does:
[https://www.google.com/chrome/](https://www.google.com/chrome/)
"""

  "ElektronUnexpectedResponse" -> """
Looks like another application is trying to use your instrument, too.

* Close Elektron **Transfer** and **C6** applications, if they are running.
* Close other software that might be exchanging SysEx messages with the instrument such as a DAW.

Then click the **Reload** button, or reload the page.

----

If that still doesn't work, the instrument might be responding to SysEx in an
unexpected way.

* Power cycle the instrument, and wait for it finish any project to loading.
"""

  _ -> message ++ "\n\n_(" ++ name ++ ")_\n"
