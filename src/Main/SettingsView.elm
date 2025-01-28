module Main.SettingsView exposing
  ( view
  )

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Markdown

import Alert
import Build
import Main.Base exposing (..)
import SysEx
import Html.Aria as Aria


fromMarkdown : String -> Html.Html msg
fromMarkdown = Markdown.toHtml [ Attr.class "markdown" ]




plainTitle : String
plainTitle = "Yo!"

betaTitle : String
betaTitle = "Hey, brave Elektronaut:"

title : String
title = if Build.beta then betaTitle else plainTitle


plainIntroduction : Html.Html msg
plainIntroduction = fromMarkdown """
I know you want to get right managing the content on your herd of Elektron
instruments, but listen up:

* This was created by me, [mzero](https://www.elektronauts.com/u/mzero/),
  not Elektron.

* If things go wonky, help me debug it - don't yell at me. This is just
  me in my spare time.

* Your stuff is private: Even though this is a web app, all your
  music, patterns, and samples never leave your computer.

**Enjoy. Have fun. Make music. Buy me a [beer](https://www.paypal.me/MtnViewMark)!**
"""

betaIntroduction : Html.Html msg
betaIntroduction = fromMarkdown """
# Important

* **Back up your projects!** When you transfer a project from the Digitakt to
  this app, please write it to a file, first thing. That way, if this has a bug
  and munches your project... you can always load and transfer the original back.

* **This is a release candidate.** I believe it works, but there may be bugs.
  Please help by filing a
  [bug report](https://github.com/mzero/elk-herd-project/issues).

* This was created by me, [mzero](https://www.elektronauts.com/u/mzero/),
  not Elektron.

"""

introduction : Html.Html msg
introduction = if Build.beta then betaIntroduction else plainIntroduction


versionInfo : Int -> String -> (Int, Html.Html msg)
versionInfo i s = (i, fromMarkdown s)


changeLog : List (Int, Html.Html msg)
changeLog =
  [ versionInfo 33000 """
## Version 3.3beta - Jan 9, 2025

This is the first open source release. The code base has undergone much
internal improvement since 3.2.1, in anticipation of supporting Digitakt II.
While tested, this should have some more milage before the next release.
"""

  , versionInfo 32100 """
## Version 3.2.1 - Dec 27, 2024

Support for Digitakt OS version 1.52
"""

  , versionInfo 32001 """
## Version 3.2 - May 28, 2024

Finally three-dot-two!
"""

  , versionInfo 32000 """
## Version 3.2beta - May 22, 2024

### Support for Digitakt OS version 1.51A:

* Should also support 1.40A. 10.40B, 1.50 and 1.51
* Song mode is still not supported:
"""

  , versionInfo 31000 """
## Version 3.1beta - Dec 12, 2022

### Partial support Digitakt OS version 1.40:

* Project saving, management and loading work with new formats from machine
* Song mode is not supported:
  * Songs saved with the project are not saved or restored
  * Patterns in songs are not kept in sync if you move patterns around
"""

  , versionInfo 30002 """
## Version 3.0.2beta - June 30, 2021

* Support Digitakt OS version 1.30
"""

  , versionInfo 30001 """
## Version 3.0.1 - Feb 20, 2020

* Support Digitakt OS version 1.20
"""

  , versionInfo 30000 """
## Version 3.0 - June 22, 2020

It's officially three-dot-oh!
"""

  , versionInfo 29907 """
## Version 3.0 rc 1 - Nov 27, 2019

### Bug fixes:

* Don't modify empty patterns when rearranging the sample pool. This keeps
empty patterns pristine. Broaden the test for empty to include previously
"shuffled" empty patterns that this bug created.

* Don't get stuck when loop-back ports are selected (like IAC Bus). Now just
give a clever warning.

### Minor improvements:

* Cleaner handling of non-sysex messages
* Individual help topcs can now be linked to
* Improved editing of pattern and sound names, keeping the cursor from jumping
in more cases.

"""
  , versionInfo 29906 """
## Version 3.0 beta 3 - Nov 2, 2019

### Bug fixes:

* Nasty bug squashed that moved plocked samples slots when samples slots
were relocated.

"""
  , versionInfo 29905 """
## Version 3.0 beta 2 - Oct 18, 2019

### New Features:

* Undo/Redo for project management operations.

* When importing, try to keep imported items together, and in an empty bank
if at all possible.

* Notice when Transfer is running, and halt, telling the user to quit Transfer.
"""

  , versionInfo 29904 """
## Version 3.0 beta 1 - Oct 11, 2019

### New Features:

* Import from a project lets you select patterns, samples and sounds from
one project to be imported into the current project.

* Help launches as a separate page.

### Bug fixes:

* Empty items that other items still refer to are treated as "phantom" and are
shown with grey text. These will no longer be accidentially overwritten.

* Sample slot zero can no longer be accidentially dragged or overwritten.

* Build is now optimized and should run a bit faster.

"""

  , versionInfo 29902 """
## Version 3.0 alpha 2 & 3 - Sep 18, 2019

### New Features:

* Support older Digitakt OS versions: 1.08, 1.10, and 1.11beta6

### If you experience trouble:

* Disable Overbridge on the Digitakt: Go to
  **Settings** > **System** > **USB Config** and
  set it to **USB MIDI**
* Stop the sequencer before sending a project to the Digitakt

### Bug fixes:

* Renamed project file commands **Open** and **Save**
* Don't include sample slot zero (off) in **Select Related** command

"""

  , versionInfo 29901 """
## Version 3.0 alpha 1 - Sep 12, 2019

### Digitakt Project Management:

* Project transfer to/from device
* Project transfer to/from files
* Pattern reordering
* Sample Pool reordering - all Pattern audio tracks, plocks,
  and Sound Pool items are kept in sync
* Sound Pool reordering - all Pattern sound plocks are kept in sync
* Pattern and Sound renaming

### Known issues & missing features:

* Requires Digitakt OS 1.11 to support project management
* LFOs set to control Sample Slot are NOT updated if you reorder the sample
  pool.
* Coming: Pattern and Sound import from 2nd project file
* Coming: Help system

"""
  ]

optInLabel : String
optInLabel = """
Send usage data to mzero's server to help make this
better. Nothin' personal, ya understand, just stats.
"""


view : Model -> Html.Html Msg
view model =
  let
    userOptIn = model.reportingOptIn
  in
    Html.div [ Attr.class "container-fluid" ]
      [ Html.div [ Attr.class "row" ]
        [ Html.div [ Attr.class "col" ]
          [ Html.div [ Attr.class "jumbotron m-5", Attr.id "app-settings" ]
            [ Html.h1 [ Attr.class "display-3" ] [ Html.text title ]
            , (Html.map OnAlert <| Alert.view model.alertModel)

            , Html.div [ Attr.class "row", Attr.id "app-settings-body" ]
              [ Html.div [ Attr.class "col"]
                [ introduction ]
              , Html.div [ Attr.class "col scroll-box"]
                <| List.map Tuple.second changeLog
              ]

            , Html.hr [ Attr.class "my-4" ] []

            , Html.div [ Attr.class "row" ]
              [ Html.div [ Attr.class "col"]
                [ if Build.statsReporting
                    then
                      Html.div [ Attr.class "form-check mb-3" ]
                      [ Html.label [ Attr.class "form-check-label" ]
                        [ Html.input
                          [ Attr.class "form-check-input"
                          , Attr.type_ "checkbox"
                          , Attr.checked userOptIn
                          , Events.onCheck ReportingOptIn
                          ] [ ]
                        , Html.text optInLabel
                        ]
                      ]
                    else
                      Html.text ""
                , Html.div [ Attr.class "input-group input-group-sm mb-3" ]
                    [ Html.div [ Attr.class "input-group-prepend" ]
                      [ Html.span [ Attr.class "input-group-text", Attr.id "flags-label" ]
                        [ Html.text "Optional flags:" ]
                      ]
                    , Html.input
                      [ Attr.type_ "text"
                      , Attr.class "form-control"
                      , Attr.value model.flags.source
                      , Attr.placeholder "you probably don't need anything here"
                      , Aria.labeledBy "flags-label"
                      , Events.onInput SetAppFlags
                      ]
                      [ ]
                    ]
                ]
              , Html.div [ Attr.class "col" ]
                [ Html.button
                  [ Attr.class "btn btn-primary btn-lg m-auto"
                  , Events.onClick (GoToScreen MidiScreen)
                  ]
                  [ Html.text "Get Started"]
                ]
              ]
            ]
          ]
        ]
      , Html.map OnSysEx <| SysEx.view model.sysExModel
    ]
