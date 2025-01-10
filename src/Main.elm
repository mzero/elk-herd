module Main exposing
  ( main
  )

import Browser

import Main.Base exposing (..)
import Main.Control exposing (..)
import Main.View exposing (..)
import Portage


main : Program String Model Msg
main =
  Browser.element
    { init = \flags -> (init flags, Portage.startUpApp ())
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


