module Build exposing
  ( appVersion
  , lastMajorVersion
  , appVersionDisplay

  , beta
  , statsReporting
  )

{-| Stored in localStorage as the last run version.
-}
appVersion : Int
appVersion = 33003

{-| On startup, if the version in localStorage, is less than this, then
the app starts with the Settings page. Otherwise, it goes right to the
Midi Setup page.
-}
lastMajorVersion : Int
lastMajorVersion = 33002

{-| Human readable version string.
-}
appVersionDisplay : String
appVersionDisplay = "3.3.1"

beta : Bool
beta = False

statsReporting : Bool
statsReporting = False
