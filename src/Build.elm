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
appVersion = 33000

{-| On startup, if the version in localStorage, is less than this, then
the app starts with the Settings page. Otherwise, it goes right to the
Midi Setup page.
-}
lastMajorVersion : Int
lastMajorVersion = 33000

{-| Human readable version string.
-}
appVersionDisplay : String
appVersionDisplay =
  "3.3"
  ++ (if beta then "b" else "")


beta : Bool
beta = True

statsReporting : Bool
statsReporting = False

