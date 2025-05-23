module Elektron.Struct.Version exposing
  ( VersionSpec(..)

  , external, int, uint32be
  )

import Html

import ByteArray.Parser as Parser
import ByteArray.Builder as Builder
import Elektron.Instrument exposing (Device, Version)
import Elektron.Struct.Part as Part
import Elektron.Struct as ST
import SysEx.Internal exposing (..)


{- When parsing a structure, sometimes we know only the device, and we want
the structure to parse the version. Othertimes, when the structure is contained
in another, the version of the containing structure fixes the version of the
member structure. It can happen whether or not the member structure has a
version field.
-}
type VersionSpec
  = MatchDevice Device
  | MatchVersion Version


buildVersion : VersionSpec -> Int -> Result String Version
buildVersion spec i =
  case spec of
    MatchDevice d -> Ok (Version d i)
    MatchVersion v ->
      if i == v.int
        then Ok v
        else Err "not the expected version"

buildExternalVersion : VersionSpec -> Result String Version
buildExternalVersion spec =
  case spec of
    MatchDevice d   -> Err "no version specified"
    MatchVersion v  -> Ok v


versionView : String -> Version -> List (Html.Html Never)
versionView label v =
  [ fieldView label <| String.fromInt v.int ]

{-| Use this `Struct` when the structure doesn't have a version field in the
binary dump. The version must be presented when decoding such a structure.
-}
external : ST.Struct VersionSpec Version Version
external =
  { encoder = \_ -> Builder.empty
  , decoder = \w -> buildExternalVersion w |> Parser.result
  , view = versionView
  , version = identity
  }

{-| Use this `Struct` when the structure has a version integer of some sort
in the binary dump.
-}
int : (Part.Part Int) -> ST.Struct VersionSpec Version Version
int pInt =
  { encoder = \v -> pInt.encoder v.int
  , decoder = \w ->
      pInt.decoder
      |> Parser.andThen ( buildVersion w >> Parser.result )
  , view = versionView
  , version = identity
  }

{-| The version found in all storage structures that have a version field.
-}
uint32be : ST.Struct VersionSpec Version Version
uint32be = int Part.uint32be


