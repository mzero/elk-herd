module Elektron.Digitakt.Blank exposing
  ( blankPatternKit
  , blankProjectSettings

  -- these are for testing
  , blankPatternKitData
  , blankProjectSettingsData
  )

{-| The application needs blank items for when users move items leaving a hole,
or outright delete them. It also needs a blank project settings for when the
user clears the project.

These two things contain many fields elk-herd doesn't manage or decode. So,
it needs a binary copy of an "emtpy" patternKit, and a projectSettings from
a newly initialized project. This file provides those.

These structures must match the versions used by the current connected
instrument. So, this blank data is provided based on the version required.

This data isn't small, and compiling 10 versions of it into the program would
bloat things considerably. (Believe me, I tried... it crashed some of the
elm tooling, and more than tripled the size of the application download!) So,
the data is stored compressed, and uncompressed when as needed.

Note for the curious: The application also needs empty sample slot entries,
but those are simple to create and aren't versioned.
See `Elektron.Digitakt.Dump.emptySample`

When the application needs an empty sound, it uses the first sound in the kit
of the empty patternKit.
-}

import ByteArray exposing (ByteArray)
import Elektron.Digitakt.BlankDataDigitakt as BlankD1
import Elektron.Digitakt.BlankDataDigitakt2 as BlankD2
import Elektron.Digitakt.Dump as Dump
import Elektron.Instrument exposing (Device(..), Version)
import Elektron.Struct.Version exposing (VersionSpec(..))
import ByteArray.Compression
import ByteArray.Parser as Parser


prepDataCompressed : Maybe (List Int) -> Maybe ByteArray
prepDataCompressed =
  Maybe.andThen
    (ByteArray.fromList >> ByteArray.Compression.decompress)


buildBlank : Dump.StorageStruct a -> Version -> Maybe ByteArray -> Maybe a
buildBlank struct v =
  let
    w = MatchVersion v
  in
  Maybe.andThen (Parser.parse (struct.decoder w) >> Result.toMaybe)


blankPatternKitData : Version -> Maybe ByteArray
blankPatternKitData v =
  prepDataCompressed
  <| case (v.device, v.int) of
      (Digitakt, 0) -> BlankD1.blankPatternKit_v0_compressed
      (Digitakt, 1) -> BlankD1.blankPatternKit_v1_compressed
      (Digitakt, 2) -> BlankD1.blankPatternKit_v2_compressed
      (Digitakt, 3) -> BlankD1.blankPatternKit_v3_compressed
      (Digitakt, 4) -> BlankD1.blankPatternKit_v4_compressed
      (Digitakt, 5) -> BlankD1.blankPatternKit_v5_compressed
      (Digitakt, 6) -> BlankD1.blankPatternKit_v6_compressed
      (Digitakt, 7) -> BlankD1.blankPatternKit_v7_compressed
      (Digitakt, 8) -> BlankD1.blankPatternKit_v8_compressed
      (Digitakt, 9) -> BlankD1.blankPatternKit_v9_compressed

      (Digitakt2, 0) -> BlankD2.blankPatternKit_v0_compressed
      _ -> Nothing

blankPatternKit : Version -> Maybe Dump.PatternKit
blankPatternKit v =
  buildBlank Dump.structPatternKit v (blankPatternKitData v)


blankProjectSettingsData : Version -> Maybe ByteArray
blankProjectSettingsData v =
  prepDataCompressed
  <| case (v.device, v.int) of
      (Digitakt, 0) -> BlankD1.blankProjectSettings_v0_compressed
      (Digitakt, 1) -> BlankD1.blankProjectSettings_v1_compressed
      (Digitakt, 2) -> BlankD1.blankProjectSettings_v2_compressed
      (Digitakt, 3) -> BlankD1.blankProjectSettings_v3_compressed
      (Digitakt, 4) -> BlankD1.blankProjectSettings_v4_compressed
      (Digitakt, 5) -> BlankD1.blankProjectSettings_v5_compressed
      (Digitakt, 6) -> BlankD1.blankProjectSettings_v6_compressed
      (Digitakt, 7) -> BlankD1.blankProjectSettings_v7_compressed

      (Digitakt2, 0) -> BlankD2.blankProjectSettings_v0_compressed
      _ -> Nothing

blankProjectSettings : Version -> Maybe Dump.ProjectSettings
blankProjectSettings v =
  buildBlank Dump.structProjectSettings v (blankProjectSettingsData v)
