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
import Elektron.Digitakt.BlankData as BlankData
import Elektron.Digitakt.Dump as Dump
import Elektron.StructUtil as SU
import ByteArray.Compression
import ByteArray.Parser as Parser


prepDataCompressed : Maybe (List Int) -> Maybe ByteArray
prepDataCompressed =
  Maybe.andThen
    (ByteArray.fromList >> ByteArray.Compression.decompress)


buildBlank : SU.VersionedPart v a -> v -> Maybe ByteArray -> Maybe a
buildBlank struct v =
  Maybe.andThen (Parser.parse (struct.decoderVersion v) >> Result.toMaybe)


blankPatternKitData : Int -> Maybe ByteArray
blankPatternKitData v = prepDataCompressed <|
  case v of
      0 -> BlankData.blankPatternKit_v0_compressed
      1 -> BlankData.blankPatternKit_v1_compressed
      2 -> BlankData.blankPatternKit_v2_compressed
      3 -> BlankData.blankPatternKit_v3_compressed
      4 -> BlankData.blankPatternKit_v4_compressed
      5 -> BlankData.blankPatternKit_v5_compressed
      6 -> BlankData.blankPatternKit_v6_compressed
      7 -> BlankData.blankPatternKit_v7_compressed
      8 -> BlankData.blankPatternKit_v8_compressed
      9 -> BlankData.blankPatternKit_v9_compressed
      _ -> Nothing

blankPatternKit : Int -> Maybe Dump.PatternKit
blankPatternKit v =
  buildBlank Dump.structPatternKit v (blankPatternKitData v)


blankProjectSettingsData : Int -> Maybe ByteArray
blankProjectSettingsData v = prepDataCompressed <|
  case v of
      0 -> BlankData.blankProjectSettings_v0_compressed
      1 -> BlankData.blankProjectSettings_v1_compressed
      2 -> BlankData.blankProjectSettings_v2_compressed
      3 -> BlankData.blankProjectSettings_v3_compressed
      4 -> BlankData.blankProjectSettings_v4_compressed
      5 -> BlankData.blankProjectSettings_v5_compressed
      6 -> BlankData.blankProjectSettings_v6_compressed
      7 -> BlankData.blankProjectSettings_v7_compressed
      _ -> Nothing

blankProjectSettings : Int -> Maybe Dump.ProjectSettings
blankProjectSettings v =
  buildBlank Dump.structProjectSettings v (blankProjectSettingsData v)
