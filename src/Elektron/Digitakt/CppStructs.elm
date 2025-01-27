module Elektron.Digitakt.CppStructs exposing (..)

{-| Sizes and offsets of fields in the C++ data structures

Each function gives the offset to a particular field, or the size of a
structure.

These functions are generated by a utility program, using the C++ header files
that define the storage formats from Elektron. Unfortunately, those header
files are not openly available. If you need another field added here, ask
Mark for it, and he can generate it for you.

-}

import Elektron.Instrument exposing (Device(..), Version)

patternStorage_kitIndex : Version -> Maybe Int
patternStorage_kitIndex v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 22896
    (Digitakt, 1) -> Just 24944
    (Digitakt, 2) -> Just 24944
    (Digitakt, 3) -> Just 24976
    (Digitakt, 4) -> Just 24976
    (Digitakt, 5) -> Just 24976
    (Digitakt, 6) -> Just 24976
    (Digitakt, 7) -> Just 24992
    (Digitakt, 8) -> Just 24992
    (Digitakt, 9) -> Just 25008
    (Digitakt2, 0) -> Just 55984
    _ -> Nothing


patternStorage_sizeof : Version -> Maybe Int
patternStorage_sizeof v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 23040
    (Digitakt, 1) -> Just 25088
    (Digitakt, 2) -> Just 25088
    (Digitakt, 3) -> Just 25088
    (Digitakt, 4) -> Just 25088
    (Digitakt, 5) -> Just 25088
    (Digitakt, 6) -> Just 25088
    (Digitakt, 7) -> Just 25088
    (Digitakt, 8) -> Just 25088
    (Digitakt, 9) -> Just 25088
    (Digitakt2, 0) -> Just 56320
    _ -> Nothing


trackStorage_soundSlotLocks : Version -> Maybe Int
trackStorage_soundSlotLocks v =
  case (v.device, v.int) of
    (Digitakt, 1) -> Just 832
    (Digitakt, 2) -> Just 832
    (Digitakt, 3) -> Just 832
    (Digitakt, 4) -> Just 832
    (Digitakt, 5) -> Just 832
    (Digitakt2, 0) -> Just 1920
    _ -> Nothing


trackStorage_sizeof : Version -> Maybe Int
trackStorage_sizeof v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 779
    (Digitakt, 1) -> Just 907
    (Digitakt, 2) -> Just 909
    (Digitakt, 3) -> Just 909
    (Digitakt, 4) -> Just 910
    (Digitakt, 5) -> Just 911
    (Digitakt2, 0) -> Just 2207
    _ -> Nothing


kitStorage_trackSounds : Version -> Maybe Int
kitStorage_trackSounds v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 36
    (Digitakt, 1) -> Just 36
    (Digitakt, 2) -> Just 36
    (Digitakt, 3) -> Just 36
    (Digitakt, 4) -> Just 36
    (Digitakt, 5) -> Just 36
    (Digitakt, 6) -> Just 36
    (Digitakt, 7) -> Just 36
    (Digitakt, 8) -> Just 36
    (Digitakt, 9) -> Just 36
    (Digitakt2, 0) -> Just 60
    _ -> Nothing


kitStorage_midiParams : Version -> Maybe Int
kitStorage_midiParams v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 1424
    (Digitakt, 1) -> Just 1424
    (Digitakt, 2) -> Just 1424
    (Digitakt, 3) -> Just 1424
    (Digitakt, 4) -> Just 1424
    (Digitakt, 5) -> Just 1424
    (Digitakt, 6) -> Just 1424
    (Digitakt, 7) -> Just 1424
    (Digitakt, 8) -> Just 1424
    (Digitakt, 9) -> Just 1424
    (Digitakt2, 0) -> Just 5004
    _ -> Nothing


kitStorage_midiMask : Version -> Maybe Int
kitStorage_midiMask v =
  case (v.device, v.int) of
    (Digitakt2, 0) -> Just 9296
    _ -> Nothing


kitStorage_sizeof : Version -> Maybe Int
kitStorage_sizeof v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 2560
    (Digitakt, 1) -> Just 2560
    (Digitakt, 2) -> Just 2560
    (Digitakt, 3) -> Just 2560
    (Digitakt, 4) -> Just 2560
    (Digitakt, 5) -> Just 2560
    (Digitakt, 6) -> Just 2560
    (Digitakt, 7) -> Just 2560
    (Digitakt, 8) -> Just 2560
    (Digitakt, 9) -> Just 2560
    (Digitakt2, 0) -> Just 9728
    _ -> Nothing


midiSetupStorage_enableMask : Version -> Maybe Int
midiSetupStorage_enableMask v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 100
    (Digitakt, 1) -> Just 100
    (Digitakt2, 0) -> Just 256
    _ -> Nothing


midiSetupStorage_sizeof : Version -> Maybe Int
midiSetupStorage_sizeof v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 108
    (Digitakt, 1) -> Just 108
    (Digitakt2, 0) -> Just 268
    _ -> Nothing


soundStorage_sampleSlot : Version -> Maybe Int
soundStorage_sampleSlot v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 52
    (Digitakt, 1) -> Just 68
    (Digitakt, 2) -> Just 68
    (Digitakt2, 0) -> Just 100
    _ -> Nothing


soundStorage_sampleFile : Version -> Maybe Int
soundStorage_sampleFile v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 132
    (Digitakt, 1) -> Just 132
    (Digitakt, 2) -> Just 132
    (Digitakt2, 0) -> Just 279
    _ -> Nothing


soundStorage_sizeof : Version -> Maybe Int
soundStorage_sizeof v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 160
    (Digitakt, 1) -> Just 160
    (Digitakt, 2) -> Just 160
    (Digitakt2, 0) -> Just 299
    _ -> Nothing


soundParameters_sampleParamId : Version -> Maybe Int
soundParameters_sampleParamId v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 12
    (Digitakt, 1) -> Just 20
    (Digitakt, 2) -> Just 20
    (Digitakt2, 0) -> Just 36
    _ -> Nothing


projectSettingsStorage_sampleList : Version -> Maybe Int
projectSettingsStorage_sampleList v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 69
    (Digitakt, 1) -> Just 70
    (Digitakt, 2) -> Just 70
    (Digitakt, 3) -> Just 70
    (Digitakt, 4) -> Just 70
    (Digitakt, 5) -> Just 70
    (Digitakt, 6) -> Just 70
    (Digitakt, 7) -> Just 70
    (Digitakt2, 0) -> Just 251
    _ -> Nothing


projectSettingsStorage_sizeof : Version -> Maybe Int
projectSettingsStorage_sizeof v =
  case (v.device, v.int) of
    (Digitakt, 0) -> Just 2560
    (Digitakt, 1) -> Just 2560
    (Digitakt, 2) -> Just 2560
    (Digitakt, 3) -> Just 2560
    (Digitakt, 4) -> Just 2560
    (Digitakt, 5) -> Just 2560
    (Digitakt, 6) -> Just 2560
    (Digitakt, 7) -> Just 2560
    (Digitakt2, 0) -> Just 16896
    _ -> Nothing


