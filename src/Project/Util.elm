module Project.Util exposing
  ( Kind(..)

  , slotLabel

  , isRelated
  )

import Array exposing (Array)

import Bank exposing (Index(..))
import Elektron.Digitakt.Related as DT


type Kind = KPattern | KSample | KSound


patternSlotLabels : Array String
patternSlotLabels =
  let
    bank i = String.dropLeft (i // 16) "ABCDEFGH" |> String.left 1
    pattern i = String.fromInt (modBy 16 i + 1) |> String.padLeft 2 '0'
    bankAndPattern i = bank i ++ pattern i
  in
    Array.initialize 128 bankAndPattern

sampleSlotLabels : Array String
sampleSlotLabels =
  Array.initialize 128 (\i -> String.fromInt i)
  -- Samples are numbered 0 (off), 1 .. 127
  -- Sometimes shown on the instrument with zero padding to three digits,
  -- sometimes not! Choose not to here, as it is easier to read.

soundSlotLabels : Array String
soundSlotLabels =
  Array.initialize 128 (\i -> String.fromInt (i+1))
  -- Sound pool sounds are numbered 1 .. 128 (internally numbers 0 .. 127!)
  -- Zero padding situation same as with samples.

slotLabel : Kind -> Int -> String
slotLabel k i =
  let
    labelSet =
      case k of
        KPattern -> patternSlotLabels
        KSample -> sampleSlotLabels
        KSound -> soundSlotLabels
  in
    Array.get i labelSet |> Maybe.withDefault "???"


isRelated : Kind -> Int -> DT.Related -> Bool
isRelated k i r =
  case k of
    KPattern -> DT.isRelatedPattern (Index i) r
    KSample -> DT.isRelatedSample (Index i) r
    KSound -> DT.isRelatedSound (Index i) r

