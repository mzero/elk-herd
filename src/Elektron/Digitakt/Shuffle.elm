module Elektron.Digitakt.Shuffle exposing
  ( Spec

  , dragAndDrop
  , compactDown
  )

import Array exposing (Array)

import Bank exposing (Bank, Index(..))
import Bank.Shuffle as Shuffle exposing (Shuffle)
import Missing.Maybe as Maybe


type alias Spec i a =
  { isEmpty    : a -> Bool
  , skip       : Index i -> Bool
  , lowerBound : Index i
  , upperBound : Index i
  }

{-| Compute the `Shuffle` from a drag-n-drop operation.  The list of indicies
being dragged need not be contiguous. The drop point is a single slot index.
The new items are put there, filling in empty slots, and "pushing" filled slots
further down the bank.

For example:
```
    0  1  2  3  4  5  6  7  8  9 10 11 12 14 15 16
    a  b  -  c  d  e  f  -  -  g  -  h  i  -  -  -
```
dragging 1 & 4 % 5 onto 8 yields:
```
    0  1  2  3  4  5  6  7  8  9 10 11 12 14 15 16
    a  -  -  c  -  -  f  -  b  d  e  g  h  i  -  -
```
The `isEmpty` argument supplies a test for items in slots that should be
considered empty anyway. This is used for things like blank patterns.
-}
dragAndDrop : Spec i a -> List (Index i) -> Index i -> Bank i a -> Shuffle i
dragAndDrop spec srcIndexes dstIndex bank =
  let
    slots = Bank.toArray bank
    srcs = List.map Bank.indexToInt srcIndexes
    (Index dst) = dstIndex
    (Index lo) = spec.lowerBound
    (Index hi) = spec.upperBound
    n = Array.length slots
    clearedSlots = List.foldl (\i a -> Array.set i Nothing a) slots srcs

    placeNext sList d moves =
      case sList of
        [] -> Shuffle.asShuffle moves
        s :: rest ->
          if d < lo || hi < d
            then Shuffle.nullShuffle    -- ran out of bounds
            else
              if spec.skip (Index d)
                then placeNext sList (d + 1) moves
                else
                  case Array.get d clearedSlots of
                    Nothing -> Shuffle.nullShuffle   -- not enough space
                    Just Nothing -> placeNext rest   (d + 1) <| addMove s d moves
                    Just (Just a) ->
                      if spec.isEmpty a
                        then placeNext  rest         (d + 1) <| addMove s d moves
                        else placeNext (rest ++ [d]) (d + 1) <| addMove s d moves

    addMove s d moves =
      if s /= d
        then (Index s, Index d) :: moves
        else                       moves
  in
    placeNext srcs dst []


{-| Compute the `Shuffle` from compacting all non-empty items down to the front
of the bank. The region from `start` to the end of the bank is compacted.
For example:
```
    0  1  2  3  4  5  6  7  8  9 10 11 12 14 15 16
    -  -  -  a  b  -  c  d  e  f  -  -  g  -  h  i
```
compacting from 1 yields:
```
    0  1  2  3  4  5  6  7  8  9 10 11 12 14 15 16
    -  a  c  f  b  d  e  g  h  i  -  -  -  -  -  -
```
The `isEmpty` argument supplies a test for items in slots that should be
considered empty anyway. This is used for things like blank patterns.

-}
compactDown : Spec i a -> Bank i a -> Shuffle i
compactDown spec bank =
  let
    (Index lo) = spec.lowerBound
    (Index hi) = spec.upperBound

    items =
      List.range lo hi
      |> List.map Index
      |> List.filter
        (\i -> Bank.get i bank |> Maybe.unwrap True spec.isEmpty |> not)
      |> List.filter (spec.skip >> not)
  in
    dragAndDrop spec items spec.lowerBound bank

