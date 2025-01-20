module Elektron.Digitakt.Shuffle exposing
  ( dragAndDrop
  , compactDown
  )

import Array exposing (Array)

import Bank exposing (Bank, Index(..))
import Bank.Shuffle as Shuffle exposing (Shuffle)


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
dragAndDrop : (a -> Bool) -> List (Index i) -> Index i -> Bank i a -> Shuffle i
dragAndDrop isEmpty srcIndexes dstIndex bank =
  let
    slots = Bank.toArray bank
    srcs = List.map Bank.indexToInt srcIndexes
    (Index dst) = dstIndex
      -- TODO: bugs here if Array is too small

    n = Array.length slots
    clearedSlots = List.foldl (\i a -> Array.set i Nothing a) slots srcs

    placeNext moves srcs_ dst_ =
      case srcs_ of
        [] -> Shuffle.asShuffle moves
        src_ :: rest ->
          case Array.get dst_ clearedSlots of
            Nothing -> Shuffle.nullShuffle   -- ran off end, not enough space
            Just Nothing -> makeMove moves src_ dst_ rest
            Just (Just a) ->
              if isEmpty a
                then makeMove moves src_ dst_ rest
                else makeMove moves src_ dst_ (rest ++ [dst_])

    makeMove moves src_ dst_ rest =
      placeNext ((Index src_, Index dst_) :: moves) rest (dst_ + 1)
  in
    placeNext [] srcs dst

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
compactDown : (a -> Bool) -> Index i -> Bank i a -> Shuffle i
compactDown isEmpty (Index start) bank =
  let
    slots = Bank.toArray bank
    go moves src dst =
      case Array.get src slots of
        Nothing -> Shuffle.asShuffle moves
        Just Nothing -> go                            moves  (src + 1)  dst
        Just (Just a) ->
          if isEmpty a
            then        go                            moves  (src + 1)  dst
            else        go ((Index src, Index dst) :: moves) (src + 1) (dst + 1)
  in
    go [] start start