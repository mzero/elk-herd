module Bank exposing
  ( Index(..)
  , indexToInt

  , Bank
  , BankOf
  , get, set, put, clear
  , update
  , initializeEmpty
  , map
  , mapUpdate
  , indexedMap
  , indexedMapUpdate
  , toIndexedList
  , fromArray, toArray

  , Shuffle
  , nullShuffle
  , mergeShuffles
  , shuffleSources
  , dragAndDrop
  , compactDown
  , importShuffle
  , source
  , rereference
  , reorder
  , importFrom
  )

{-| Banks are a set of fixed slots where some kind of data can be stored.
For example, each project in a Digitakt has a bank of 128 patterns.
-}

import Array exposing (Array)
import Dict exposing (Dict)
import List

import Missing.List as List


{-| A bank of items. Banks have a set size, and each "slot" of the bank can
hold an item, or be empty. The items are of type `a`, whereas the contents of
a slot are of type `Maybe a`, since they can be empty.
-}
type alias BankOf a = Bank a a


{-| Index type used to index into a Bank. Normally, a is the same type as the
type of the item in the Bank. This ensures, for example, that `Index Pattern`
can only be used on `BankOf Pattern`, and not accidentally on `BankOf Sound`.
-}
type Index i = Index Int


{-| A more generalized bank of items, where the index basis (i), and the item
type (a) can be different. Most of the time you probably want `BankOf`.
Use this when you need to have a "related" bank that shares the same indexes as
a bank of a different type.
-}
type Bank i a = Bank (Array (Maybe a))


{-| Unwraps an `Index`.
Use this sparingly, you are removing the type protection!
-}
indexToInt : Index i -> Int
indexToInt (Index i) = i


{- BANK OPERATIONS -}

get : Index i -> Bank i a -> Maybe a
get (Index i) (Bank a) = Array.get i a |> Maybe.withDefault Nothing

set : Index i -> Maybe a -> Bank i a -> Bank i a
set (Index i) mv (Bank a as bank) = Bank <| Array.set i mv a

put : Index i -> a -> Bank i a -> Bank i a
put idx v = set idx (Just v)

clear : Index i -> Bank i a -> Bank i a
clear idx = set idx Nothing

{-| Apply an slot update function to a particular slot contents.
Update functions are powerful. See `mapUpdate` for some examples of them.
-}
update : (Maybe a -> Maybe a) -> Index i -> Bank i a -> Bank i a
update f idx bank = set idx (f <| get idx bank) bank

{-| Create a bank of n slots, all empty.
-}
initializeEmpty : Int -> Bank i a
initializeEmpty count = Bank <| Array.repeat count Nothing

{- Create a related bank by mapping all the elements.
All non-empty slots will be transformed, Empty slots will stay empty.
-}
map : (a -> b) -> Bank i a -> Bank i b
map f (Bank a) = Bank <| Array.map (Maybe.map f) a

{-| Apply an update function every slot in the bank. This has many uses:

    cubbyLabels : BankOf String
    ...
    mapUpdate (Maybe.map (String.left 8)) cubbyLabels
      -- trim the name of every assigned cubby

    mapUpdate (Maybe.withDefault "--free--" >> Just) cubbyLabels
      -- label all unassigned cubbies "--free--"

    mapUpdate (\s -> if s == Just "Bob" then Nothing else s) cubbyLabels
      -- clear all slots labeled "Bob"

-}
mapUpdate : (Maybe a -> Maybe b) -> Bank i a -> Bank i b
mapUpdate f (Bank a) = Bank <| Array.map f a

indexedMap : (Int -> a -> b) -> Bank i a -> Bank i b
indexedMap f (Bank a) =
  Bank <| Array.indexedMap (\i -> Maybe.map (f i)) a

indexedMapUpdate : (Int -> Maybe a -> Maybe b) -> Bank i a -> Bank i b
indexedMapUpdate f (Bank a) =
  Bank <| Array.indexedMap f a

toIndexedList : Bank i a -> List (Index i, Maybe a)
toIndexedList (Bank a) =
  Array.toIndexedList a
  |> List.map (Tuple.mapFirst Index)


fromArray : Array (Maybe a) -> Bank i a
fromArray = Bank

toArray : Bank i a -> Array (Maybe a)
toArray (Bank a) = a


{- SHUFFLES -}

type To a = Trash | Dst Int
type From a = Empty | Src Int

{-| A `Shuffle` represents a reordering of items in a bank.
-}
type Shuffle a =
  Shuffle
  { movesTo: Dict Int (To a)
  , cameFrom: Dict Int (From a)
  }

nullShuffle : Shuffle i
nullShuffle = Shuffle { movesTo = Dict.empty, cameFrom = Dict.empty }


{-| Given a list of moves, from one slot to another, construct a `Shuffle`.
The moves can be intra-bank, so moving from slot i to slot j, leaves slot i
empty. Or the moves can be from one bank to another, leaving the source bank
unchanged.
-}
buildShuffle : Bool -> List (Int, Int) -> Shuffle i
buildShuffle intraBank moves =
  Shuffle
    { movesTo = Dict.fromList
      <| (if intraBank then List.map (\(i, j) -> (j, Trash)) moves else [])
        ++ List.map (\(i, j) -> (i, Dst j)) moves
    , cameFrom = Dict.fromList
      <| (if intraBank then List.map (\(i, j) -> (i, Empty)) moves else [])
        ++ List.map (\(i, j) -> (j, Src i)) moves
  }

{-| A list of indexs that are moving to new places.
-}
shuffleSources  : Shuffle i -> List (Index i)
shuffleSources (Shuffle { movesTo }) =
  Dict.toList movesTo
  |> List.filterMap
    (\(i, dst) -> if dst == Trash then Nothing else Just (Index i))

{-| When moving things within a single bank. -}
asShuffle : List (Int, Int) -> Shuffle i
asShuffle = buildShuffle True

{-| When moving things from one bank to another. -}
asImport : List (Int, Int) -> Shuffle i
asImport = buildShuffle False


mergeShuffles : Shuffle i -> Shuffle i -> Shuffle i
mergeShuffles (Shuffle a) (Shuffle b) =
  Shuffle
    { movesTo = Dict.fromList
      <| Dict.toList b.movesTo ++ Dict.toList a.movesTo
    , cameFrom = Dict.fromList
      <| Dict.toList b.cameFrom ++ Dict.toList a.cameFrom
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
dragAndDrop : (a -> Bool) -> List (Index i) -> Index i -> Bank i a -> Shuffle i
dragAndDrop isEmpty srcIndexes dstIndex (Bank slots) =
  let
    srcs = List.map indexToInt srcIndexes
    (Index dst) = dstIndex
      -- TODO: bugs here if Array is too small

    n = Array.length slots
    clearedSlots = List.foldl (\i a -> Array.set i Nothing a) slots srcs

    placeNext moves srcs_ dst_ =
      case srcs_ of
        [] -> asShuffle moves
        src_ :: rest ->
          case Array.get dst_ clearedSlots of
            Nothing -> nullShuffle   -- ran off end, not enough space
            Just Nothing -> makeMove moves src_ dst_ rest
            Just (Just a) ->
              if isEmpty a
                then makeMove moves src_ dst_ rest
                else makeMove moves src_ dst_ (rest ++ [dst_])

    makeMove moves src_ dst_ rest =
      placeNext ((src_, dst_) :: moves) rest (dst_ + 1)
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
compactDown : (a -> Bool) -> Int -> Bank i a -> Shuffle i
compactDown isEmpty start (Bank slots) =
  let
    go moves src dst =
      case Array.get src slots of
        Nothing -> asShuffle moves
        Just Nothing ->   go                moves  (src + 1)  dst
        Just (Just a) ->
          if isEmpty a
            then          go                moves  (src + 1)  dst
            else          go ((src, dst) :: moves) (src + 1) (dst + 1)
  in
    go [] start start


{-| Construct a `Shuffle` of items being imported from one bank into another.
-}
importShuffle : List (Index i, Index i) -> Shuffle i
importShuffle =
  List.map (\(Index i, Index j) -> (i,j)) >> asImport


{-| Where does this index come from? Returns `Just` the index it came from,
or itself if it didn't move. Returns `Nothing` if the item is being cleared.
-}
source : Shuffle i -> Index i -> Maybe (Index i)
source (Shuffle { cameFrom }) (Index i as idx) =
  case Dict.get i cameFrom of
    Nothing -> Just idx
    Just Empty -> Nothing
    Just (Src j) -> Just (Index j)


{-| Where does this item go? Returns `Nothing` if the item was removed.
Returns `Just` either the new location, or itself if it didn't move.

This is often used for fixing up the references to an item as it moves.
-}
rereference : Shuffle i -> Index i -> Maybe (Index i)
rereference (Shuffle { movesTo }) (Index i as idx) =
  case Dict.get i movesTo of
    Nothing -> Just idx
    Just Trash -> Nothing
    Just (Dst j) -> Just (Index j)


{-| Shuffle the items. The source bank and destination bank can be the same.
The returned bank is the permuted destination bank.  The fixup function is
applied to items that are moved.
-}
applyShuffle : Shuffle i -> (a -> a) -> Bank i a -> Bank i a -> Bank i a
applyShuffle (Shuffle { cameFrom }) fixupFn (Bank src) (Bank dst) =
  let
    getFrom i dItem =
      case Dict.get i cameFrom of
        Nothing -> dItem          -- no change, use what is in dst already
        Just Empty -> Nothing
        Just (Src j) ->
          Array.get j src |> Maybe.withDefault Nothing |> Maybe.map fixupFn
  in
    Bank <| Array.indexedMap getFrom dst


{-| Shuffle items within a bank.
-}
reorder : Shuffle i -> Bank i a -> Bank i a
reorder shuf bank = applyShuffle shuf identity bank bank


{-| Shuffle items as they are imported from one bank to another. The fix up
function is used here to adjust references to things like samples and sounds
from the original project to the destination project.
-}
importFrom : Shuffle i -> (a -> a) -> Bank i a -> Bank i a -> Bank i a
importFrom = applyShuffle

