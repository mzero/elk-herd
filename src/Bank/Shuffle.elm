module Bank.Shuffle exposing
  ( Shuffle
  , nullShuffle
  , mergeShuffles

  , asShuffle, asImport

  , shuffleSources
  , source
  , rereference

  , reorder
  , importFrom
  )

import Array exposing (Array)
import Dict exposing (Dict)

import Bank exposing (Bank, Index(..))


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
buildShuffle : Bool -> List (Index i, Index i) -> Shuffle i
buildShuffle intraBank moves =
  Shuffle
    { movesTo = Dict.fromList
      <| ( if intraBank
            then List.map (\(Index i, Index j) -> (j, Trash)) moves
            else []
        )
        ++ List.map (\(Index i, Index j) -> (i, Dst j)) moves
    , cameFrom = Dict.fromList
      <| ( if intraBank
            then List.map (\(Index i, Index j) -> (i, Empty)) moves
            else []
        )
        ++ List.map (\(Index i, Index j) -> (j, Src i)) moves
  }

{-| A list of indexs that are moving to new places.
-}
shuffleSources  : Shuffle i -> List (Index i)
shuffleSources (Shuffle { movesTo }) =
  Dict.toList movesTo
  |> List.filterMap
    (\(i, dst) -> if dst == Trash then Nothing else Just (Index i))


{-| Construct a `Shuffle` of items being moved within a single bank.
-}
asShuffle : List (Index i, Index i) -> Shuffle i
asShuffle = buildShuffle True

{-| Construct a `Shuffle` of items being imported from one bank into another.
-}
asImport : List (Index i, Index i) -> Shuffle i
asImport = buildShuffle False


mergeShuffles : Shuffle i -> Shuffle i -> Shuffle i
mergeShuffles (Shuffle a) (Shuffle b) =
  Shuffle
    { movesTo = Dict.fromList
      <| Dict.toList b.movesTo ++ Dict.toList a.movesTo
    , cameFrom = Dict.fromList
      <| Dict.toList b.cameFrom ++ Dict.toList a.cameFrom
    }


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
applyShuffle (Shuffle { cameFrom }) fixupFn srcBank dstBank =
  let
    src = Bank.toArray srcBank
    dst = Bank.toArray dstBank
    getFrom i dItem =
      case Dict.get i cameFrom of
        Nothing -> dItem          -- no change, use what is in dst already
        Just Empty -> Nothing
        Just (Src j) ->
          Array.get j src |> Maybe.withDefault Nothing |> Maybe.map fixupFn
  in
    Bank.fromArray <| Array.indexedMap getFrom dst


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

