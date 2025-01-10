module Bank.IndexSet exposing
  ( IndexSet
  , empty
  , isEmpty
  , singleton
  , member
  , size

  , toList
  , fromList

  , filter
  , union
  , diff

  , isSubset
  )

{-| An `IndexSet i` works just like a `Set (Index i)`, only you can't have
that in Elm because you can't define your own `comparable` types. Hence we
get all this boilerplate.

The functions here mean exactly the same as they do for `Set`.
-}

import Set exposing (Set)

import Bank exposing (Index(..))


type IndexSet a = IndexSet (Set Int)

empty : IndexSet i
empty = IndexSet Set.empty

isEmpty : IndexSet i -> Bool
isEmpty (IndexSet s) = Set.isEmpty s

singleton : Index i -> IndexSet i
singleton (Index i) = IndexSet (Set.singleton i)

member : Index i -> IndexSet i -> Bool
member (Index i) (IndexSet s) = Set.member i s

size : IndexSet i -> Int
size (IndexSet s) = Set.size s

toList : IndexSet i -> List (Index i)
toList (IndexSet s) = List.map Index <| Set.toList s

fromList : List (Index i) -> IndexSet i
fromList = IndexSet << Set.fromList << List.map Bank.indexToInt

onIndexSet : (Set Int -> Set Int) -> IndexSet i -> IndexSet i
onIndexSet f (IndexSet s) = IndexSet (f s)

onIndexSet2 : (Set Int -> Set Int -> Set Int) -> IndexSet i -> IndexSet i -> IndexSet i
onIndexSet2 f (IndexSet s) (IndexSet t) = IndexSet (f s t)

filter : (Index i -> Bool) -> IndexSet i -> IndexSet i
filter f = onIndexSet (Set.filter (Index >> f))

union : IndexSet i -> IndexSet i -> IndexSet i
union = onIndexSet2 Set.union

diff : IndexSet i -> IndexSet i -> IndexSet i
diff = onIndexSet2 Set.diff

isSubset : IndexSet i -> IndexSet i -> Bool
isSubset (IndexSet s) (IndexSet t) =
  Set.isEmpty <| Set.filter (\i -> not (Set.member i s)) t
