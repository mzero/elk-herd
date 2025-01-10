module Job exposing
  ( Step(..)
  , Job
  , map
  , mapProgress
  , andThen

  , singleton

  , list
  , indexedList
  , array
  , indexedArray
  , dict
  , set
  )

{-| Perform a sequence of calculations one step at a time.

This can be very useful if you'd like to do a longer compputation but
want to keep the rest of the UI running, either to update a progress bar
or to allow user interaction while the computation is happening.

This package is simpler and more lighweight than using web workers,
but it clearly doesn't have any true parallelism.

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)

{-| The state of a Job, which has progress indication of p, and is computing
an a.

Either there is a NextStep, which has how much progress the job has made,
and a function for doing some work on the job and returning the next Step.

Or, the job is Complete, with an a as the result.
-}
type Step p a
  = NextStep p (() -> Step p a)
  | Complete a

{-| A job before it has started.
-}
type alias Job p a = () -> Step p a

map : (a -> b) -> Job p a -> Job p b
map fab ja =
  \_ -> case ja () of
    NextStep p jaNext -> NextStep p <| map fab jaNext
    Complete a -> Complete (fab a)

mapProgress : (p -> q) -> Job p a -> Job q a
mapProgress fpq ja =
  \_ -> case ja () of
    NextStep p jaNext -> NextStep (fpq p) <| mapProgress fpq jaNext
    Complete a -> Complete a

andThen : (a -> Job p b) -> Job p a -> Job p b
andThen jobFn ja =
  \_ -> case ja () of
    NextStep p jaNext -> NextStep p <| andThen jobFn jaNext
    Complete a -> jobFn a ()


singleton : (a -> b) -> a -> Job () b
singleton fab a =
  \_ -> NextStep () (\_ -> Complete (fab a))


listPrim : (Int -> a -> b) -> (Int -> a -> p) -> (List b -> c) -> List a -> Job p c
listPrim fValue fProgress fComplete items =
  let
    step i inList outList =
      case inList of
        a :: rest ->
          NextStep (fProgress i a) (\_ -> step (i + 1) rest (fValue i a :: outList))
        [] -> Complete (fComplete outList)
  in
    \_ -> step 0 items []

{-| Maps elements in a list, one step at a time, as a Job
-}
list : (a -> b) -> List a -> Job a (List b)
list fab = listPrim (\_ a -> fab a) (\_ a -> a) List.reverse

{-| Maps elements in a list, supplying the index to both the mapping function,
as part of the progress value.
-}
indexedList : (Int -> a -> b) -> List a -> Job (Int, a) (List b)
indexedList fiab = listPrim fiab Tuple.pair List.reverse


arrayPrim : (Int -> a -> b) -> (Int -> a -> p) -> (Array b -> c) -> Array a -> Job p c
arrayPrim fValue fProgress fComplete items =
  let
    step i outArray =
      case Array.get i items of
        Just a ->
          NextStep (fProgress i a) (\_ -> step (i + 1) (Array.push (fValue i a) outArray))
        Nothing -> Complete (fComplete outArray)
  in
    \_ -> step 0 Array.empty

{-| Maps elements in an array, one step at a time, as a Job
-}
array : (a -> b) -> Array a -> Job a (Array b)
array fab = arrayPrim (\_ a -> fab a) (\_ a -> a) identity

{-| Maps elements in an array, supplying the index to both the mapping function,
as part of the progress value.
-}

indexedArray : (Int -> a -> b) -> Array a -> Job (Int, a) (Array b)
indexedArray fiab = arrayPrim fiab Tuple.pair identity

{-| Maps elements in a dictionary, one entry at a time, as a Job.
-}
dict : (comparable -> a -> b) -> Dict comparable a -> Job (comparable, a) (Dict comparable b)
dict fn = Dict.toList >> listPrim (\_ (k, v) -> (k, fn k v)) (\_ a -> a) Dict.fromList

{-| Maps elements in a set, one value at a time, as a Job.
-}
set : (comparableA -> comparableB) -> Set comparableA -> Job comparableA (Set comparableB)
set fn = Set.toList >> listPrim (\_ a -> fn a) (\_ a -> a) Set.fromList

