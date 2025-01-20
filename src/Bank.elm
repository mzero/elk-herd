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
