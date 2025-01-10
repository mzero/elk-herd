module Missing.List exposing
    ( lookup
    , mapResult
    , nub
    , takeWhile
    , toMaybe
    , zip
    )


lookup : a -> List (a, b) -> Maybe b
lookup e l =
  case l of
    (a, b) :: tail -> if a == e then Just b else lookup e tail
    [] -> Nothing

nub : List a -> List a
nub =
  let
    merge e es = if List.member e es then es else e :: es
  in
    List.foldl merge []

mapResult : (a -> Result e b) -> List a -> List b
mapResult f = List.filterMap (Result.toMaybe << f)

takeWhile : (a -> Bool) -> List a -> List a
takeWhile f l0 =
  let
    go l = case l of
      e :: es -> if f e then e :: go es else []
      [] -> []
  in
    go l0

toMaybe : List a -> Maybe a
toMaybe l =
  case l of
    a :: _ -> Just a
    [] -> Nothing

zip : List a -> List b -> List (a, b)
zip = List.map2 Tuple.pair
