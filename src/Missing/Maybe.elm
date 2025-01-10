module Missing.Maybe exposing
    ( isJust
    , unwrap
    , toList
    , orElse
    )


isJust : Maybe a -> Bool
isJust m = case m of
  Just _ -> True
  Nothing -> False

unwrap : b -> (a -> b) -> Maybe a -> b
unwrap b fa ma = case ma of
  Just a -> fa a
  Nothing -> b

toList : Maybe a -> List a
toList m = case m of
  Just a -> [a]
  Nothing -> []

orElse : Maybe a -> Maybe a -> Maybe a
orElse ma mb =
  if isJust mb then mb else ma
