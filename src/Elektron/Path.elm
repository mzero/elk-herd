module Elektron.Path exposing
  ( Path
  , rootPath
  , factoryPath
  , trashPath
  , pathString
  , baseName
  , dirPath
  , subPath
  , pathDepth
  , startsWith
  , deepestCommonAncestor
  )

{-| Paths in the +Drive
-}

import Missing.List as List

{-| A list of path elements, from leaf to root order.
The root path is an empty list
No element should be "", ".", or ".."
-}
type alias Path = List String

rootPath : Path
rootPath = [ ]

{-| This is not an official part of any instrument. elk-herd creates and uses
it when the user deletes samples, so the file isn't lost immediately, and the
user can undo.
-}
trashPath : Path
trashPath = [ "trash" ]

{-| Path to the factory samples.
-}
factoryPath : Path
factoryPath = [ "factory"]


pathString : Path -> String
pathString p = case p of
  [] -> "/"
  _ -> List.foldl ((++) << (String.cons '/')) "" p

baseName : Path -> String
baseName p = case p of
  n :: _ -> n
  [] -> ""

dirPath : Path -> Path
dirPath = List.drop 1

subPath : Path -> String -> Path
subPath path name = name :: path

pathDepth : Path -> Int
pathDepth = List.length

startsWith : Path -> Path -> Bool
startsWith p q =
  let
    go r = if p == r then True else case r of
      [] -> False
      _ :: r_ -> go r_
  in
    go q

{-| Returns the deepest path that contains both paths.

    /alpha/beta/gamma/delta
    /alpha/beta/goose/duck/ermine
    --> /alpha/beta

    /alpha/beta/gamma/delta
    /alpha/beta/gamma
    --> /alpha/beta/gamma

    /alpha/beta/gamma
    /aardvark/bear/goose/duck
    --> /
-}
deepestCommonAncestor : Path -> Path -> Path
deepestCommonAncestor p q =
  List.zip (List.reverse p) (List.reverse q)
  |> List.takeWhile (\(a,b) -> a == b)
  |> List.map Tuple.first
  |> List.reverse

