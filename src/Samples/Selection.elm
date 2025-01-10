module Samples.Selection exposing
  ( setSelection

  , pathNext

  , selectParent
  , selectChild
  , selectSibling

  , startMouse
  , finishMouse
  )

{-| Various utilities for manipulating the selection.alias

The main types and functions for selections are in `Samples.Base`.
TODO: Move them here.
-}

import Set

import Elektron.Drive as Drive exposing (Drive)
import Elektron.Path as Path exposing (Path)
import Missing.Html.Events as Events
import Missing.List as List
import Missing.Maybe as Maybe
import Samples.Base exposing (..)
import Samples.UpdateAction exposing (..)


setSelection : Selection -> UpdateAction
setSelection sel = updateModel <| \model -> { model | selection = Just sel }

selectEntry : Drive.Entry -> UpdateAction
selectEntry entry =
  setSelection <| mkSelect entry.path (Drive.isDirEntry entry) False NoInteraction


dirsAndFiles : Path -> Drive -> (List Drive.Entry, List Drive.Entry)
dirsAndFiles path drive =
  Drive.getEntry path drive
  |> Maybe.andThen Drive.subEntries
  |> Maybe.withDefault []
  |> List.partition Drive.isDirEntry

listNext : (a -> Bool) -> List a -> Maybe a
listNext f =
  let
    go items = case items of
      a :: (b :: _ as items_) -> if f a then Just b else go items_
      _ -> Nothing
  in
    go

pathNext : Bool -> Path -> Drive -> Maybe Path
pathNext prev path drive =
  Drive.getEntry path drive
  |> Maybe.andThen (\entry ->
    dirsAndFiles (Path.dirPath path) drive
    |> (if Drive.isDirEntry entry then Tuple.first else Tuple.second)
    |> (if prev then List.reverse else identity)
    |> listNext ((==) (Path.baseName path) << .name)
    |> Maybe.map .path
    )


type alias PathAction = Path -> UpdateAction

selectParent_ : PathAction
selectParent_ path = withModel <| \model ->
  let
    parent = Path.dirPath path
  in
    if parent == Path.rootPath
      then noop
      else setSelection <| mkSelect parent True False NoInteraction


selectChild_ : PathAction
selectChild_ path = withModel <| \model ->
  let
    (dirs, files) = dirsAndFiles path model.drive
  in
    case files ++ dirs of
      e :: _ -> selectEntry e
      _ -> noop

selectFileSibiling : Bool -> PathAction
selectFileSibiling prev path = withModel <| \model ->
  dirsAndFiles (Path.dirPath path) model.drive
  |> Tuple.second
  |> (if prev then List.reverse else identity)
  |> listNext ((==) (Path.baseName path) << .name)
  |> Maybe.unwrap noop selectEntry

selectDirSibiling : Bool -> PathAction
selectDirSibiling prev path = withModel <| \model ->
  let
    dirs p =
      dirsAndFiles p model.drive
      |> Tuple.first
      |> (if prev then List.reverse else identity)

    getInc p =
      dirs (Path.dirPath p)
      |> listNext ((==) (Path.baseName p) << .name)
      |> Maybe.map .path

    goUp p =
      let
        parent = Path.dirPath p
      in
        if parent == Path.rootPath then noop else sel parent

    diveDown p =
      dirs p
      |> List.toMaybe
      |> Maybe.unwrap p (diveDown << .path)

    diveIn = dirs >> List.toMaybe >> Maybe.map .path

    nextOut p =
      getInc p |> Maybe.unwrap (outNext <| Path.dirPath p) sel
    outNext p =
      if p == Path.rootPath then noop else nextOut p

    sel p = setSelection <| mkSelect p True False NoInteraction

  in
    if prev
      then getInc path |> Maybe.unwrap (goUp path) (diveDown >> sel)
      else diveIn path |> Maybe.unwrap (nextOut path) sel




onFocusedPath : PathAction -> PathAction ->  UpdateAction
onFocusedPath dirAct fileAct = withModel <| \model ->
  case model.selection of
    Just sel -> (if selectIsDir sel then dirAct else fileAct) (focusedPath sel)
    Nothing -> noop

selectParent : UpdateAction
selectParent = onFocusedPath selectParent_ selectParent_

selectChild : UpdateAction
selectChild = onFocusedPath selectChild_ (always noop)

selectSibling : Bool -> UpdateAction
selectSibling prev =
  onFocusedPath (selectDirSibiling prev) (selectFileSibiling prev)




between : (a -> e) -> e -> e -> List a -> List a
between f e1 e2 =
  let
    test a = let e = f a in e == e1 || e == e2
    skip l = case l of
      [] -> []
      a :: r ->
        if test a
          then a :: thru r
          else skip r
    thru l = case l of
      [] -> []
      a :: r ->
        if test a
          then [a]
          else a :: thru r
  in
    skip

extendPaths : Path -> Drive -> SelectedPaths -> SelectedPaths
extendPaths p drive w =
  let
    parent = Path.dirPath p
    baseA = Path.baseName p
    baseB = Path.baseName w.focus

    paths_ = Drive.getEntry parent drive
      |> Maybe.andThen Drive.subEntries
      |> Maybe.withDefault []
      |> between .name baseA baseB
      |> List.map .path
      |> Set.fromList
      |> Set.union w.basis
  in
    { w | paths = paths_ }

fixBasis : SelectedPaths -> SelectedPaths
fixBasis w = { w | basis = w.paths }


startMouse : Path -> Bool -> Events.MouseEvent -> Maybe Selection -> UpdateAction
startMouse p isDir evt mSel = withModel <| \model ->
  let
    alreadySelected = Maybe.unwrap False (isPathSelected p) mSel

    mod = case (evt.meta, evt.shift) of
      (True, _) -> if alreadySelected then Removing else Adding
      (False, True) -> Extending
      (False, False) -> Selecting

    onePath = Set.singleton p
    oneW = SelectedPaths onePath onePath p isDir

    w = case mSel of
      Nothing -> oneW
      Just sel -> if sel.what.isDir == isDir then sel.what else oneW

    w_ = case mod of
      Extending -> extendPaths p model.drive w
      Selecting ->
        if alreadySelected
          then { w | focus = p }          -- will get cleaned up on up
          else oneW
      Adding -> fixBasis { w | paths = Set.insert p w.paths, focus = p }
      Removing -> { w | focus = p }       -- will get removed on up

  in
     setSelection (Selection w_ Nothing (Clicking mod evt.xy p))

finishMouse : Selection -> UpdateAction
finishMouse sel = case sel.mouseState of
  Clicking mod xy p ->
    let
      setPaths ps = { w | paths = ps, basis = ps }

      w = sel.what
      w_ = case mod of
        Extending -> w
        Selecting -> fixBasis { w | paths = Set.singleton p }
        Adding -> w           -- was added on down
        Removing -> fixBasis { w | paths = Set.remove p w.paths }
    in
      setSelection (Selection w_ Nothing NoInteraction)
  _ -> noop





