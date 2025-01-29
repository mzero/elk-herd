module Elektron.Drive exposing
  ( Drive
  , emptyDrive
  , isEmptyDrive
  , driveEntry
  , getEntry
  , contentEntriesDepthFirst

  , HashSize
  , hashSize
  , FileName
  , FileNamesByHash
  , fileNamesByHash
  , collisions

  , Stats
  , driveStats
  , trashStats

  , Entry
  , Item(..)
  , isDirEntry
  , subEntries

  , setRawEntries

  , makeUniqueName
  , makeUniqueNames
  )

{-| A model of the +Drive sample tree. The +Drive sample tree is much like
any other heirarchial file system.

Each sample file also has a hash of the contents stored with other info in the
directory. Entries in a project's sample pool refer to a sample by this hash &
file size, not path. Thus, the sample pool entries will still refer to the
correct samples, even if the user moves the around the +Drive. It even works
that if the user deletes a sample from the +Drive, and then later re-uploads the
same sample, the sample pool entry will still find it.
-}

import Dict
import Set

import Elektron.Path as Path exposing (Path)
import Missing.List as List
import Missing.Maybe as Maybe
import SysEx.ApiUtil

{-| Information associated with a name in a directory.

This information exists any kind of entry. See `Item` for information
specific to each kind.

Some of this information is computed for the application's convienence:
  * `path` is whole to this item, and is redundant, but nice to have
  * `itemSize` is computed for directories so we know how big they are easily
-}
type alias Entry =
  { name: String, path: Path, locked: Bool, item: Item, itemSize: Int }


{-| The entry information specific to each kind of item.

If it wasn't clear...
  * `D` are directories. They contian a list of `Entries`
  * `F` are sample files. They have a size, and they aforementioned hash.
  * `X` are for things we didn't expect returned by the API
-}
type Item
  = D (List Entry)
  | F { size: Int, hash: Int }
  | X RawEntry


isDirEntry : Entry -> Bool
isDirEntry entry = case entry.item of
  D _ -> True
  _ -> False

subEntries : Entry -> Maybe (List Entry)
subEntries e = case e.item of
  D es -> Just es
  _ -> Nothing



type Drive = Drive Entry

buildDrive : List Entry -> Drive
buildDrive entries =
  let
    driveSize = List.sum <| List.map .itemSize entries
  in
    Drive <| Entry "/" Path.rootPath False (D entries) driveSize

emptyDrive : Drive
emptyDrive = buildDrive []

isEmptyDrive : Drive -> Bool
isEmptyDrive (Drive entry0) = case entry0.item of
  D es -> List.isEmpty es
  _ -> True

driveEntry : Drive -> Entry
driveEntry (Drive entry0) = entry0

getEntry : Path -> Drive -> Maybe Entry
getEntry path (Drive entry0) =
  let
    get name mEntries =
      mEntries
      |> Maybe.andThen subEntries
      |> Maybe.andThen
        (List.toMaybe << List.filter ((==) name << .name))
  in
    List.foldr get (Just entry0) path

{-| Enumerates every `Entry` from a path on down, with the entries in the list
depth-first. Does not include the entry for the path itself.

The only use of this in the code is deleting the trash, which must delete all
sub entries in a directory before deleting the directory.
-}
contentEntriesDepthFirst : Path -> Drive -> List Entry
contentEntriesDepthFirst path drive =
  let
    subs e = List.concatMap subsThenSelf <| Maybe.withDefault [] <| subEntries e
    subsThenSelf e = subs e ++ [e]
  in
    getEntry path drive |> Maybe.map subs |> Maybe.withDefault []


{- This is a mirror of the mechanism by which sample slots are connected to
sample files. elk-herd needs it to be able to display the names of the samples
in the sample pool.

The value is a `List Path` because someone could have uploaded a sample more
than once into the +Drive and the same hash & size would appear at multiple
places in the tree.

TODO: Make a type alias for Hash, or perhaps a wrapped type, so that the order
here can't get confused.
-}

type alias HashSize = (Int, Int)
  -- NB: In most of the Elektron API & structures it is hash, size.
  -- BUT, in some it is size, hash. I'd use `type Hash = Hash Int`, but then
  -- it can't be used in a Dict key. 

hashSize : Int -> Int -> HashSize
hashSize h s = (h, s)

type alias FilesByHash = Dict.Dict HashSize (List Path)

filesByHash : Drive -> FilesByHash
filesByHash (Drive entry0) =
  let
    note path = Just << Maybe.unwrap [path] ((::) path)

    add_entry { path, item } d = case item of
      F { size, hash } -> Dict.update (hash, size) (note path) d
      D entries -> List.foldl add_entry d entries
      _ -> d
  in
    add_entry entry0 Dict.empty


type alias FileName = { name : String, path : String }
type alias FileNamesByHash = Dict.Dict HashSize FileName

fileNamesByHash : Drive -> FileNamesByHash
fileNamesByHash drive =
  let
    extractName _ paths =
      case paths of
        (path :: _) -> { name = Path.baseName path
                       , path = Path.pathString path
                       }
        [ ]         -> { name = "???" , path = "???" } -- never happens
  in
  filesByHash drive |> Dict.map extractName


collisions : Drive -> List (HashSize, (List Path))
collisions =
    Dict.toList << Dict.filter (\_ ps -> List.length ps > 1) << filesByHash


{-| These stats are only used by the `Report` module to send info about
+Drive usage back to the stats server.

Only aggregate stats about the whole +Drive, or the trash folder are ever
sent (even exported by the module!)
-}
type alias Stats = { dirs : Int, files : Int, others : Int, totalSize : Int }

zeroStats : Stats
zeroStats = Stats 0 0 0 0

sumStats : Stats -> Stats -> Stats
sumStats s t =
  { dirs = s.dirs + t.dirs
  , files = s.files + t.files
  , others = s.others + t.others
  , totalSize = s.totalSize + t.totalSize
  }

itemStats : Item -> Stats
itemStats item =
  let
    sumEntryStats = List.foldl (sumStats << itemStats << .item)
  in
    case item of
      D es -> sumEntryStats { zeroStats | dirs = 1 } es
      F f -> { zeroStats | files = 1, totalSize = f.size }
      X _ -> { zeroStats | others = 1 }

entryStats : Entry -> Stats
entryStats entry = itemStats entry.item


driveStats : Drive -> Stats
driveStats (Drive entry0) = entryStats entry0

trashStats : Drive -> Stats
trashStats = getEntry Path.trashPath >> Maybe.unwrap zeroStats entryStats


{-| Computes the total size taken up by an item. For file-like items, this
info is in the Item. For directories, the size is sum of it's entries' sizes.

Note: This size is only the space occupied by the samples. We have no way
of knowing how much space the directories themselves take up.
-}
computeItemSize : Item -> Int
computeItemSize item = case item of
  D es -> List.sum <| List.map .itemSize es
  F { size } -> size
  X { size } -> size


{-| Aliased here to make it clear that this is the unprocessed entry from the
SysEx API.

For reference:
    { hash: Int, size: Int, locked: Bool, type_: Char, name: String }
-}
type alias RawEntry = SysEx.ApiUtil.DirEntry


buildEntry : Path -> RawEntry -> Entry
buildEntry path rawEntry =
  let
    buildItem = case rawEntry.type_ of
      'D' -> if rawEntry.size == 0 && rawEntry.hash == 0
            then D []
            else X rawEntry
      'F' -> F { size = rawEntry.size, hash = rawEntry.hash }
      _  -> X rawEntry
  in
    { name =
        if String.isEmpty rawEntry.name
          then "--broken: empty--"
          else rawEntry.name
    , path = Path.subPath path rawEntry.name
    , locked = rawEntry.locked
    , item = buildItem
    , itemSize = computeItemSize buildItem
    }

buildEntries : Path -> List RawEntry -> List Entry
buildEntries path = List.map (buildEntry path)


{-| Upon receiving a new directory contents, this compares it to the existing
directoy contents.

Two lists are returned:

First is list of entries to use. Normally this is just the new entries, but
if an entry already existed, and was a directory, we keep the old version so
that we don't lose the entries inside it.

Second is a list of entries that should be fetched. Normally this is only
totally new entries, but if `rec` is `True`, then new entries that already
existed will be listed here for re-fetching as well.

TODO: Mystery #1: Why is this matching done case-insensitively?
TODO: Mystery #2: Why are non-directories included second list? The only place
that list is used immediately filters it for directories. Might as well do
it here by replacing both `True` constants here with `isDireEntry newE`
-}
mergeEntries : Bool -> List Entry -> List Entry -> (List Entry, List Entry)
mergeEntries rec oldEntries newEntries =
  -- returns a list of replacement entries, and entries to schedule for updates
  -- any dirs in new that exist in old, the old are retained (to keep the subs)
  -- any dirs in new that didn't exist in old are rescheduled
  -- if rec(ursive) is true, then existing dirs are rescheduled as well
  let
    oldDict = Dict.fromList <| List.map (\e -> (e.name, e)) oldEntries
    check newE = if isDirEntry newE
      then
        case Dict.get newE.name oldDict of
          Just oldE -> if oldE.locked == newE.locked && isDirEntry oldE
            then (oldE, rec)    -- Recurse into things known about or not
            else (newE, True)
          Nothing -> (newE, True)
      else
        (newE, False)
    sortKey = String.toUpper << .name
    merged = List.map check <| List.sortBy sortKey newEntries
    mergedEntries = List.map Tuple.first merged
    updateEntries = List.map Tuple.first <| List.filter Tuple.second merged
  in
    (mergedEntries, updateEntries)


{-| Updates a `Drive` from a new directory listing from the API.

Returns the updated drive, and the second list as returned from `mergeEntries`.
-}
setRawEntries : Bool -> Path -> List RawEntry -> Drive -> (Drive, List Entry)
setRawEntries rec path rawEntries (Drive entry0) =
  let
    newEntries = buildEntries path rawEntries

    onEntry : String -> List Entry -> (Item -> (Item, List a))
      -> (List Entry, List a)
    onEntry p entries itemF = case entries of
      [] -> ([], [])
      e :: es ->
        if e.name == p
          then
            let
              (newItem, result) = itemF e.item
              newItemSize = computeItemSize newItem
              e_ = { e | item = newItem, itemSize = newItemSize }
            in
              (e_ :: es, result)
          else
            Tuple.mapFirst ((::) e) <| onEntry p es itemF

    onDirItem : (List Entry -> (List Entry, List a)) -> Item -> (Item, List a)
    onDirItem entryF item = case item of
      D es -> Tuple.mapFirst D <| entryF es
      i -> (i, [])

    setPath : List String -> List Entry -> (List Entry, List Entry)
    setPath parts entries = case parts of
      [] ->
        mergeEntries rec entries newEntries

      p :: ps ->      -- find this part, recurse into it
        onEntry p entries <| onDirItem <| setPath ps

    driveEntries = case entry0.item of
      D es -> es
      _ -> []
  in
    Tuple.mapFirst buildDrive <| setPath (List.reverse path) driveEntries


entriesAt : Path -> Drive -> List Entry
entriesAt path drive =
  getEntry path drive
  |> Maybe.andThen subEntries
  |> Maybe.withDefault []

makeUniqueName : Path -> String -> Drive -> String
makeUniqueName path name0 drive =
  let
    names = List.map .name <| entriesAt path drive
    uniq n =
      let
        nameN = name0 ++ " " ++ String.fromInt n
      in
        if List.member nameN names then uniq (n+1) else nameN
  in
    if List.member name0 names then uniq 1 else name0


makeUniqueNames : Path -> List String -> Drive -> List String
makeUniqueNames path namesIn0 drive =
  let
    names0 = Set.fromList <| List.map .name <| entriesAt path drive

    xform nameIn names =
      let
        uniq n =
          let
            nameN = nameIn ++ " " ++ String.fromInt n
          in
            if Set.member nameN names then uniq (n+1) else nameN
      in
        if Set.member nameIn names then uniq 1 else nameIn

    go namesIn names = case namesIn of
      [] -> []
      n :: ns ->
        let
          nameOut = xform n names
        in
          nameOut :: go ns (Set.insert nameOut names)
  in
    go namesIn0 names0
