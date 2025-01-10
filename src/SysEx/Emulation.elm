module SysEx.Emulation exposing
  ( Model
  , init

  , respond
  )

{-| This code was written to emulate the sample file system part of the Elektron
API. It was written back when I had just one Digitakt and was still reverse
engineering the protocol... I didn't want to mess up the only instrument I had
and was performing on.

This code has probably completely atrophied. It might work? It hasn't been run
in a long time. I'm a little loathe to just ditch it, as someone might want to
revive it.

In the meantime, see the `Build` module for the constant that enables this code.
-}

import Dict exposing (Dict)

import Build
import Missing.List as List
import Missing.Maybe as Maybe
import SysEx.Message exposing (..)



type alias PathInfo =
  { path : String, parts : List String, name : String, level: Int }
  -- path is in canonical monoid form: no trialing /, and root is ""
  -- parts is in leaf to root order, and empty for root
  -- name is "" for root, but effectively meaningless
  -- level is number of parts, in particular, 0 for root, and equal to the
  --   number of slashes in path

fromParts : List String -> PathInfo
fromParts parts =
  { path = List.foldl (\p s -> "/" ++ p ++ s) "" parts
  , parts = parts
  , name = List.toMaybe parts |> Maybe.withDefault ""
  , level = List.length parts
  }

pathInfo : String -> Maybe PathInfo
pathInfo path =
  let
    firstChar = String.left 1 path
    firstCharGood = firstChar == "/"

    parts = case String.split "/" <| String.dropLeft 1 path of
      [""] -> []
      ps -> List.reverse ps
    level = List.length parts

    goodPart p = not <| List.member p [ "", ".", ".."]
    allGoodParts = List.all goodPart parts

    (lastPart, lastPartGood) =
      case parts of
        n :: _ -> (n, goodPart n)
        _ -> ("", True)
  in
    if firstCharGood && allGoodParts && lastPartGood
      then Just <| fromParts parts
      else Nothing

parent : PathInfo -> Maybe PathInfo
parent pi = case pi.parts of
  _ :: qi -> Just <| fromParts qi
  [] -> Nothing

parents : PathInfo -> List PathInfo
parents pi = case parent pi of
  Just qi -> qi :: parents qi
  Nothing -> []



type alias Model = Dict String DirEntry

mkDirEntry : String -> DirEntry
mkDirEntry name =
  { hash = 0, size = 0, locked = False, type_ = 'D', name = name }


hideFactoryEntry : Bool
hideFactoryEntry = True


dirList : PathInfo -> Model -> (Model, List DirEntry)
dirList pi model =
  let
    nSlashes = String.length << String.filter ((==) '/')
    subLevel = pi.level + 1
    isSub p _ = String.startsWith pi.path p && nSlashes p == subLevel
    modify =
      if pi.level == 0 && hideFactoryEntry
        then List.filter (.name >> (==) "factory" >> not)
        else identity
  in
    (model, modify <| Dict.values <| Dict.filter isSub model)

dirCreate : PathInfo -> Model -> (Model, Bool)
dirCreate pi model =
  let
    go qi (m, ok) =
      if ok
        then
          case Dict.get qi.path m of
            Just entry -> (m, entry.type_ == 'D')
            Nothing -> (Dict.insert qi.path (mkDirEntry qi.name) m, True)
        else
          (m, False)
  in
    List.foldl go (model, True) (List.reverse (parents pi) ++ [pi])

dirDelete : PathInfo -> Model -> (Model, Bool)
dirDelete pi model =
  let
    isSubOrSelf p _ = String.startsWith pi.path p
    noSubs = (Dict.size <| Dict.filter isSubOrSelf model) <= 1
    type_ = Maybe.unwrap 'D' .type_ <| Dict.get pi.path model
    forceError = String.contains "err-rmdir" pi.name
  in
    if type_ == 'D' && noSubs && not forceError
      then (Dict.remove pi.path model, True)
      else (model, False)

fileDelete : PathInfo -> Model -> (Model, Bool)
fileDelete pi model =
  let
    type_ = Maybe.unwrap 'F' .type_ <| Dict.get pi.path model
    forceError = String.contains "err-rm" pi.name
  in
    if type_ == 'F' && not forceError
      then (Dict.remove pi.path model, True)
      else (model, False)

itemRename : PathInfo -> PathInfo -> Model -> (Model, Bool)
itemRename src dst model =
  let
    srcExist = Dict.member src.path model
    srcType_ = Maybe.unwrap '?' .type_ <| Dict.get src.path model
    srcLen = String.length src.path
    srcAsDir = src.path ++ "/"
    srcDir = parent src
    dstExist = Dict.member dst.path model
    distDir = parent dst
    dstDirType_ = case distDir of
      Just qi -> Maybe.unwrap '?' .type_ <| Dict.get qi.path model
      Nothing -> 'D'
    recursive = Maybe.unwrap False
      (String.startsWith src.path << .path) distDir

    simpleRename = srcDir == distDir
    fileMove = srcType_ == 'F'
    ok104 = simpleRename || fileMove
      -- okay in 1.04 version of the firmware
      -- a dir move, in 1.04, results in a mislinked .. entry

    ok = srcExist
      && not dstExist && dstDirType_ == 'D'
      && not recursive && not forceError
      && ok104

    move (p, e) =
      if src.path == p
        then (dst.path, { e | name = dst.name })
        else
          if String.startsWith srcAsDir p
            then (dst.path ++ String.dropLeft srcLen p, e)
            else (p, e)
    forceError = String.contains "err-mv" src.name
  in
    if ok
      then (Dict.fromList <| List.map move <| Dict.toList model, True)
      else (model, False)


respond : ElkMessage -> Model -> (Model, Maybe ElkMessage)
respond msg model =
  let
    response msg_ = (model, Just msg_)

    withPath : (PathInfo -> Model -> (Model, a)) -> String -> Maybe (Model, a)
    withPath f s = Maybe.map (\pi -> f pi model) <| pathInfo s

    withPaths2 : (PathInfo -> PathInfo -> Model -> (Model, a)) -> String -> String -> Maybe (Model, a)
    withPaths2 f s1 s2 = Maybe.map2 (\p1 p2 -> f p1 p2 model) (pathInfo s1) (pathInfo s2)

    toEntries : (List DirEntry -> ElkMessage) -> Maybe (Model, List DirEntry) -> (Model, Maybe ElkMessage)
    toEntries msg_ = Tuple.mapSecond (Just << msg_) << Maybe.withDefault (model, [])

    toOk : (Bool -> ElkMessage) -> Maybe (Model, Bool) -> (Model, Maybe ElkMessage)
    toOk msg_ = Tuple.mapSecond (\ok -> Just <| msg_ ok) << Maybe.withDefault (model, False)

    orNot : String -> (Model, Maybe ElkMessage) -> (Model, Maybe ElkMessage)
    orNot s r = if String.contains "err-not" s then (model, Nothing) else r
  in
    case msg of
      DeviceRequest ->
        response <| DeviceResponse
          12
          [ 0x01, 0x02, 0x10, 0x11, 0x12, 0x20, 0x21 ]
          "Digitakt"

      VersionRequest ->
        response <| VersionResponse "0021" "1.10"

      DirListRequest path ->
        withPath dirList path |> toEntries DirListResponse |> orNot path

      DirCreateRequest path ->
        withPath dirCreate path |> toOk DirCreateResponse

      DirDeleteRequest path ->
        withPath dirDelete path |> toOk DirDeleteResponse

      FileDeleteRequest path ->
        withPath fileDelete path |> toOk FileDeleteResponse

      ItemRenameRequest path newPath ->
        withPaths2 itemRename path newPath |> toOk ItemRenameResponse

      _ -> (model, Nothing)


buildFromTrace : List (String, (List DirEntry)) -> Model
buildFromTrace =
  let
    buildPath a b = (if String.endsWith "/" a then a else a ++ "/") ++ b
    buildEntry s e = (buildPath s e.name, e)
  in
    Dict.insert "" (mkDirEntry "")
    << Dict.fromList
    << List.concatMap (\(s, es) -> List.map (buildEntry s) es)

init : Model
init = if not Build.emulateDigitakt then Dict.empty else buildFromTrace
  [ ( "/"
    , [ { hash = 0, size = 0, locked = False, type_ = 'D', name = "clay-bells" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "demos" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "factory" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "GIRAFFE" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "incoming" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "modem" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "MORE HAZARDS" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "nature" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "packs" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "recorded" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "TanzBÅR" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "test-noise" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "test-xfer" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "wwv" }
      ]
    )

  , ( "/clay-bells"
    , [ { hash = 1548417721, size = 609122, locked = False, type_ = 'F', name = "arp (CDEFC)" }
      , { hash = 1121059869, size = 820070, locked = False, type_ = 'F', name = "drone (C)" }
      , { hash = 2146245135, size = 121628, locked = False, type_ = 'F', name = "hard (F)" }
      , { hash = 1674617155, size = 100248, locked = False, type_ = 'F', name = "pop" }
      , { hash = 3334872283, size = 749202, locked = False, type_ = 'F', name = "rept (E)" }
      , { hash = 3434400701, size = 66238, locked = False, type_ = 'F', name = "room&shake" }
      , { hash = 1513888169, size = 335356, locked = False, type_ = 'F', name = "single (F)" }
      , { hash = 3291604975, size = 768064, locked = False, type_ = 'F', name = "smooth (D)" }
      ]
    )

  , ( "/demos"
    , [ ]
    )

  , ( "/factory"
    , [ { hash = 0, size = 0, locked = True, type_ = 'D', name = "Drums" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Synths" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Toolbox" }
      ]
    )

  , ( "/GIRAFFE"
    , [ { hash = 1886218927, size = 265018, locked = False, type_ = 'F', name = "GIRAFFE1" }
      , { hash = 149614215, size = 389316, locked = False, type_ = 'F', name = "GIRAFFE2" }
      , { hash = 1844155727, size = 966988, locked = False, type_ = 'F', name = "GIRAFFE3" }
      , { hash = 381983527, size = 189016, locked = False, type_ = 'F', name = "GIRAFFE4" }
      , { hash = 3849838787, size = 247492, locked = False, type_ = 'F', name = "GIRAFFE5" }
      ]
    )

  , ( "/incoming"
    , [ ]
    )

  , ( "/modem"
    , [ { hash = 1149119517, size = 15932, locked = False, type_ = 'F', name = "BUZZZ" }
      , { hash = 3809211369, size = 18658, locked = False, type_ = 'F', name = "CRASH" }
      , { hash = 1567136489, size = 31038, locked = False, type_ = 'F', name = "DEEUMP" }
      , { hash = 677734173, size = 2194216, locked = False, type_ = 'F', name = "JuneChallenge" }
      , { hash = 943015221, size = 81922, locked = False, type_ = 'F', name = "NOIZE" }
      , { hash = 2509072249, size = 63614, locked = False, type_ = 'F', name = "RING" }
      , { hash = 3284228725, size = 65664, locked = False, type_ = 'F', name = "SNARG" }
      , { hash = 243199453, size = 44258, locked = False, type_ = 'F', name = "TONE-A" }
      , { hash = 3628219543, size = 38182, locked = False, type_ = 'F', name = "TONE-B" }
      , { hash = 1123914325, size = 92638, locked = False, type_ = 'F', name = "TONE1" }
      ]
    )

  , ( "/MORE HAZARDS"
    , [ { hash = 877270067, size = 1974738, locked = False, type_ = 'F', name = "AH" }
      , { hash = 2012653763, size = 927702, locked = False, type_ = 'F', name = "ROMANS" }
      , { hash = 1124283935, size = 2071498, locked = False, type_ = 'F', name = "TRAIN" }
      ]
    )

  , ( "/nature"
    , [ { hash = 4044069533, size = 131140, locked = False, type_ = 'F', name = "M.17 chirps" }
      , { hash = 515996659, size = 158038, locked = False, type_ = 'F', name = "M.17 waohwaa" }
      , { hash = 1233737951, size = 52906, locked = False, type_ = 'F', name = "M.cas roll" }
      , { hash = 866983187, size = 137266, locked = False, type_ = 'F', name = "M.dec s&h" }
      , { hash = 4187969103, size = 197566, locked = False, type_ = 'F', name = "M.n13 chirps" }
      ]
    )

  , ( "/packs"
    , [ { hash = 0, size = 0, locked = False, type_ = 'D', name = "djembe" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "Drum Enthusiast" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "Transistors & Skins" }
      ]
    )

  , ( "/recorded"
    , [ { hash = 3359037287, size = 24238, locked = False, type_ = 'F', name = "BLE-OOP" }
      , { hash = 994955957, size = 13874, locked = False, type_ = 'F', name = "BLEEP" }
      , { hash = 4212067651, size = 20872, locked = False, type_ = 'F', name = "BREK-2CY" }
      , { hash = 1748202223, size = 20886, locked = False, type_ = 'F', name = "BREK-BD" }
      , { hash = 569031367, size = 10390, locked = False, type_ = 'F', name = "BREK-H2" }
      , { hash = 2659918599, size = 10514, locked = False, type_ = 'F', name = "BREK-HH" }
      , { hash = 4164074115, size = 20908, locked = False, type_ = 'F', name = "BREK-SN" }
      , { hash = 1109279277, size = 10488, locked = False, type_ = 'F', name = "BREK-TM" }
      , { hash = 2793470505, size = 37534, locked = False, type_ = 'F', name = "GTRM" }
      , { hash = 451734285, size = 43214, locked = False, type_ = 'F', name = "HMMM" }
      , { hash = 1587899433, size = 162046, locked = False, type_ = 'F', name = "HUGEAMTOFTIME" }
      , { hash = 1611366293, size = 347712, locked = False, type_ = 'F', name = "STUTTER LLAMA" }
      , { hash = 558165773, size = 140380, locked = False, type_ = 'F', name = "THISHAÜS" }
      ]
    )

  , ( "/TanzBÅR"
    , [ { hash = 1097033939, size = 125028, locked = False, type_ = 'F', name = "bd1_A" }
      , { hash = 4280670275, size = 44408, locked = False, type_ = 'F', name = "bd1_B" }
      , { hash = 482521435, size = 144142, locked = False, type_ = 'F', name = "bd1_bended" }
      , { hash = 3261882483, size = 181472, locked = False, type_ = 'F', name = "bd1_C" }
      , { hash = 1070066027, size = 90872, locked = False, type_ = 'F', name = "bd1_D" }
      , { hash = 1572543495, size = 111440, locked = False, type_ = 'F', name = "bd1_E" }
      , { hash = 3341093889, size = 264908, locked = False, type_ = 'F', name = "bd1_F" }
      , { hash = 217485349, size = 156680, locked = False, type_ = 'F', name = "bd2_A" }
      , { hash = 73649619, size = 323386, locked = False, type_ = 'F', name = "bd2_B" }
      , { hash = 2109773343, size = 138066, locked = False, type_ = 'F', name = "bd2_C" }
      , { hash = 1179110909, size = 814566, locked = False, type_ = 'F', name = "bd2_superlong" }
      , { hash = 4046910453, size = 122762, locked = False, type_ = 'F', name = "bs_A" }
      , { hash = 3840349835, size = 21194, locked = False, type_ = 'F', name = "bs_B" }
      , { hash = 3722467921, size = 35072, locked = False, type_ = 'F', name = "cb_A" }
      , { hash = 3708787325, size = 13216, locked = False, type_ = 'F', name = "cb_B" }
      , { hash = 243014505, size = 14872, locked = False, type_ = 'F', name = "cb_C" }
      , { hash = 3882506865, size = 7940, locked = False, type_ = 'F', name = "cl_A" }
      , { hash = 3392970403, size = 2268, locked = False, type_ = 'F', name = "cl_B" }
      , { hash = 1838611515, size = 11624, locked = False, type_ = 'F', name = "cl_C" }
      , { hash = 3120048491, size = 4044, locked = False, type_ = 'F', name = "cl_D" }
      , { hash = 3346832355, size = 96064, locked = False, type_ = 'F', name = "cp_A" }
      , { hash = 302500893, size = 399278, locked = False, type_ = 'F', name = "cp_B" }
      , { hash = 490717199, size = 271894, locked = False, type_ = 'F', name = "cp_C" }
      , { hash = 107205001, size = 36198, locked = False, type_ = 'F', name = "cp_D" }
      , { hash = 37467423, size = 120270, locked = False, type_ = 'F', name = "cp_E" }
      , { hash = 696405677, size = 413268, locked = False, type_ = 'F', name = "cy_A" }
      , { hash = 3334580705, size = 593214, locked = False, type_ = 'F', name = "cy_B" }
      , { hash = 2735238139, size = 194596, locked = False, type_ = 'F', name = "cy_C" }
      , { hash = 259162649, size = 162066, locked = False, type_ = 'F', name = "cy_D" }
      , { hash = 2185647515, size = 34190, locked = False, type_ = 'F', name = "hh_A" }
      , { hash = 1075999357, size = 18104, locked = False, type_ = 'F', name = "hh_B" }
      , { hash = 1049070001, size = 52144, locked = False, type_ = 'F', name = "hh_C" }
      , { hash = 2967606137, size = 31204, locked = False, type_ = 'F', name = "ht_A" }
      , { hash = 147822049, size = 166972, locked = False, type_ = 'F', name = "ht_B" }
      , { hash = 2592551989, size = 448904, locked = False, type_ = 'F', name = "lt_A" }
      , { hash = 1525184449, size = 153062, locked = False, type_ = 'F', name = "lt_B" }
      , { hash = 1809253449, size = 143206, locked = False, type_ = 'F', name = "lt_C" }
      , { hash = 3035843655, size = 3226, locked = False, type_ = 'F', name = "ma_A" }
      , { hash = 957577179, size = 9170, locked = False, type_ = 'F', name = "ma_B" }
      , { hash = 3094823653, size = 20782, locked = False, type_ = 'F', name = "ma_C" }
      , { hash = 1747692175, size = 288066, locked = False, type_ = 'F', name = "mt_A" }
      , { hash = 2017467805, size = 36340, locked = False, type_ = 'F', name = "mt_B" }
      , { hash = 3717478935, size = 453142, locked = False, type_ = 'F', name = "mt_C" }
      , { hash = 3085165867, size = 843680, locked = False, type_ = 'F', name = "oh_A" }
      , { hash = 373842031, size = 112704, locked = False, type_ = 'F', name = "oh_B" }
      , { hash = 1198028357, size = 28480, locked = False, type_ = 'F', name = "oh_C" }
      , { hash = 1251848945, size = 98354, locked = False, type_ = 'F', name = "oh_D" }
      , { hash = 3382374651, size = 48066, locked = False, type_ = 'F', name = "rs_A" }
      , { hash = 248027477, size = 68630, locked = False, type_ = 'F', name = "rs_B" }
      , { hash = 634317879, size = 12066, locked = False, type_ = 'F', name = "sd_A" }
      , { hash = 446395555, size = 46314, locked = False, type_ = 'F', name = "sd_B" }
      , { hash = 640131605, size = 39814, locked = False, type_ = 'F', name = "sd_C" }
      , { hash = 271538989, size = 62244, locked = False, type_ = 'F', name = "sd_D" }
      , { hash = 1269778051, size = 14972, locked = False, type_ = 'F', name = "sd_E" }
      , { hash = 3154621635, size = 370918, locked = False, type_ = 'F', name = "sd_F" }
      ]
    )

  , ( "/test-noise"
    , [ { hash = 3987969965, size = 2880064, locked = False, type_ = 'F', name = "pink noise" }
      , { hash = 414884573, size = 2880064, locked = False, type_ = 'F', name = "white noise" }
      , { hash = 1981747295, size = 2880064, locked = False, type_ = 'F', name = "white noise 48k" }
      ]
    )

  , ( "/test-xfer"
    , [ { hash = 0, size = 0, locked = False, type_ = 'D', name = "bass drum" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "clÅPE" }
      , { hash = 1849845001, size = 2464, locked = False, type_ = 'F', name = "short-3k-saw" }
      ]
    )

  , ( "/wwv"
    , [ { hash = 3866554197, size = 313966, locked = False, type_ = 'F', name = "WWV 0h0m" }
      , { hash = 1886474601, size = 167730, locked = False, type_ = 'F', name = "WWV 10and15" }
      , { hash = 1260932733, size = 73942, locked = False, type_ = 'F', name = "WWV aloha" }
      , { hash = 3282029595, size = 145368, locked = False, type_ = 'F', name = "WWV astrotime" }
      , { hash = 3230466381, size = 117224, locked = False, type_ = 'F', name = "WWV attention" }
      , { hash = 466004531, size = 158716, locked = False, type_ = 'F', name = "WWV BIV" }
      , { hash = 4274536711, size = 192064, locked = False, type_ = 'F', name = "WWV blips" }
      , { hash = 2917608207, size = 141300, locked = False, type_ = 'F', name = "WWV c-u-time" }
      , { hash = 4175222813, size = 96004, locked = False, type_ = 'F', name = "WWV energy" }
      , { hash = 3305127785, size = 182838, locked = False, type_ = 'F', name = "WWV f-Experimental" }
      , { hash = 3248721995, size = 240064, locked = False, type_ = 'F', name = "WWV KC2XIO" }
      , { hash = 310185109, size = 72808, locked = False, type_ = 'F', name = "WWV leap sec" }
      , { hash = 1894164025, size = 165834, locked = False, type_ = 'F', name = "WWV m-Experimental" }
      , { hash = 220876813, size = 365290, locked = False, type_ = 'F', name = "WWV spaceship" }
      , { hash = 2045812095, size = 3086, locked = False, type_ = 'F', name = "WWV tick" }
      , { hash = 2423960773, size = 1536064, locked = False, type_ = 'F', name = "WWV ticks" }
      , { hash = 3663062251, size = 768064, locked = False, type_ = 'F', name = "WWV tone beat" }
      ]
    )

  , ( "/factory/Drums"
    , [ { hash = 0, size = 0, locked = True, type_ = 'D', name = "Acoustic" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Electronic" }
      ]
    )

  , ( "/factory/Synths"
    , [ { hash = 0, size = 0, locked = True, type_ = 'D', name = "Analog Notes" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Stabs & Pads" }
      ]
    )

  , ( "/factory/Toolbox"
    , [ { hash = 0, size = 0, locked = True, type_ = 'D', name = "Noise" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Oscillators" }
      ]
    )

  , ( "/packs/djembe"
    , [ { hash = 767914133, size = 64240, locked = False, type_ = 'F', name = "djembe-1" }
      , { hash = 4031391535, size = 58440, locked = False, type_ = 'F', name = "djembe-2" }
      , { hash = 1877220747, size = 59764, locked = False, type_ = 'F', name = "djembe-3" }
      , { hash = 3026188883, size = 38656, locked = False, type_ = 'F', name = "djembe-4" }
      , { hash = 1258054243, size = 46522, locked = False, type_ = 'F', name = "djembe-5" }
      , { hash = 1581048885, size = 46214, locked = False, type_ = 'F', name = "djembe-6" }
      , { hash = 3287947503, size = 22362, locked = False, type_ = 'F', name = "djembe-7" }
      , { hash = 2711768347, size = 32806, locked = False, type_ = 'F', name = "djembe-8" }
      , { hash = 3235198095, size = 44650, locked = False, type_ = 'F', name = "djembe-9" }
      , { hash = 2560968289, size = 48240, locked = False, type_ = 'F', name = "djembe-10" }
      , { hash = 2584995595, size = 76368, locked = False, type_ = 'F', name = "djembe-11" }
      , { hash = 1838710683, size = 92418, locked = False, type_ = 'F', name = "djembe-bass-tone-1" }
      , { hash = 1747626591, size = 91528, locked = False, type_ = 'F', name = "djembe-bass-tone-2" }
      , { hash = 1205388085, size = 89004, locked = False, type_ = 'F', name = "djembe-fill-1" }
      , { hash = 1229617367, size = 139202, locked = False, type_ = 'F', name = "djembe-fill-2" }
      , { hash = 3404747821, size = 90876, locked = False, type_ = 'F', name = "djembe-fill-3" }
      , { hash = 1882957937, size = 25142, locked = False, type_ = 'F', name = "djembe-flam-1" }
      , { hash = 23779029, size = 43532, locked = False, type_ = 'F', name = "djembe-flam-2" }
      , { hash = 2966376849, size = 60112, locked = False, type_ = 'F', name = "djembe-muffle-1" }
      , { hash = 884162627, size = 51160, locked = False, type_ = 'F', name = "djembe-muffle-2" }
      , { hash = 2903834851, size = 70048, locked = False, type_ = 'F', name = "djembe-muffle-3" }
      , { hash = 42331701, size = 49462, locked = False, type_ = 'F', name = "djembe-rim-1" }
      , { hash = 1405803411, size = 45108, locked = False, type_ = 'F', name = "djembe-rim-2" }
      , { hash = 706388243, size = 58980, locked = False, type_ = 'F', name = "djembe-rim-3" }
      , { hash = 2540006537, size = 47076, locked = False, type_ = 'F', name = "djembe-slap-1" }
      , { hash = 3441218711, size = 48060, locked = False, type_ = 'F', name = "djembe-slap-2" }
      , { hash = 3434274413, size = 50150, locked = False, type_ = 'F', name = "djembe-slap-3" }
      ]
    )

  , ( "/packs/Drum Enthusiast"
    , [ { hash = 2855986843, size = 96638, locked = False, type_ = 'F', name = "BD +BASS" }
      , { hash = 1784980997, size = 31310, locked = False, type_ = 'F', name = "BD 84" }
      , { hash = 3464716181, size = 79118, locked = False, type_ = 'F', name = "BD ACOUSTIC" }
      , { hash = 3076016657, size = 47162, locked = False, type_ = 'F', name = "BD DEEP" }
      , { hash = 400573249, size = 17728, locked = False, type_ = 'F', name = "BD FLAT" }
      , { hash = 3075798895, size = 33252, locked = False, type_ = 'F', name = "BD FUR" }
      , { hash = 2579261771, size = 43754, locked = False, type_ = 'F', name = "BD GREEDY" }
      , { hash = 764218089, size = 38046, locked = False, type_ = 'F', name = "BD HEAVY" }
      , { hash = 3892981919, size = 111464, locked = False, type_ = 'F', name = "BD IMPACT" }
      , { hash = 3002037255, size = 44432, locked = False, type_ = 'F', name = "BD KNOCK" }
      , { hash = 896080005, size = 184990, locked = False, type_ = 'F', name = "BD LONG" }
      , { hash = 1431329949, size = 40852, locked = False, type_ = 'F', name = "BD MID" }
      , { hash = 1184009869, size = 34642, locked = False, type_ = 'F', name = "BD MINIMAL" }
      , { hash = 1954084697, size = 91716, locked = False, type_ = 'F', name = "BD MORE" }
      , { hash = 830808203, size = 23838, locked = False, type_ = 'F', name = "BD NIC" }
      , { hash = 3102767543, size = 93440, locked = False, type_ = 'F', name = "BD NUMB" }
      , { hash = 210968907, size = 124306, locked = False, type_ = 'F', name = "BD OXY" }
      , { hash = 366579905, size = 164158, locked = False, type_ = 'F', name = "BD RESOR" }
      , { hash = 1246911303, size = 27338, locked = False, type_ = 'F', name = "BD SEMI" }
      , { hash = 1125790915, size = 53132, locked = False, type_ = 'F', name = "BD SMASH" }
      , { hash = 2149322739, size = 106572, locked = False, type_ = 'F', name = "BD SUB" }
      , { hash = 1144997189, size = 17408, locked = False, type_ = 'F', name = "BD TAPE" }
      , { hash = 1828782413, size = 32120, locked = False, type_ = 'F', name = "CLP CREEP" }
      , { hash = 2342422847, size = 19430, locked = False, type_ = 'F', name = "CLP DETROIT" }
      , { hash = 2775316347, size = 12876, locked = False, type_ = 'F', name = "HAT BASIC" }
      , { hash = 139043401, size = 27284, locked = False, type_ = 'F', name = "HAT BONA FIDE" }
      , { hash = 1958070805, size = 49530, locked = False, type_ = 'F', name = "HAT BRUSH" }
      , { hash = 177990871, size = 18276, locked = False, type_ = 'F', name = "HAT CYM" }
      , { hash = 3671587141, size = 103082, locked = False, type_ = 'F', name = "HAT FAVE" }
      , { hash = 418964081, size = 17274, locked = False, type_ = 'F', name = "HAT JUST SO" }
      , { hash = 1270595369, size = 12876, locked = False, type_ = 'F', name = "HAT MINIMAL" }
      , { hash = 2526956837, size = 15222, locked = False, type_ = 'F', name = "HAT SIMPLE" }
      , { hash = 37414479, size = 12876, locked = False, type_ = 'F', name = "HAT SIMPLE2" }
      , { hash = 3269160313, size = 23378, locked = False, type_ = 'F', name = "HAT SWEEP" }
      , { hash = 3404637721, size = 12876, locked = False, type_ = 'F', name = "PERC BING E5" }
      , { hash = 2094145089, size = 12876, locked = False, type_ = 'F', name = "PERC BLIB" }
      , { hash = 51542347, size = 16200, locked = False, type_ = 'F', name = "PERC GLAZE" }
      , { hash = 110281525, size = 35958, locked = False, type_ = 'F', name = "PERC NORMAN" }
      , { hash = 1900983741, size = 12876, locked = False, type_ = 'F', name = "PERC RIM" }
      , { hash = 1638241653, size = 25754, locked = False, type_ = 'F', name = "PERC WHIM" }
      , { hash = 3897884699, size = 15126, locked = False, type_ = 'F', name = "SN ATTACK" }
      , { hash = 1209884965, size = 37808, locked = False, type_ = 'F', name = "SN BODY" }
      , { hash = 3869763505, size = 20212, locked = False, type_ = 'F', name = "SN DIRECT" }
      , { hash = 2964015467, size = 17484, locked = False, type_ = 'F', name = "SN DRUNK" }
      , { hash = 1259564793, size = 35764, locked = False, type_ = 'F', name = "SN ECHO" }
      , { hash = 2234743943, size = 16100, locked = False, type_ = 'F', name = "SN GRUNCH" }
      , { hash = 3771172051, size = 12876, locked = False, type_ = 'F', name = "SN HIGH" }
      , { hash = 4209415333, size = 22610, locked = False, type_ = 'F', name = "SN IUF" }
      , { hash = 1004451963, size = 12876, locked = False, type_ = 'F', name = "SN MINIMAL" }
      , { hash = 3799645495, size = 26034, locked = False, type_ = 'F', name = "SN PHASING" }
      , { hash = 2675449985, size = 58382, locked = False, type_ = 'F', name = "SN ROOM" }
      , { hash = 2726269357, size = 38258, locked = False, type_ = 'F', name = "SN SID" }
      , { hash = 2795589061, size = 38328, locked = False, type_ = 'F', name = "SN SID 2" }
      , { hash = 148837113, size = 33310, locked = False, type_ = 'F', name = "SN SID 3" }
      , { hash = 540835741, size = 44638, locked = False, type_ = 'F', name = "SN SID 4" }
      , { hash = 1332306427, size = 112252, locked = False, type_ = 'F', name = "SN STRIKE" }
      , { hash = 2083383821, size = 39238, locked = False, type_ = 'F', name = "SN SUFFER" }
      , { hash = 3728495183, size = 56340, locked = False, type_ = 'F', name = "SN SWAMP" }
      , { hash = 2299824171, size = 28896, locked = False, type_ = 'F', name = "SNAP DUO" }
      , { hash = 604608345, size = 12876, locked = False, type_ = 'F', name = "SNAP THIN" }
      , { hash = 1118359113, size = 33234, locked = False, type_ = 'F', name = "TOM GRUBBY" }
      , { hash = 3723920299, size = 25780, locked = False, type_ = 'F', name = "TOM OVER E4" }
      , { hash = 2048088539, size = 86114, locked = False, type_ = 'F', name = "TOM SLIGHT" }
      , { hash = 3700656287, size = 159900, locked = False, type_ = 'F', name = "TOM TRACY" }
      ]
    )

  , ( "/packs/Transistors & Skins"
    , [ { hash = 0, size = 0, locked = False, type_ = 'D', name = "Cosmo CZ Synth" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "Korg Rhythm 55B" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "Nomad Rhythm Maker" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "Old Organic Drums" }
      , { hash = 0, size = 0, locked = False, type_ = 'D', name = "Small Percussive Things" }
      ]
    )

  , ( "/test-xfer/bass drum"
    , [ { hash = 210968907, size = 124306, locked = False, type_ = 'F', name = "BD OXY" }
      , { hash = 1246911303, size = 27338, locked = False, type_ = 'F', name = "BD SEMI" }
      , { hash = 1125790915, size = 53132, locked = False, type_ = 'F', name = "BD SMASH" }
      ]
    )

  , ( "/test-xfer/clÅPE"
    , [ { hash = 1828782413, size = 32120, locked = False, type_ = 'F', name = "CLP CREEP" }
      , { hash = 2342422847, size = 19430, locked = False, type_ = 'F', name = "CLP DETROIT" }
      ]
    )

  , ( "/factory/Drums/Acoustic"
    , [ { hash = 0, size = 0, locked = True, type_ = 'D', name = "Ambient Kit" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Dirty Kit" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Natural Kit" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Punchy Kit" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Slapped Perc" }
      ]
    )

  , ( "/factory/Drums/Electronic"
    , [ { hash = 0, size = 0, locked = True, type_ = 'D', name = "Abrasive" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Adlib" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Booming" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Bronze" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Buggin" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Cable" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Class" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Digit" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Don" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Fella" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Flange" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Pearl" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Puff" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Reson" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Rocking" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Rom" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "School" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Soft" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Spring" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Stonk" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Trunk" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Velvet" }
      , { hash = 0, size = 0, locked = True, type_ = 'D', name = "Weird" }
      ]
    )

  , ( "/factory/Synths/Analog Notes"
    , [ { hash = 789033197, size = 193716, locked = True, type_ = 'F', name = "Acidic" }
      , { hash = 1635520009, size = 348870, locked = True, type_ = 'F', name = "Bronze" }
      , { hash = 3520956689, size = 194798, locked = True, type_ = 'F', name = "Cheerful" }
      , { hash = 2820940849, size = 416398, locked = True, type_ = 'F', name = "Cheers" }
      , { hash = 3405238297, size = 144024, locked = True, type_ = 'F', name = "Cities" }
      , { hash = 1833914761, size = 342466, locked = True, type_ = 'F', name = "Club" }
      , { hash = 1371401055, size = 138882, locked = True, type_ = 'F', name = "Colorful" }
      , { hash = 3394510037, size = 247350, locked = True, type_ = 'F', name = "Darker" }
      , { hash = 3343029285, size = 338670, locked = True, type_ = 'F', name = "Determined" }
      , { hash = 1557967729, size = 73360, locked = True, type_ = 'F', name = "Down" }
      , { hash = 854502379, size = 230464, locked = True, type_ = 'F', name = "Effit" }
      , { hash = 117982693, size = 248218, locked = True, type_ = 'F', name = "Falling" }
      , { hash = 859822275, size = 260240, locked = True, type_ = 'F', name = "Felt" }
      , { hash = 2049737287, size = 53868, locked = True, type_ = 'F', name = "Flaunt" }
      , { hash = 945097417, size = 247146, locked = True, type_ = 'F', name = "Flies" }
      , { hash = 1910133291, size = 52386, locked = True, type_ = 'F', name = "Franc" }
      , { hash = 1444615073, size = 114316, locked = True, type_ = 'F', name = "Frog" }
      , { hash = 1878380003, size = 105086, locked = True, type_ = 'F', name = "Golden" }
      , { hash = 489965405, size = 196864, locked = True, type_ = 'F', name = "Highly" }
      , { hash = 2909980347, size = 108794, locked = True, type_ = 'F', name = "Hits" }
      , { hash = 98955103, size = 194992, locked = True, type_ = 'F', name = "Kitty" }
      , { hash = 3841599891, size = 163264, locked = True, type_ = 'F', name = "Klonk" }
      , { hash = 603390517, size = 259264, locked = True, type_ = 'F', name = "Later" }
      , { hash = 4130279547, size = 163264, locked = True, type_ = 'F', name = "Mallet" }
      , { hash = 3611087011, size = 46504, locked = True, type_ = 'F', name = "Mellow" }
      , { hash = 1382847441, size = 225664, locked = True, type_ = 'F', name = "Middle" }
      , { hash = 1911601043, size = 195756, locked = True, type_ = 'F', name = "Mouth" }
      , { hash = 1976104117, size = 237346, locked = True, type_ = 'F', name = "Noice" }
      , { hash = 2580359471, size = 110464, locked = True, type_ = 'F', name = "One" }
      , { hash = 712335667, size = 248324, locked = True, type_ = 'F', name = "Phasor" }
      , { hash = 3281734577, size = 62554, locked = True, type_ = 'F', name = "Plonk" }
      , { hash = 3789625577, size = 76864, locked = True, type_ = 'F', name = "Quick" }
      , { hash = 3784984255, size = 172864, locked = True, type_ = 'F', name = "Raver" }
      , { hash = 4276416023, size = 172864, locked = True, type_ = 'F', name = "Ringer" }
      , { hash = 2932749719, size = 195638, locked = True, type_ = 'F', name = "Rips" }
      , { hash = 3964492385, size = 345664, locked = True, type_ = 'F', name = "Rubber" }
      , { hash = 1224351141, size = 43380, locked = True, type_ = 'F', name = "Short" }
      , { hash = 1204964399, size = 363170, locked = True, type_ = 'F', name = "Small" }
      , { hash = 2788735495, size = 36434, locked = True, type_ = 'F', name = "Stout" }
      , { hash = 2750533317, size = 96922, locked = True, type_ = 'F', name = "Subtle" }
      , { hash = 440583049, size = 245220, locked = True, type_ = 'F', name = "Synced" }
      , { hash = 3322301467, size = 174964, locked = True, type_ = 'F', name = "Trapper" }
      ]
    )

  , ( "/factory/Synths/Stabs & Pads"
    , [ { hash = 3290855319, size = 1570492, locked = True, type_ = 'F', name = "Alchemistic" }
      , { hash = 2791079433, size = 1060808, locked = True, type_ = 'F', name = "Badlands" }
      , { hash = 2301652721, size = 736216, locked = True, type_ = 'F', name = "Bounce" }
      , { hash = 1571965091, size = 552914, locked = True, type_ = 'F', name = "Bow" }
      , { hash = 1472550181, size = 578212, locked = True, type_ = 'F', name = "Breather" }
      , { hash = 4281766695, size = 993270, locked = True, type_ = 'F', name = "Breeze" }
      , { hash = 3571237717, size = 1198762, locked = True, type_ = 'F', name = "Chillin" }
      , { hash = 4026280497, size = 1277426, locked = True, type_ = 'F', name = "Chopped" }
      , { hash = 1458554725, size = 443584, locked = True, type_ = 'F', name = "Clave" }
      , { hash = 4115439315, size = 244388, locked = True, type_ = 'F', name = "Deep" }
      , { hash = 4057260839, size = 365088, locked = True, type_ = 'F', name = "Disco" }
      , { hash = 2920252495, size = 475662, locked = True, type_ = 'F', name = "Dragged" }
      , { hash = 150255725, size = 774258, locked = True, type_ = 'F', name = "Drifter" }
      , { hash = 4216784643, size = 364154, locked = True, type_ = 'F', name = "Droops" }
      , { hash = 63098675, size = 1162716, locked = True, type_ = 'F', name = "Entrance" }
      , { hash = 42037827, size = 761538, locked = True, type_ = 'F', name = "Fashionable" }
      , { hash = 3229214637, size = 637434, locked = True, type_ = 'F', name = "Fried" }
      , { hash = 1644838621, size = 414630, locked = True, type_ = 'F', name = "Glow" }
      , { hash = 1763656243, size = 676230, locked = True, type_ = 'F', name = "Heatvision" }
      , { hash = 4168905955, size = 1232422, locked = True, type_ = 'F', name = "Highway" }
      , { hash = 4201444237, size = 317572, locked = True, type_ = 'F', name = "Hoover" }
      , { hash = 2723582015, size = 384162, locked = True, type_ = 'F', name = "Icy" }
      , { hash = 443215897, size = 728994, locked = True, type_ = 'F', name = "Judgement" }
      , { hash = 3113606781, size = 346688, locked = True, type_ = 'F', name = "Keys" }
      , { hash = 161431979, size = 177754, locked = True, type_ = 'F', name = "Kicks" }
      , { hash = 3939930923, size = 463344, locked = True, type_ = 'F', name = "Kiss" }
      , { hash = 4196386445, size = 314502, locked = True, type_ = 'F', name = "Legit" }
      , { hash = 253143429, size = 593908, locked = True, type_ = 'F', name = "Lowkey" }
      , { hash = 3784590805, size = 958112, locked = True, type_ = 'F', name = "Opener" }
      , { hash = 3384748687, size = 180824, locked = True, type_ = 'F', name = "Pick" }
      , { hash = 3388636511, size = 2326294, locked = True, type_ = 'F', name = "Plato" }
      , { hash = 185306287, size = 358742, locked = True, type_ = 'F', name = "Pluck" }
      , { hash = 2739520721, size = 192746, locked = True, type_ = 'F', name = "Ripper" }
      , { hash = 1392803545, size = 1119424, locked = True, type_ = 'F', name = "Skip" }
      , { hash = 2127656117, size = 336096, locked = True, type_ = 'F', name = "Slower" }
      , { hash = 39823415, size = 40510, locked = True, type_ = 'F', name = "Stabby" }
      , { hash = 879730295, size = 367226, locked = True, type_ = 'F', name = "Strung" }
      , { hash = 1490147483, size = 424414, locked = True, type_ = 'F', name = "Tennis" }
      , { hash = 327036493, size = 717392, locked = True, type_ = 'F', name = "Thoughtful" }
      , { hash = 3868600511, size = 701740, locked = True, type_ = 'F', name = "Tubular" }
      , { hash = 569815659, size = 195202, locked = True, type_ = 'F', name = "Typical" }
      , { hash = 1933413499, size = 1277722, locked = True, type_ = 'F', name = "UFO" }
      , { hash = 3897813221, size = 1577218, locked = True, type_ = 'F', name = "VCR" }
      , { hash = 3633195799, size = 405976, locked = True, type_ = 'F', name = "Whisp" }
      ]
    )

  , ( "/factory/Toolbox/Noise"
    , [ { hash = 4039745321, size = 168594, locked = True, type_ = 'F', name = "Bassy" }
      , { hash = 2622830837, size = 114422, locked = True, type_ = 'F', name = "Browny" }
      , { hash = 3401550977, size = 212988, locked = True, type_ = 'F', name = "Empty" }
      , { hash = 2891845415, size = 136228, locked = True, type_ = 'F', name = "Glassy" }
      , { hash = 125436621, size = 185986, locked = True, type_ = 'F', name = "Grainy" }
      , { hash = 2287474367, size = 150762, locked = True, type_ = 'F', name = "Pillowy" }
      , { hash = 419550331, size = 93468, locked = True, type_ = 'F', name = "Rainy" }
      , { hash = 3250378627, size = 122778, locked = True, type_ = 'F', name = "Snowy" }
      , { hash = 1386758245, size = 111842, locked = True, type_ = 'F', name = "Staticy" }
      , { hash = 1916836939, size = 229162, locked = True, type_ = 'F', name = "Subtly" }
      , { hash = 1413159997, size = 115592, locked = True, type_ = 'F', name = "Velvety" }
      , { hash = 218569917, size = 63104, locked = True, type_ = 'F', name = "Watery" }
      ]
    )

  , ( "/factory/Toolbox/Oscillators"
    , [ { hash = 1682276933, size = 798, locked = True, type_ = 'F', name = "Acid Org" }
      , { hash = 2589330387, size = 798, locked = True, type_ = 'F', name = "Acid Pul" }
      , { hash = 3061034183, size = 798, locked = True, type_ = 'F', name = "Acid Pul 01" }
      , { hash = 2658197569, size = 798, locked = True, type_ = 'F', name = "Acid Pul 02" }
      , { hash = 417301863, size = 430, locked = True, type_ = 'F', name = "Acid Res" }
      , { hash = 1765186877, size = 798, locked = True, type_ = 'F', name = "Acid Saw" }
      , { hash = 609430587, size = 798, locked = True, type_ = 'F', name = "Acid Squ" }
      , { hash = 3617696827, size = 1532, locked = True, type_ = 'F', name = "Acid Sub 01" }
      , { hash = 1684565289, size = 1534, locked = True, type_ = 'F', name = "Acid Sub 02" }
      , { hash = 1370267899, size = 1526, locked = True, type_ = 'F', name = "Acid Sub 03" }
      , { hash = 1047521157, size = 1526, locked = True, type_ = 'F', name = "Acid Sub 04" }
      , { hash = 1253220473, size = 434, locked = True, type_ = 'F', name = "Amiga" }
      , { hash = 433468097, size = 434, locked = True, type_ = 'F', name = "Amigo" }
      , { hash = 3611061299, size = 800, locked = True, type_ = 'F', name = "Anasine" }
      , { hash = 2170909281, size = 432, locked = True, type_ = 'F', name = "Asqu" }
      , { hash = 2020499967, size = 430, locked = True, type_ = 'F', name = "Brite" }
      , { hash = 2298236823, size = 1530, locked = True, type_ = 'F', name = "Cave" }
      , { hash = 3374734501, size = 800, locked = True, type_ = 'F', name = "Classic" }
      , { hash = 2396923137, size = 430, locked = True, type_ = 'F', name = "Clav" }
      , { hash = 809314885, size = 800, locked = True, type_ = 'F', name = "Comb" }
      , { hash = 2821416049, size = 794, locked = True, type_ = 'F', name = "Curly" }
      , { hash = 1744188503, size = 800, locked = True, type_ = 'F', name = "Dblfold" }
      , { hash = 3130345829, size = 802, locked = True, type_ = 'F', name = "Deep" }
      , { hash = 1841265523, size = 804, locked = True, type_ = 'F', name = "Donk" }
      , { hash = 2792695551, size = 432, locked = True, type_ = 'F', name = "FM 01" }
      , { hash = 4179441521, size = 798, locked = True, type_ = 'F', name = "FM 02" }
      , { hash = 2545299737, size = 430, locked = True, type_ = 'F', name = "FM 03" }
      , { hash = 548688401, size = 430, locked = True, type_ = 'F', name = "FM 04" }
      , { hash = 3436309271, size = 432, locked = True, type_ = 'F', name = "FM 05" }
      , { hash = 4252463139, size = 430, locked = True, type_ = 'F', name = "FM 06" }
      , { hash = 2501376673, size = 798, locked = True, type_ = 'F', name = "FM 07" }
      , { hash = 1021797761, size = 798, locked = True, type_ = 'F', name = "FM 08" }
      , { hash = 646380409, size = 430, locked = True, type_ = 'F', name = "FM 09" }
      , { hash = 1948987125, size = 798, locked = True, type_ = 'F', name = "FM 10" }
      , { hash = 2809314249, size = 798, locked = True, type_ = 'F', name = "FM 11" }
      , { hash = 4071777009, size = 800, locked = True, type_ = 'F', name = "FMish" }
      , { hash = 3421378945, size = 1554, locked = True, type_ = 'F', name = "FMsqu" }
      , { hash = 403932073, size = 798, locked = True, type_ = 'F', name = "Folded 01" }
      , { hash = 269898559, size = 798, locked = True, type_ = 'F', name = "Folded 02" }
      , { hash = 1844019357, size = 798, locked = True, type_ = 'F', name = "Folded 03" }
      , { hash = 3150129045, size = 796, locked = True, type_ = 'F', name = "Folded 04" }
      , { hash = 3940523193, size = 798, locked = True, type_ = 'F', name = "Folded 05" }
      , { hash = 421498827, size = 798, locked = True, type_ = 'F', name = "Folded 06" }
      , { hash = 89549369, size = 798, locked = True, type_ = 'F', name = "Gentle" }
      , { hash = 1586199597, size = 796, locked = True, type_ = 'F', name = "Holloramp" }
      , { hash = 3609746765, size = 804, locked = True, type_ = 'F', name = "Hollosharp" }
      , { hash = 273365721, size = 804, locked = True, type_ = 'F', name = "Hollow" }
      , { hash = 2292988371, size = 798, locked = True, type_ = 'F', name = "Less" }
      , { hash = 2192517691, size = 798, locked = True, type_ = 'F', name = "Low" }
      , { hash = 2760088289, size = 798, locked = True, type_ = 'F', name = "Nasal" }
      , { hash = 1138773239, size = 798, locked = True, type_ = 'F', name = "Oboe" }
      , { hash = 592978639, size = 430, locked = True, type_ = 'F', name = "Organ 01" }
      , { hash = 3792407345, size = 800, locked = True, type_ = 'F', name = "Organ 02" }
      , { hash = 3057632739, size = 430, locked = True, type_ = 'F', name = "Poly" }
      , { hash = 3708392947, size = 800, locked = True, type_ = 'F', name = "Reed" }
      , { hash = 4113958331, size = 430, locked = True, type_ = 'F', name = "Rekt" }
      , { hash = 3364325211, size = 798, locked = True, type_ = 'F', name = "Saw" }
      , { hash = 742777099, size = 798, locked = True, type_ = 'F', name = "Sawfold" }
      , { hash = 363196881, size = 804, locked = True, type_ = 'F', name = "Sharp" }
      , { hash = 644400091, size = 804, locked = True, type_ = 'F', name = "Sharptri" }
      , { hash = 1118264893, size = 798, locked = True, type_ = 'F', name = "Sides" }
      , { hash = 2064812713, size = 798, locked = True, type_ = 'F', name = "Sine" }
      , { hash = 3540098021, size = 434, locked = True, type_ = 'F', name = "Skev" }
      , { hash = 1053183537, size = 796, locked = True, type_ = 'F', name = "Skewed" }
      , { hash = 542598441, size = 806, locked = True, type_ = 'F', name = "Spikes" }
      , { hash = 1219499791, size = 796, locked = True, type_ = 'F', name = "Subosc" }
      , { hash = 375934821, size = 798, locked = True, type_ = 'F', name = "Thick" }
      , { hash = 3860939657, size = 798, locked = True, type_ = 'F', name = "Thin" }
      , { hash = 1416713059, size = 804, locked = True, type_ = 'F', name = "Tones" }
      , { hash = 2739365977, size = 796, locked = True, type_ = 'F', name = "Tri" }
      , { hash = 2822989779, size = 432, locked = True, type_ = 'F', name = "Trifold 01" }
      , { hash = 28735643, size = 804, locked = True, type_ = 'F', name = "Trifold 02" }
      ]
    )

  , ( "/packs/Transistors & Skins/Cosmo CZ Synth"
    , [ { hash = 2376786073, size = 63784, locked = False, type_ = 'F', name = "CZBrokenTom1" }
      , { hash = 569101455, size = 75408, locked = False, type_ = 'F', name = "CZBrokenTom2" }
      , { hash = 1174452605, size = 80614, locked = False, type_ = 'F', name = "CZBrokenTom3" }
      , { hash = 1974046505, size = 88508, locked = False, type_ = 'F', name = "CZBrokenTom4" }
      , { hash = 1798317459, size = 335804, locked = False, type_ = 'F', name = "CZChordMajor" }
      , { hash = 3184592511, size = 378880, locked = False, type_ = 'F', name = "CZChordMinor" }
      , { hash = 2766467119, size = 51046, locked = False, type_ = 'F', name = "CZCosmoTom" }
      , { hash = 199138729, size = 45820, locked = False, type_ = 'F', name = "CZCosmoTom1" }
      , { hash = 2982166967, size = 45850, locked = False, type_ = 'F', name = "CZCosmoTom2" }
      , { hash = 4237177061, size = 47818, locked = False, type_ = 'F', name = "CZCosmoTom3" }
      , { hash = 185550565, size = 49056, locked = False, type_ = 'F', name = "CZCosmoTom4" }
      , { hash = 906793531, size = 55906, locked = False, type_ = 'F', name = "CZCosmoTom5" }
      , { hash = 1327696285, size = 358236, locked = False, type_ = 'F', name = "CZGong1" }
      , { hash = 4076503955, size = 354904, locked = False, type_ = 'F', name = "CZGong2" }
      , { hash = 1142681495, size = 367096, locked = False, type_ = 'F', name = "CZKick+Bass1" }
      , { hash = 1816636201, size = 360686, locked = False, type_ = 'F', name = "CZKick+Bass2" }
      , { hash = 2740620609, size = 99602, locked = False, type_ = 'F', name = "CZKick1" }
      , { hash = 2604268045, size = 16082, locked = False, type_ = 'F', name = "CZKick2" }
      , { hash = 3767545759, size = 63286, locked = False, type_ = 'F', name = "CZKick3" }
      , { hash = 1553636997, size = 67602, locked = False, type_ = 'F', name = "CZKick4" }
      , { hash = 3204292539, size = 113540, locked = False, type_ = 'F', name = "CZKick5" }
      , { hash = 1650306747, size = 94076, locked = False, type_ = 'F', name = "CZKick6" }
      , { hash = 2227611423, size = 96594, locked = False, type_ = 'F', name = "CZNoise1" }
      , { hash = 2754786415, size = 180348, locked = False, type_ = 'F', name = "CZPoing1" }
      , { hash = 3040626147, size = 180348, locked = False, type_ = 'F', name = "CZPoing2" }
      , { hash = 375470051, size = 29304, locked = False, type_ = 'F', name = "CZScratch1" }
      , { hash = 3175287803, size = 42752, locked = False, type_ = 'F', name = "CZScratch2" }
      , { hash = 3389537969, size = 117828, locked = False, type_ = 'F', name = "CZSnare1" }
      , { hash = 4151526279, size = 119774, locked = False, type_ = 'F', name = "CZSnare2" }
      , { hash = 3049780039, size = 23872, locked = False, type_ = 'F', name = "CZSnare3" }
      , { hash = 96121017, size = 96064, locked = False, type_ = 'F', name = "CZSnare4" }
      , { hash = 2886766399, size = 135996, locked = False, type_ = 'F', name = "CZSnare5" }
      , { hash = 485032753, size = 121758, locked = False, type_ = 'F', name = "CZSnare6" }
      , { hash = 1924937733, size = 331844, locked = False, type_ = 'F', name = "CZSpaceHit1" }
      , { hash = 1811817235, size = 331310, locked = False, type_ = 'F', name = "CZSpaceHit2" }
      , { hash = 3493039705, size = 96898, locked = False, type_ = 'F', name = "CZSpaceTom1" }
      , { hash = 1644948835, size = 93990, locked = False, type_ = 'F', name = "CZSpaceTom2" }
      , { hash = 2359923055, size = 92320, locked = False, type_ = 'F', name = "CZSpaceTom3" }
      , { hash = 1081934301, size = 91172, locked = False, type_ = 'F', name = "CZSpaceTom4" }
      , { hash = 3169666953, size = 91172, locked = False, type_ = 'F', name = "CZSpaceTom5" }
      , { hash = 765886749, size = 639668, locked = False, type_ = 'F', name = "CZStupidSound1" }
      , { hash = 3141830785, size = 811500, locked = False, type_ = 'F', name = "CZStupidSound2" }
      , { hash = 3037465907, size = 161032, locked = False, type_ = 'F', name = "CZStupidTom1" }
      , { hash = 3800174793, size = 154146, locked = False, type_ = 'F', name = "CZStupidTom2" }
      ]
    )

  , ( "/packs/Transistors & Skins/Korg Rhythm 55B"
    , [ { hash = 59237965, size = 11988, locked = False, type_ = 'F', name = "ElectricKrrr" }
      , { hash = 268049043, size = 30190, locked = False, type_ = 'F', name = "ElectricTrrr" }
      , { hash = 457556549, size = 138172, locked = False, type_ = 'F', name = "KR55bCymbal1" }
      , { hash = 676518079, size = 136468, locked = False, type_ = 'F', name = "KR55bCymbal2" }
      , { hash = 1312502533, size = 78182, locked = False, type_ = 'F', name = "KR55bHi-Tom1" }
      , { hash = 711894939, size = 96064, locked = False, type_ = 'F', name = "KR55bHi-Tom2" }
      , { hash = 3530500407, size = 91764, locked = False, type_ = 'F', name = "KR55bHi-Tom3" }
      , { hash = 1837542711, size = 22110, locked = False, type_ = 'F', name = "KR55bKick1" }
      , { hash = 3227547965, size = 27624, locked = False, type_ = 'F', name = "KR55bKick2" }
      , { hash = 4199743771, size = 39682, locked = False, type_ = 'F', name = "KR55bKick3" }
      , { hash = 2513956277, size = 27102, locked = False, type_ = 'F', name = "KR55bKick4" }
      , { hash = 3984297225, size = 46486, locked = False, type_ = 'F', name = "KR55bKick5" }
      , { hash = 752871487, size = 98470, locked = False, type_ = 'F', name = "KR55bKick6" }
      , { hash = 2754931009, size = 60234, locked = False, type_ = 'F', name = "KR55bLongHH1" }
      , { hash = 1733710679, size = 60234, locked = False, type_ = 'F', name = "KR55bLongHH2" }
      , { hash = 3416929607, size = 78154, locked = False, type_ = 'F', name = "KR55bLow-Tom1" }
      , { hash = 3153796911, size = 87702, locked = False, type_ = 'F', name = "KR55bLow-tom2" }
      , { hash = 3223589535, size = 89058, locked = False, type_ = 'F', name = "KR55bLow-Tom3" }
      , { hash = 2640524403, size = 13902, locked = False, type_ = 'F', name = "KR55bShortHH1" }
      , { hash = 887673287, size = 22634, locked = False, type_ = 'F', name = "KR55bShortHH2" }
      , { hash = 3380166727, size = 22434, locked = False, type_ = 'F', name = "KR55bSnare1" }
      , { hash = 662815789, size = 20974, locked = False, type_ = 'F', name = "KR55bSnare2" }
      , { hash = 2560359793, size = 22658, locked = False, type_ = 'F', name = "KR55bSnare3" }
      , { hash = 494750777, size = 27826, locked = False, type_ = 'F', name = "KR55bSnare4" }
      , { hash = 1811464197, size = 22434, locked = False, type_ = 'F', name = "KR55bSnare5" }
      , { hash = 1678592011, size = 20682, locked = False, type_ = 'F', name = "KR55bSnare6" }
      , { hash = 2609644229, size = 27450, locked = False, type_ = 'F', name = "KR55bSnare7" }
      , { hash = 242059995, size = 78454, locked = False, type_ = 'F', name = "KR55bSnare8" }
      , { hash = 2378186741, size = 105572, locked = False, type_ = 'F', name = "KR55bSnare9" }
      ]
    )

  , ( "/packs/Transistors & Skins/Nomad Rhythm Maker"
    , [ { hash = 4286493527, size = 8656, locked = False, type_ = 'F', name = "NRMAmped1" }
      , { hash = 3064951533, size = 11122, locked = False, type_ = 'F', name = "NRMAmped2" }
      , { hash = 3307488077, size = 16792, locked = False, type_ = 'F', name = "NRMAmped3" }
      , { hash = 115443479, size = 17570, locked = False, type_ = 'F', name = "NRMAmped4" }
      , { hash = 3763822027, size = 11156, locked = False, type_ = 'F', name = "NRMAmped5" }
      , { hash = 2881072267, size = 5772, locked = False, type_ = 'F', name = "NRMAmped6" }
      , { hash = 3420676385, size = 17686, locked = False, type_ = 'F', name = "NRMAmped7" }
      , { hash = 1506079173, size = 18916, locked = False, type_ = 'F', name = "NRMAmped8" }
      , { hash = 2180230805, size = 31990, locked = False, type_ = 'F', name = "NRMAmped9" }
      , { hash = 3979987019, size = 42590, locked = False, type_ = 'F', name = "NRMAmped10" }
      , { hash = 1132427867, size = 10338, locked = False, type_ = 'F', name = "NRMDirect1" }
      , { hash = 2818931427, size = 18998, locked = False, type_ = 'F', name = "NRMDirect2" }
      , { hash = 3742341005, size = 5186, locked = False, type_ = 'F', name = "NRMDirect3" }
      , { hash = 407106979, size = 6976, locked = False, type_ = 'F', name = "NRMDirect4" }
      , { hash = 3930464319, size = 19194, locked = False, type_ = 'F', name = "NRMDirect5" }
      , { hash = 2340934941, size = 17086, locked = False, type_ = 'F', name = "NRMDirect6" }
      , { hash = 4139633161, size = 15074, locked = False, type_ = 'F', name = "NRMDirect7" }
      , { hash = 3058303519, size = 42714, locked = False, type_ = 'F', name = "NRMDirect8" }
      , { hash = 1790682417, size = 37496, locked = False, type_ = 'F', name = "NRMDirect9" }
      , { hash = 758895925, size = 51588, locked = False, type_ = 'F', name = "NRMDirect10" }
      , { hash = 2965410085, size = 33014, locked = False, type_ = 'F', name = "NRMSwitch" }
      ]
    )

  , ( "/packs/Transistors & Skins/Old Organic Drums"
    , [ { hash = 2455319813, size = 23070, locked = False, type_ = 'F', name = "OrganicBongoHi" }
      , { hash = 2716147801, size = 24168, locked = False, type_ = 'F', name = "OrganicBongoLow" }
      , { hash = 3900249911, size = 54024, locked = False, type_ = 'F', name = "OrganicClap" }
      , { hash = 4271500297, size = 23348, locked = False, type_ = 'F', name = "OrganicClaves" }
      , { hash = 1965322353, size = 923560, locked = False, type_ = 'F', name = "OrganicCrash1" }
      , { hash = 2655844749, size = 915956, locked = False, type_ = 'F', name = "OrganicCrash2" }
      , { hash = 4253827017, size = 983428, locked = False, type_ = 'F', name = "OrganicCrash3" }
      , { hash = 3869408539, size = 905082, locked = False, type_ = 'F', name = "OrganicCrash4" }
      , { hash = 183001955, size = 973100, locked = False, type_ = 'F', name = "OrganicCrash5" }
      , { hash = 1061536827, size = 27720, locked = False, type_ = 'F', name = "OrganicFingers" }
      , { hash = 3860369831, size = 43934, locked = False, type_ = 'F', name = "OrganicHHLong" }
      , { hash = 1358625851, size = 649792, locked = False, type_ = 'F', name = "OrganicHHOpen" }
      , { hash = 2614115335, size = 8492, locked = False, type_ = 'F', name = "OrganicHHShort" }
      , { hash = 235417709, size = 26896, locked = False, type_ = 'F', name = "OrganicHHStomped" }
      , { hash = 2993013657, size = 221764, locked = False, type_ = 'F', name = "OrganicHi-Tom" }
      , { hash = 1451503173, size = 68818, locked = False, type_ = 'F', name = "OrganicKick1" }
      , { hash = 3889023503, size = 126030, locked = False, type_ = 'F', name = "OrganicKick2" }
      , { hash = 621086787, size = 190746, locked = False, type_ = 'F', name = "OrganicKick3" }
      , { hash = 1539865423, size = 143934, locked = False, type_ = 'F', name = "OrganicKick4" }
      , { hash = 4212624157, size = 70082, locked = False, type_ = 'F', name = "OrganicKick5" }
      , { hash = 1982104991, size = 114242, locked = False, type_ = 'F', name = "OrganicKick6" }
      , { hash = 3752321029, size = 152040, locked = False, type_ = 'F', name = "OrganicKick7" }
      , { hash = 309173973, size = 77048, locked = False, type_ = 'F', name = "OrganicKick8" }
      , { hash = 1314169815, size = 289182, locked = False, type_ = 'F', name = "OrganicLow-Tom" }
      , { hash = 2006259753, size = 574980, locked = False, type_ = 'F', name = "OrganicRide1" }
      , { hash = 2431018595, size = 575946, locked = False, type_ = 'F', name = "OrganicRide2" }
      , { hash = 1279511541, size = 31800, locked = False, type_ = 'F', name = "OrganicSnare1" }
      , { hash = 2557887153, size = 28078, locked = False, type_ = 'F', name = "OrganicSnare2" }
      , { hash = 778288205, size = 31888, locked = False, type_ = 'F', name = "OrganicSnare3" }
      , { hash = 2203088525, size = 38270, locked = False, type_ = 'F', name = "OrganicSnare4" }
      , { hash = 1659204427, size = 82524, locked = False, type_ = 'F', name = "OrganicSnare5" }
      , { hash = 3905929715, size = 59316, locked = False, type_ = 'F', name = "OrganicSnare6" }
      , { hash = 524578119, size = 46968, locked = False, type_ = 'F', name = "OrganicSnareBrush" }
      , { hash = 2157568591, size = 218392, locked = False, type_ = 'F', name = "OrganicTimpani1" }
      , { hash = 1374762177, size = 407924, locked = False, type_ = 'F', name = "OrganicTimpani2" }
      , { hash = 3834498657, size = 529694, locked = False, type_ = 'F', name = "TheGoat" }
      ]
    )

  , ( "/packs/Transistors & Skins/Small Percussive Things"
    , [ { hash = 747352719, size = 159874, locked = False, type_ = 'F', name = "Bell1" }
      , { hash = 1622546645, size = 544840, locked = False, type_ = 'F', name = "Bell2" }
      , { hash = 3547764709, size = 529298, locked = False, type_ = 'F', name = "Bell3" }
      , { hash = 1292276269, size = 427072, locked = False, type_ = 'F', name = "Bell4" }
      , { hash = 3533525319, size = 1130050, locked = False, type_ = 'F', name = "Bell5" }
      , { hash = 182233541, size = 269034, locked = False, type_ = 'F', name = "Ehh..._" }
      , { hash = 1989756697, size = 786342, locked = False, type_ = 'F', name = "MetalCrawl" }
      , { hash = 1219023269, size = 515586, locked = False, type_ = 'F', name = "MiniGong1" }
      , { hash = 738089127, size = 484118, locked = False, type_ = 'F', name = "MiniGong2" }
      , { hash = 416193319, size = 383780, locked = False, type_ = 'F', name = "MiniGong3" }
      , { hash = 1810045507, size = 296234, locked = False, type_ = 'F', name = "MiniGong4" }
      , { hash = 3286034491, size = 360060, locked = False, type_ = 'F', name = "MiniGong5" }
      , { hash = 1431586571, size = 470566, locked = False, type_ = 'F', name = "MiniGong6" }
      , { hash = 2032527761, size = 124128, locked = False, type_ = 'F', name = "MiniTambourin1" }
      , { hash = 1573864017, size = 142994, locked = False, type_ = 'F', name = "MiniTambourin2" }
      , { hash = 1888910471, size = 171220, locked = False, type_ = 'F', name = "PureStupidity" }
      , { hash = 2053008501, size = 348274, locked = False, type_ = 'F', name = "SkinCrawl1" }
      , { hash = 2681981577, size = 371230, locked = False, type_ = 'F', name = "SkinCrawl2" }
      , { hash = 4009045095, size = 98202, locked = False, type_ = 'F', name = "Tadada" }
      , { hash = 1720411377, size = 86070, locked = False, type_ = 'F', name = "Tododo" }
      , { hash = 2728071201, size = 387962, locked = False, type_ = 'F', name = "ToyPiano1" }
      , { hash = 164285015, size = 629810, locked = False, type_ = 'F', name = "ToyPiano2" }
      , { hash = 3379236497, size = 683616, locked = False, type_ = 'F', name = "ToyPiano3" }
      , { hash = 2170661745, size = 284962, locked = False, type_ = 'F', name = "ToyPianoLid" }
      , { hash = 3470435293, size = 261572, locked = False, type_ = 'F', name = "WoodMetal1" }
      , { hash = 3521761503, size = 281222, locked = False, type_ = 'F', name = "WoodMetal2" }
      , { hash = 1944068927, size = 240298, locked = False, type_ = 'F', name = "WoodMetal3" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Ambient Kit"
    , [ { hash = 2050951153, size = 322580, locked = True, type_ = 'F', name = "CP Ambient" }
      , { hash = 2823541499, size = 670880, locked = True, type_ = 'F', name = "CY Ambient" }
      , { hash = 714150573, size = 77842, locked = True, type_ = 'F', name = "HH Ambient" }
      , { hash = 2325119539, size = 165280, locked = True, type_ = 'F', name = "HT Ambient" }
      , { hash = 3284752375, size = 185026, locked = True, type_ = 'F', name = "LT Ambient" }
      , { hash = 2751568889, size = 223368, locked = True, type_ = 'F', name = "MT Ambient" }
      , { hash = 2365302761, size = 279854, locked = True, type_ = 'F', name = "SD Ambient" }
      , { hash = 3115627571, size = 352468, locked = True, type_ = 'F', name = "SP Ambient" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Dirty Kit"
    , [ { hash = 1889368597, size = 148864, locked = True, type_ = 'F', name = "BD1 Dirty" }
      , { hash = 3286921735, size = 134464, locked = True, type_ = 'F', name = "BD2 Dirty" }
      , { hash = 2191653637, size = 114600, locked = True, type_ = 'F', name = "CP Dirty" }
      , { hash = 2258613883, size = 889358, locked = True, type_ = 'F', name = "CY Dirty" }
      , { hash = 1673996417, size = 4864, locked = True, type_ = 'F', name = "HH Dirty" }
      , { hash = 3443423789, size = 225944, locked = True, type_ = 'F', name = "LT Dirty" }
      , { hash = 407154483, size = 168064, locked = True, type_ = 'F', name = "MT Dirty" }
      , { hash = 1771213557, size = 172864, locked = True, type_ = 'F', name = "SD Dirty" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Natural Kit"
    , [ { hash = 3562521245, size = 163968, locked = True, type_ = 'F', name = "BD Natural" }
      , { hash = 837916901, size = 537664, locked = True, type_ = 'F', name = "CY Natural" }
      , { hash = 3466689467, size = 102718, locked = True, type_ = 'F', name = "HH Natural" }
      , { hash = 2798978455, size = 190922, locked = True, type_ = 'F', name = "LT Natural" }
      , { hash = 3656039203, size = 153664, locked = True, type_ = 'F', name = "MT Natural" }
      , { hash = 3011210385, size = 785994, locked = True, type_ = 'F', name = "RD Natural" }
      , { hash = 1326871377, size = 133836, locked = True, type_ = 'F', name = "SD Natural" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Punchy Kit"
    , [ { hash = 3712641913, size = 107036, locked = True, type_ = 'F', name = "BD Punchy" }
      , { hash = 3379742077, size = 268960, locked = True, type_ = 'F', name = "BT Punchy" }
      , { hash = 2866172509, size = 138444, locked = True, type_ = 'F', name = "CP Punchy" }
      , { hash = 4067554305, size = 945726, locked = True, type_ = 'F', name = "CY Punchy" }
      , { hash = 2046517889, size = 34624, locked = True, type_ = 'F', name = "HH Punchy" }
      , { hash = 750814917, size = 154484, locked = True, type_ = 'F', name = "HT Punchy" }
      , { hash = 3073566611, size = 213878, locked = True, type_ = 'F', name = "LT Punchy" }
      , { hash = 2233642341, size = 233108, locked = True, type_ = 'F', name = "MT Punchy" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Slapped Perc"
    , [ { hash = 1915578961, size = 802398, locked = True, type_ = 'F', name = "Bendir 1" }
      , { hash = 1264641105, size = 122610, locked = True, type_ = 'F', name = "Bendir 2" }
      , { hash = 40714909, size = 284476, locked = True, type_ = 'F', name = "Bendir 3" }
      , { hash = 2834264035, size = 353376, locked = True, type_ = 'F', name = "Bendir 4" }
      , { hash = 3992904813, size = 294566, locked = True, type_ = 'F', name = "Darbuka Clay Dum 1" }
      , { hash = 3062624705, size = 149202, locked = True, type_ = 'F', name = "Darbuka Clay Dum 2" }
      , { hash = 3755101245, size = 51522, locked = True, type_ = 'F', name = "Darbuka Clay Edge" }
      , { hash = 2747972095, size = 45114, locked = True, type_ = 'F', name = "Darbuka Clay Slap" }
      , { hash = 1119130331, size = 56938, locked = True, type_ = 'F', name = "Darbuka Clay Slide" }
      , { hash = 76153369, size = 157332, locked = True, type_ = 'F', name = "Darbuka Clay Snare 1" }
      , { hash = 2881845251, size = 112534, locked = True, type_ = 'F', name = "Darbuka Clay Snare 2" }
      , { hash = 1991427695, size = 37588, locked = True, type_ = 'F', name = "Darbuka Metal 1" }
      , { hash = 315126721, size = 14392, locked = True, type_ = 'F', name = "Darbuka Metal 2" }
      , { hash = 735793217, size = 105362, locked = True, type_ = 'F', name = "Darbuka Metal 3" }
      , { hash = 3187622159, size = 64284, locked = True, type_ = 'F', name = "Darbuka Metal 4" }
      , { hash = 321514109, size = 41328, locked = True, type_ = 'F', name = "Finger Cymbal" }
      , { hash = 1101830089, size = 358224, locked = True, type_ = 'F', name = "Steinn Noisy" }
      , { hash = 2855542173, size = 229616, locked = True, type_ = 'F', name = "Steinn Rim" }
      , { hash = 2001566777, size = 277878, locked = True, type_ = 'F', name = "Steinn Snare" }
      , { hash = 3114268589, size = 220002, locked = True, type_ = 'F', name = "Tambourine 1" }
      , { hash = 307422735, size = 81398, locked = True, type_ = 'F', name = "Tambourine 2" }
      , { hash = 4132308459, size = 38128, locked = True, type_ = 'F', name = "Tambourine 3" }
      , { hash = 3560641805, size = 76732, locked = True, type_ = 'F', name = "Tambourine 4" }
      , { hash = 912332963, size = 55790, locked = True, type_ = 'F', name = "Tambourine 5" }
      , { hash = 2376574831, size = 92814, locked = True, type_ = 'F', name = "Tombak Bass" }
      , { hash = 3465853963, size = 75370, locked = True, type_ = 'F', name = "Tombak Double" }
      , { hash = 4085146651, size = 27612, locked = True, type_ = 'F', name = "Tombak Edge" }
      , { hash = 2082981623, size = 44786, locked = True, type_ = 'F', name = "Tombak Fingers" }
      , { hash = 3830147603, size = 65706, locked = True, type_ = 'F', name = "Tombak Knock" }
      , { hash = 3554105601, size = 69102, locked = True, type_ = 'F', name = "Tombak Rim" }
      , { hash = 1336960345, size = 85202, locked = True, type_ = 'F', name = "Tombak Snare" }
      , { hash = 2222839691, size = 80008, locked = True, type_ = 'F', name = "Tombak Tom" }
      ]
    )

  , ( "/factory/Drums/Electronic/Abrasive"
    , [ { hash = 453629625, size = 96058, locked = True, type_ = 'F', name = "BD1 Abrasive" }
      , { hash = 3960220237, size = 96058, locked = True, type_ = 'F', name = "BD2 Abrasive" }
      , { hash = 1721717317, size = 120064, locked = True, type_ = 'F', name = "CB1 Abrasive" }
      , { hash = 3783147935, size = 186526, locked = True, type_ = 'F', name = "CB2 Abrasive" }
      , { hash = 496756547, size = 144064, locked = True, type_ = 'F', name = "MT1 Abrasive" }
      , { hash = 1315514127, size = 90528, locked = True, type_ = 'F', name = "MT2 Abrasive" }
      , { hash = 1893069493, size = 114932, locked = True, type_ = 'F', name = "MT3 Abrasive" }
      , { hash = 1780918391, size = 42498, locked = True, type_ = 'F', name = "PC1 Abrasive" }
      , { hash = 3126209245, size = 90348, locked = True, type_ = 'F', name = "PC2 Abrasive" }
      , { hash = 2687881419, size = 186524, locked = True, type_ = 'F', name = "PC3 Abrasive" }
      , { hash = 2429442999, size = 114504, locked = True, type_ = 'F', name = "PC4 Abrasive" }
      , { hash = 2202246649, size = 90504, locked = True, type_ = 'F', name = "SD1 Abrasive" }
      , { hash = 4146646443, size = 89498, locked = True, type_ = 'F', name = "SD2 Abrasive" }
      , { hash = 3242562037, size = 90520, locked = True, type_ = 'F', name = "SD3 Abrasive" }
      , { hash = 2769963285, size = 138522, locked = True, type_ = 'F', name = "SD4 Abrasive" }
      , { hash = 793697517, size = 42522, locked = True, type_ = 'F', name = "SD5 Abrasive" }
      ]
    )

  , ( "/factory/Drums/Electronic/Adlib"
    , [ { hash = 2775936515, size = 74610, locked = True, type_ = 'F', name = "BD Adlib" }
      , { hash = 3772289645, size = 29304, locked = True, type_ = 'F', name = "CB Adlib" }
      , { hash = 948130245, size = 4566, locked = True, type_ = 'F', name = "CL Adlib" }
      , { hash = 2958702495, size = 245112, locked = True, type_ = 'F', name = "CY Adlib" }
      , { hash = 1272073153, size = 21270, locked = True, type_ = 'F', name = "HH Adlib" }
      , { hash = 246116297, size = 56684, locked = True, type_ = 'F', name = "MT Adlib" }
      , { hash = 863475037, size = 4146, locked = True, type_ = 'F', name = "RS Adlib" }
      , { hash = 3879522185, size = 55528, locked = True, type_ = 'F', name = "SD Adlib" }
      ]
    )

  , ( "/factory/Drums/Electronic/Booming"
    , [ { hash = 1053062609, size = 192064, locked = True, type_ = 'F', name = "BD Booming" }
      , { hash = 3789103107, size = 42512, locked = True, type_ = 'F', name = "CL Booming" }
      , { hash = 145937297, size = 90510, locked = True, type_ = 'F', name = "CP Booming" }
      , { hash = 2762217321, size = 45684, locked = True, type_ = 'F', name = "HH Booming" }
      , { hash = 2533447415, size = 146444, locked = True, type_ = 'F', name = "MT Booming" }
      , { hash = 1950650969, size = 93580, locked = True, type_ = 'F', name = "OH Booming" }
      , { hash = 1277419129, size = 42502, locked = True, type_ = 'F', name = "RS Booming" }
      , { hash = 3059234655, size = 42516, locked = True, type_ = 'F', name = "SD Booming" }
      ]
    )

  , ( "/factory/Drums/Electronic/Bronze"
    , [ { hash = 3622389989, size = 123200, locked = True, type_ = 'F', name = "BD1 Bronze" }
      , { hash = 1458481175, size = 84698, locked = True, type_ = 'F', name = "BD2 Bronze" }
      , { hash = 269062041, size = 123542, locked = True, type_ = 'F', name = "CP Bronze" }
      , { hash = 3092479575, size = 208406, locked = True, type_ = 'F', name = "CY Bronze" }
      , { hash = 1423971365, size = 61054, locked = True, type_ = 'F', name = "HH Bronze" }
      , { hash = 1707867849, size = 143474, locked = True, type_ = 'F', name = "MT Bronze" }
      , { hash = 726283775, size = 101260, locked = True, type_ = 'F', name = "RS Bronze" }
      , { hash = 3562143453, size = 122854, locked = True, type_ = 'F', name = "ST Bronze" }
      ]
    )

  , ( "/factory/Drums/Electronic/Buggin"
    , [ { hash = 44887955, size = 36062, locked = True, type_ = 'F', name = "BD1 Buggin" }
      , { hash = 174180907, size = 108062, locked = True, type_ = 'F', name = "BD2 Buggin" }
      , { hash = 4023800719, size = 59286, locked = True, type_ = 'F', name = "HH1 Buggin" }
      , { hash = 1797563723, size = 56734, locked = True, type_ = 'F', name = "HH2 Buggin" }
      , { hash = 4261459963, size = 98596, locked = True, type_ = 'F', name = "MT Buggin" }
      , { hash = 1476154435, size = 46750, locked = True, type_ = 'F', name = "RS Buggin" }
      , { hash = 2331312373, size = 45470, locked = True, type_ = 'F', name = "SD1 Buggin" }
      , { hash = 2311616311, size = 45594, locked = True, type_ = 'F', name = "SD2 Buggin" }
      ]
    )

  , ( "/factory/Drums/Electronic/Cable"
    , [ { hash = 632705171, size = 144062, locked = True, type_ = 'F', name = "BD Cable" }
      , { hash = 1893356817, size = 30528, locked = True, type_ = 'F', name = "CA Cable" }
      , { hash = 4113689349, size = 39576, locked = True, type_ = 'F', name = "HH Cable" }
      , { hash = 4075275119, size = 47390, locked = True, type_ = 'F', name = "MT Cable" }
      , { hash = 4076747997, size = 57998, locked = True, type_ = 'F', name = "OH Cable" }
      , { hash = 3181929769, size = 15134, locked = True, type_ = 'F', name = "RS Cable" }
      , { hash = 846830385, size = 44704, locked = True, type_ = 'F', name = "SD Cable" }
      ]
    )

  , ( "/factory/Drums/Electronic/Class"
    , [ { hash = 1378410779, size = 96048, locked = True, type_ = 'F', name = "BD Class" }
      , { hash = 1051298389, size = 42110, locked = True, type_ = 'F', name = "CB Class" }
      , { hash = 1139728051, size = 42112, locked = True, type_ = 'F', name = "CL Class" }
      , { hash = 1199572459, size = 26308, locked = True, type_ = 'F', name = "CP Class" }
      , { hash = 1213275125, size = 15068, locked = True, type_ = 'F', name = "HH Class" }
      , { hash = 2755882515, size = 45818, locked = True, type_ = 'F', name = "MT Class" }
      , { hash = 1540291755, size = 66526, locked = True, type_ = 'F', name = "OH Class" }
      , { hash = 2099150655, size = 42542, locked = True, type_ = 'F', name = "SD Class" }
      ]
    )

  , ( "/factory/Drums/Electronic/Digit"
    , [ { hash = 1139277629, size = 144064, locked = True, type_ = 'F', name = "BD Digit" }
      , { hash = 3996165251, size = 42530, locked = True, type_ = 'F', name = "CB Digit" }
      , { hash = 2368422339, size = 33192, locked = True, type_ = 'F', name = "CP Digit" }
      , { hash = 1187120671, size = 204952, locked = True, type_ = 'F', name = "CY Digit" }
      , { hash = 1809077313, size = 54520, locked = True, type_ = 'F', name = "HH Digit" }
      , { hash = 2276121603, size = 62368, locked = True, type_ = 'F', name = "MT Digit" }
      , { hash = 2165069335, size = 72514, locked = True, type_ = 'F', name = "OH Digit" }
      , { hash = 348138077, size = 45082, locked = True, type_ = 'F', name = "SD Digit" }
      ]
    )

  , ( "/factory/Drums/Electronic/Don"
    , [ { hash = 1527052849, size = 191914, locked = True, type_ = 'F', name = "BT Don" }
      , { hash = 3794808131, size = 109518, locked = True, type_ = 'F', name = "CP Don" }
      , { hash = 3498187173, size = 43568, locked = True, type_ = 'F', name = "HH1 Don" }
      , { hash = 3937773429, size = 43848, locked = True, type_ = 'F', name = "HH2 Don" }
      , { hash = 1645256439, size = 151474, locked = True, type_ = 'F', name = "HT Don" }
      , { hash = 2078484653, size = 155152, locked = True, type_ = 'F', name = "LT Don" }
      , { hash = 2175702251, size = 125890, locked = True, type_ = 'F', name = "MT Don" }
      , { hash = 1972745265, size = 188684, locked = True, type_ = 'F', name = "NS1 Don" }
      , { hash = 3597379659, size = 174932, locked = True, type_ = 'F', name = "NS2 Don" }
      , { hash = 2093133419, size = 94456, locked = True, type_ = 'F', name = "RS Don" }
      , { hash = 3327397857, size = 113252, locked = True, type_ = 'F', name = "SD1 Don" }
      , { hash = 3913926295, size = 140876, locked = True, type_ = 'F', name = "SD2 Don" }
      , { hash = 712963073, size = 191898, locked = True, type_ = 'F', name = "SD3 Don" }
      ]
    )

  , ( "/factory/Drums/Electronic/Fella"
    , [ { hash = 2755333529, size = 107966, locked = True, type_ = 'F', name = "BD1 Fella" }
      , { hash = 4060939691, size = 281410, locked = True, type_ = 'F', name = "BD2 Fella" }
      , { hash = 1085669151, size = 98202, locked = True, type_ = 'F', name = "BD3 Fella" }
      , { hash = 4279226129, size = 262720, locked = True, type_ = 'F', name = "BD4 Fella" }
      , { hash = 1718001001, size = 17570, locked = True, type_ = 'F', name = "CP Fella" }
      , { hash = 2886884383, size = 257188, locked = True, type_ = 'F', name = "CY Fella" }
      , { hash = 423582397, size = 145062, locked = True, type_ = 'F', name = "HH Fella" }
      , { hash = 2234964977, size = 145062, locked = True, type_ = 'F', name = "HH2 Fella" }
      , { hash = 211582373, size = 103038, locked = True, type_ = 'F', name = "MT Fella" }
      , { hash = 2654504729, size = 104098, locked = True, type_ = 'F', name = "OH Fella" }
      , { hash = 4149900063, size = 180062, locked = True, type_ = 'F', name = "OH2 Fella" }
      , { hash = 3609738979, size = 84802, locked = True, type_ = 'F', name = "PC1 Fella" }
      , { hash = 1948795943, size = 131738, locked = True, type_ = 'F', name = "PC2 Fella" }
      , { hash = 2757327865, size = 135810, locked = True, type_ = 'F', name = "PC3 Fella" }
      , { hash = 3299472905, size = 25630, locked = True, type_ = 'F', name = "RS Fella" }
      , { hash = 3456512597, size = 80274, locked = True, type_ = 'F', name = "SD1 Fella" }
      , { hash = 2635909905, size = 74390, locked = True, type_ = 'F', name = "SD2 Fella" }
      , { hash = 1352455847, size = 51362, locked = True, type_ = 'F', name = "SD3 Fella" }
      , { hash = 3797722685, size = 45430, locked = True, type_ = 'F', name = "SD4 Fella" }
      , { hash = 1433464391, size = 168096, locked = True, type_ = 'F', name = "SP Fella" }
      ]
    )

  , ( "/factory/Drums/Electronic/Flange"
    , [ { hash = 732887511, size = 102424, locked = True, type_ = 'F', name = "BD1 Flange" }
      , { hash = 1267711783, size = 166938, locked = True, type_ = 'F', name = "BD2 Flange" }
      , { hash = 610668523, size = 158744, locked = True, type_ = 'F', name = "BD3 Flange" }
      , { hash = 2090756357, size = 101652, locked = True, type_ = 'F', name = "BD4 Flange" }
      , { hash = 2106932801, size = 277118, locked = True, type_ = 'F', name = "CY Flange" }
      , { hash = 675521171, size = 138808, locked = True, type_ = 'F', name = "HH1 Flange" }
      , { hash = 2750077833, size = 247428, locked = True, type_ = 'F', name = "HH2 Flange" }
      , { hash = 1740017081, size = 157312, locked = True, type_ = 'F', name = "HH3 Flange" }
      , { hash = 803738087, size = 219260, locked = True, type_ = 'F', name = "MT Flange" }
      , { hash = 2950757065, size = 221312, locked = True, type_ = 'F', name = "PC1 Flange" }
      , { hash = 3963211645, size = 251000, locked = True, type_ = 'F', name = "PC2 Flange" }
      , { hash = 951698201, size = 205438, locked = True, type_ = 'F', name = "PC3 Flange" }
      , { hash = 293573243, size = 126076, locked = True, type_ = 'F', name = "SD1 Flange" }
      , { hash = 758389503, size = 124026, locked = True, type_ = 'F', name = "SD2 Flange" }
      , { hash = 2903291537, size = 188540, locked = True, type_ = 'F', name = "SD3 Flange" }
      , { hash = 463948859, size = 188028, locked = True, type_ = 'F', name = "SD4 Flange" }
      ]
    )

  , ( "/factory/Drums/Electronic/Pearl"
    , [ { hash = 3994081591, size = 96064, locked = True, type_ = 'F', name = "BD Pearl" }
      , { hash = 3127917223, size = 236226, locked = True, type_ = 'F', name = "CB Pearl" }
      , { hash = 2894119139, size = 20064, locked = True, type_ = 'F', name = "CL Pearl" }
      , { hash = 1329288469, size = 34478, locked = True, type_ = 'F', name = "LT Pearl" }
      , { hash = 4022162503, size = 50064, locked = True, type_ = 'F', name = "PC Pearl" }
      , { hash = 2579147773, size = 10590, locked = True, type_ = 'F', name = "RS Pearl" }
      , { hash = 1566049591, size = 20064, locked = True, type_ = 'F', name = "SD Pearl" }
      , { hash = 3423792885, size = 188058, locked = True, type_ = 'F', name = "SY Pearl" }
      ]
    )

  , ( "/factory/Drums/Electronic/Puff"
    , [ { hash = 1783618171, size = 23424, locked = True, type_ = 'F', name = "BD1 Puff" }
      , { hash = 3170911575, size = 192058, locked = True, type_ = 'F', name = "BD2 Puff" }
      , { hash = 3129314493, size = 21560, locked = True, type_ = 'F', name = "CB Puff" }
      , { hash = 3255091671, size = 16480, locked = True, type_ = 'F', name = "CL Puff" }
      , { hash = 359674347, size = 66522, locked = True, type_ = 'F', name = "HH Puff" }
      , { hash = 713601405, size = 44694, locked = True, type_ = 'F', name = "HT Puff" }
      , { hash = 461843749, size = 45468, locked = True, type_ = 'F', name = "MT Puff" }
      , { hash = 3510175877, size = 45988, locked = True, type_ = 'F', name = "SD Puff" }
      ]
    )

  , ( "/factory/Drums/Electronic/Reson"
    , [ { hash = 3424937007, size = 61726, locked = True, type_ = 'F', name = "BD Reson" }
      , { hash = 1016625579, size = 58786, locked = True, type_ = 'F', name = "HH Reson" }
      , { hash = 4012954605, size = 142712, locked = True, type_ = 'F', name = "MT Reson" }
      , { hash = 523044043, size = 184978, locked = True, type_ = 'F', name = "OH Reson" }
      , { hash = 633015141, size = 165536, locked = True, type_ = 'F', name = "PC Reson" }
      , { hash = 3711235203, size = 80544, locked = True, type_ = 'F', name = "RS Reson" }
      , { hash = 340312669, size = 101022, locked = True, type_ = 'F', name = "SD1 Reson" }
      , { hash = 1972269489, size = 38810, locked = True, type_ = 'F', name = "SD2 Reson" }
      ]
    )

  , ( "/factory/Drums/Electronic/Rocking"
    , [ { hash = 1602387793, size = 75964, locked = True, type_ = 'F', name = "BD Rocking" }
      , { hash = 2417978701, size = 9160, locked = True, type_ = 'F', name = "CB Rocking" }
      , { hash = 4029245541, size = 51724, locked = True, type_ = 'F', name = "CP Rocking" }
      , { hash = 2681454377, size = 191998, locked = True, type_ = 'F', name = "CY Rocking" }
      , { hash = 1958464499, size = 23608, locked = True, type_ = 'F', name = "HH Rocking" }
      , { hash = 302632397, size = 87382, locked = True, type_ = 'F', name = "HT Rocking" }
      , { hash = 4258241149, size = 92678, locked = True, type_ = 'F', name = "LT Rocking" }
      , { hash = 222327243, size = 79404, locked = True, type_ = 'F', name = "SD Rocking" }
      ]
    )

  , ( "/factory/Drums/Electronic/Rom"
    , [ { hash = 1962983837, size = 60062, locked = True, type_ = 'F', name = "BD Rom" }
      , { hash = 3759519671, size = 192168, locked = True, type_ = 'F', name = "CY Rom" }
      , { hash = 2830362537, size = 45734, locked = True, type_ = 'F', name = "HH Rom" }
      , { hash = 2623687467, size = 54540, locked = True, type_ = 'F', name = "MT Rom" }
      , { hash = 3664353819, size = 96932, locked = True, type_ = 'F', name = "OH Rom" }
      , { hash = 516420319, size = 141606, locked = True, type_ = 'F', name = "RD Rom" }
      , { hash = 690505565, size = 26060, locked = True, type_ = 'F', name = "RS Rom" }
      , { hash = 3644178745, size = 46764, locked = True, type_ = 'F', name = "SD Rom" }
      ]
    )

  , ( "/factory/Drums/Electronic/School"
    , [ { hash = 2824609755, size = 34542, locked = True, type_ = 'F', name = "BD School" }
      , { hash = 563098601, size = 14010, locked = True, type_ = 'F', name = "HH1 School" }
      , { hash = 584846149, size = 25758, locked = True, type_ = 'F', name = "HH2 School" }
      , { hash = 2950770455, size = 19296, locked = True, type_ = 'F', name = "OH School" }
      , { hash = 2803681789, size = 48004, locked = True, type_ = 'F', name = "PC1 School" }
      , { hash = 729026127, size = 24994, locked = True, type_ = 'F', name = "PC2 School" }
      , { hash = 1135720051, size = 20630, locked = True, type_ = 'F', name = "RS School" }
      , { hash = 1240809129, size = 49064, locked = True, type_ = 'F', name = "SD School" }
      ]
    )

  , ( "/factory/Drums/Electronic/Soft"
    , [ { hash = 3322801105, size = 72056, locked = True, type_ = 'F', name = "BD Soft" }
      , { hash = 40327539, size = 43680, locked = True, type_ = 'F', name = "CB Soft" }
      , { hash = 1852776721, size = 11224, locked = True, type_ = 'F', name = "CL Soft" }
      , { hash = 718209921, size = 141434, locked = True, type_ = 'F', name = "CY Soft" }
      , { hash = 2933206711, size = 70294, locked = True, type_ = 'F', name = "HH Soft" }
      , { hash = 3955188807, size = 90510, locked = True, type_ = 'F', name = "MT Soft" }
      , { hash = 260379633, size = 27920, locked = True, type_ = 'F', name = "RS Soft" }
      , { hash = 2750506331, size = 90534, locked = True, type_ = 'F', name = "SD Soft" }
      ]
    )

  , ( "/factory/Drums/Electronic/Spring"
    , [ { hash = 2168370381, size = 192064, locked = True, type_ = 'F', name = "BD Spring" }
      , { hash = 422746755, size = 95402, locked = True, type_ = 'F', name = "CL Spring" }
      , { hash = 686550177, size = 345836, locked = True, type_ = 'F', name = "PC1 Spring" }
      , { hash = 3958828027, size = 389092, locked = True, type_ = 'F', name = "PC2 Spring" }
      , { hash = 746356413, size = 192170, locked = True, type_ = 'F', name = "SD1 Spring" }
      , { hash = 959451841, size = 289448, locked = True, type_ = 'F', name = "SD2 Spring" }
      , { hash = 2352742687, size = 332494, locked = True, type_ = 'F', name = "SY1 Spring" }
      , { hash = 2349030453, size = 196184, locked = True, type_ = 'F', name = "SY2 Spring" }
      ]
    )

  , ( "/factory/Drums/Electronic/Stonk"
    , [ { hash = 889809715, size = 87600, locked = True, type_ = 'F', name = "BD Stonk" }
      , { hash = 3044515919, size = 88210, locked = True, type_ = 'F', name = "CB Stonk" }
      , { hash = 144274987, size = 132768, locked = True, type_ = 'F', name = "HH Stonk" }
      , { hash = 760282299, size = 138744, locked = True, type_ = 'F', name = "HT Stonk" }
      , { hash = 4218933827, size = 138874, locked = True, type_ = 'F', name = "MT Stonk" }
      , { hash = 1812466605, size = 25662, locked = True, type_ = 'F', name = "RS Stonk" }
      , { hash = 1643468379, size = 42662, locked = True, type_ = 'F', name = "SD Stonk" }
      ]
    )

  , ( "/factory/Drums/Electronic/Trunk"
    , [ { hash = 1006065941, size = 88586, locked = True, type_ = 'F', name = "BD Trunk" }
      , { hash = 905539497, size = 40334, locked = True, type_ = 'F', name = "CP Trunk" }
      , { hash = 2426931503, size = 86424, locked = True, type_ = 'F', name = "MC Trunk" }
      , { hash = 2303669155, size = 173770, locked = True, type_ = 'F', name = "OH Trunk" }
      , { hash = 1480870903, size = 130684, locked = True, type_ = 'F', name = "PC Trunk" }
      , { hash = 1473935865, size = 15204, locked = True, type_ = 'F', name = "RS Trunkl" }
      , { hash = 2843490465, size = 41638, locked = True, type_ = 'F', name = "SD Trunk" }
      ]
    )

  , ( "/factory/Drums/Electronic/Velvet"
    , [ { hash = 1715372189, size = 57664, locked = True, type_ = 'F', name = "BD Velvet" }
      , { hash = 1472057673, size = 45566, locked = True, type_ = 'F', name = "CB Velvet" }
      , { hash = 2399537585, size = 52136, locked = True, type_ = 'F', name = "CP Velvet" }
      , { hash = 1750675801, size = 115320, locked = True, type_ = 'F', name = "HH1 Velvet" }
      , { hash = 38392563, size = 82590, locked = True, type_ = 'F', name = "HH2 Velvet" }
      , { hash = 1990961525, size = 305828, locked = True, type_ = 'F', name = "LT Velvet" }
      , { hash = 3114111797, size = 114978, locked = True, type_ = 'F', name = "MT Velvet" }
      , { hash = 1594888231, size = 55972, locked = True, type_ = 'F', name = "RS Velvet" }
      , { hash = 2621616677, size = 38570, locked = True, type_ = 'F', name = "SD Velvet" }
      ]
    )

  , ( "/factory/Drums/Electronic/Weird"
    , [ { hash = 3925089181, size = 96064, locked = True, type_ = 'F', name = "BD1 Weird" }
      , { hash = 1339113459, size = 48064, locked = True, type_ = 'F', name = "BD2 Weird" }
      , { hash = 3143110323, size = 42524, locked = True, type_ = 'F', name = "CP Weird" }
      , { hash = 3089904961, size = 48538, locked = True, type_ = 'F', name = "HH1 Weird" }
      , { hash = 3603257391, size = 45734, locked = True, type_ = 'F', name = "HH2 Weird" }
      , { hash = 2254360247, size = 23124, locked = True, type_ = 'F', name = "RS Weird" }
      , { hash = 2143599789, size = 42482, locked = True, type_ = 'F', name = "SD1 Weird" }
      , { hash = 2182760157, size = 54592, locked = True, type_ = 'F', name = "SD2 Weird" }
      ]
    )

  ]

