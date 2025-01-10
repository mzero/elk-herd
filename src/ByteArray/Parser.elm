module ByteArray.Parser exposing
  ( Parser
  , parse

  , succeed
  , fail
  , result

  , andThen

  , maybe
  , oneOf
  , repeat
  , repeatToEnd

  , map
  , map2
  , map3
  , map4
  , map5
  , map6

  , Mark
  , mark
  , jump
  , skip
  , upToOffset

  , atEnd
  , rest

  , byte
  , bytes
  , upToByte

  , expectByte
  , expectList

  , uint8
  , uint16be
  , uint32be
  , string0ascii
  , string0win1252
  )

{-| A simple, standard monaic parser combintor library over `ByteArray` objects.
-}

import ByteArray exposing (ByteArray)
import ByteArray.String

type alias State = { data : ByteArray, pos : Int }
type alias Err = String

type Parser r = P (State -> Result Err (State, r))


parse : Parser r -> ByteArray -> Result Err r
parse (P pfn) ba = Result.map Tuple.second <| pfn { data = ba, pos = 0 }


succeed : r -> Parser r
succeed r = P (\st -> Ok (st, r))

fail : String -> Parser a
fail err = P (\_ -> Err err)

result : Result String r -> Parser r
result res = case res of
  Ok r -> succeed r
  Err err -> fail err


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen fpb (P pafn) = P (\st ->
  case pafn st of
    Ok (st_, a) ->
      case fpb a of
        P pbfn -> pbfn st_
    Err err ->
      Err err
  )


maybe : Parser a -> Parser (Maybe a)
maybe (P pafn) = P (\st ->
  case pafn st of
    Ok (st_, a) -> Ok (st_, Just a)
    Err err -> Err err
  )

oneOf : List (Parser a) -> Parser a
oneOf =
  let
    go ps st = case ps of
      [] -> Err "oneOf exhausted"
      (P pafn) :: ps_rest -> case pafn st of
        Ok st_r -> Ok st_r
        Err _ -> go ps_rest st
  in
    P << go

repeat : Int -> Parser a -> Parser (List a)
repeat n (P pafn) =
  let
    go i st rs =
      if i == 0
        then Ok (st, List.reverse rs)
        else case pafn st of
          Ok (st_, r) -> go (i - 1) st_ (r :: rs)
          Err err -> Err err
  in
    P (\st -> go n st [])

repeatToEnd : Parser a -> Parser (List a)
repeatToEnd (P pafn) =
  let
    go st rs =
      if st.pos >= ByteArray.length st.data
        then Ok (st, List.reverse rs)
        else case pafn st of
          Ok (st_, r) -> go st_ (r :: rs)
          Err err -> Err err
  in
    P (\st -> go st [])


map : (a -> r) -> Parser a -> Parser r
map f pa = pa |> andThen (\a -> succeed <| f a)

map2 : (a -> b -> r) -> Parser a -> Parser b -> Parser r
map2 f pa pb = pa |> andThen (\a -> map (f a) pb)

map3 : (a -> b -> c -> r) -> Parser a -> Parser b -> Parser c -> Parser r
map3 f pa pb pc = pa |> andThen (\a -> map2 (f a) pb pc)

map4 : (a -> b -> c -> d -> r)
  -> Parser a -> Parser b -> Parser c -> Parser d -> Parser r
map4 f pa pb pc pd = pa |> andThen (\a -> map3 (f a) pb pc pd)

map5 : (a -> b -> c -> d -> e -> r)
  -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser r
map5 f pa pb pc pd pe = pa |> andThen (\a -> map4 (f a) pb pc pd pe)

map6 : (a -> b -> c -> d -> e -> f -> r)
  -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
  -> Parser r
map6 f pa pb pc pd pe pf = pa |> andThen (\a -> map5 (f a) pb pc pd pe pf)


{-| An opaque marker for a point in a parse.

Note: This is an experimental feature, and the API may change considerably.
-}
type Mark = Mark State

{-| Returns a `Mark` for the current place in the parse.
-}
mark : Parser Mark
mark = P (\st -> Ok (st, Mark st))

{-| Returns the parser to the place in the `ByteArray` where it was at the time
of the `Mark`. The parser will proceed from there, parsing those bytes for a
second time.
-}
jump : Mark -> Parser ()
jump (Mark m) = P (\st ->
  if m.data == st.data -- this will be cheap if they are, as expected, the same
    then Ok (m, ())
    else Err "jump to mark from another parse"
  )

{-| Simply skips forward n bytes. Even though the bytes are dropped, it is an
error if there isn't at least n bytes left in the `ByteArray`.
-}
skip : Int -> Parser ()
skip n = P (\st ->
  let
    pos = st.pos + n
  in
    if (pos < st.pos)
      then Err "backward skip"
      else
        if pos > ByteArray.length st.data
          then Ok ({ st | pos = pos }, ())
          else Err "skip past end"
  )

{-| Returns all the bytes from where the parser is right now, up to some offset
past a previous mark. The idea is you take a mark at the start at the start of
a structure, parse some stuff, then need to skip forward to some offset, saving
the skipped bytes.
-}
upToOffset : Mark -> Int -> Parser ByteArray
upToOffset (Mark m) n = P (\st ->
  if m.data /= st.data           -- cheap if the same, as expected
    then Err "marker from a different parse sequence"
    else
      let
        pos = m.pos + n
      in
        if pos < st.pos
          then Err "parsing has proceeded beyond offset"
          else if (pos > ByteArray.length st.data)
            then Err "offset is beyond the end of data"
            else Ok ({ st | pos = pos },
                    ByteArray.section st.pos (pos - st.pos) st.data)
  )


atEnd : Parser ()
atEnd = P (\st ->
  if st.pos >= ByteArray.length st.data
    then Ok (st, ())
    else Err "not at end"
  )

rest : Parser ByteArray
rest = P (\st ->
  let
    end = ByteArray.length st.data
    start = min st.pos end
    len = end - start
  in
    Ok ({ st | pos = end }, ByteArray.section start len st.data)
  )

byte : Parser Int
byte = P (\st ->
  case ByteArray.get st.pos st.data of
    Just b -> Ok ({ st | pos = st.pos+1 }, b)
    Nothing -> Err "no more data"
  )

bytes : Int -> Parser ByteArray
bytes n = P (\st ->
  let
    start = st.pos
    end = start + n
  in
    if end <= ByteArray.length st.data
      then Ok ({ st | pos = end }, ByteArray.section start n st.data)
      else Err <| "fewer than " ++ String.fromInt n ++ " bytes left"
  )

upToByte : Int -> Parser ByteArray
upToByte marker = P (\st ->
  let
    find i = case ByteArray.get i st.data of
      Just b -> if b == marker then Just i else find (i+1)
      Nothing -> Nothing
    idx = find st.pos
  in
    case find st.pos of
      Just i ->
        Ok ({ st | pos = i+1 }, ByteArray.section st.pos (i - st.pos) st.data)
      Nothing ->
        Err <| "can't find marker byte " ++ String.fromInt marker
  )

expectByte : Int -> Parser ()
expectByte match =
  byte |> andThen (\b ->
    if b == match
      then succeed ()
      else fail <| "expecting " ++ String.fromInt match
    )

expectList : List Int -> Parser ()
expectList match =
  bytes (List.length match) |> andThen (\bs ->
    if ByteArray.toList bs == match
      then succeed ()
      else fail <| "expecting "
                     ++ String.join "," (List.map String.fromInt match)
    )

uint8 : Parser Int
uint8 = byte

uint16be : Parser Int
uint16be = map2 (\hi lo -> 256 * hi + lo) byte byte

uint32be : Parser Int
uint32be = map2 (\hi lo -> 65536 * hi + lo) uint16be uint16be

string0ascii : Parser String
string0ascii = map ByteArray.String.decodeAscii <| upToByte 0

string0win1252 : Parser String
string0win1252 = map ByteArray.String.decodeWin1252 <| upToByte 0
