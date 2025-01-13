module Elektron.Struct exposing
  ( Struct
  , forVersionSpec

  , struct
  , field, fieldV
  , omitField
  , skipTo, skipToOrOmit
  , version
  , build
  , buildAsPart
  )

{-| Structure descriptors for decoding and encoding the Elekctron storage
data structures. Descriptors encapsulate how to convert between bytes in the
struct, and Elm objects.

This code contains some rather specialized support for the issues surrounding
Elektron storage formats. In particular the versioning system requires non-
trivial amounts of code.
-}

import Array exposing (Array)
import Html
import Html.Attributes as Attr

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import ByteArray.Parser as Parser
import Elektron.Struct.Part as Part exposing (Part)
import Missing.List as List
import SysEx.Internal exposing (..)


{- About versions

Elektron storage data structures are versioned. As they release new OSes for
an instrument, the data structures will be updated. For example, there are
currently 10 versions of the Digitakt patternStorage structure. While generally,
the structures only add fields at the end, sometimes they add them in the
middle. Hence, each version, while having mostly the same fields, may have
the fields we care about in different places. And in one case, may not have the
field we care about at all.

Some structures include a version number at the start. This version field is
always at the same offset, so you can parse that far, get the version, then
know how to parse the rest.

Other structures don't have a version field. For these, the enclosing
structure's version determines which version is used. For example, version 8 of
a patternStorage has tracks that are version 4 of trackStorage.

Sometimes, both things are true: For example, kitStorage version 6 contains a
midiSetupStorage version 1. But midiSetupStorage has its own version field.
What happens if the embedded structure has the wrong version? Who knows! We
consider that a parse error. (FIXME: we don't actually error in this case!)

Lastly, we must consider patternKitStorage: This has just two fields, a
patternStorage and a kitStorage, each with their own version fields. Only the
two versions must match. So we parse the first letting it tell us what version,
and then parse the second insisting on it being the same.
-}

type alias Encoder = Part.Encoder
type alias Decoder a = Part.Decoder a


{-| Pretty much like a `Part`, but for structures that contain a version.
The `decoder` function takes a version request type - w -, which is used to
constrain or validate the version actually decoded.
The `version` function extracts the version from the structure.
-}
type alias Struct w v a =
  { encoder : a -> Encoder
  , decoder : w -> Decoder a
  , view : String -> a -> List (Html.Html Never)
  , version : a -> v
  }

{-| Return a `Part` for a particular version of a `Struct`.
-}
forVersionSpec : Struct w v a -> w -> Part a
forVersionSpec pa w =
  { encoder = pa.encoder
  , decoder = pa.decoder w
  , view = pa.view
  }


{- Building up `Part` descriptors for structures.

Here's a simple example:

    type alias PLock =
      { paramId:  Int
      , track:    Int
      , steps:    Array Int  -- 64 steps
      }

We can create a `Part` for this like so:

    structPLock : Part PLock
    structPLock =
      struct PLock
        |> field   .paramId    "paramId"   uint8
        |> field   .track      "track"     uint8
        |> field   .steps      "steps"     (array 64 uint16be)
        |> build

The `struct Plock` starts building a `Part`, but is "partially constructed".
Each call to `field` adds more to the partial construction adding:

  * how to extract and encode the field
  * how to decode the field and add it to the construction
  * how to extract and view the field

The final `build` function then combines all these together and returns a
`Part` that can build, parse, and view the whole `PLock` type.

-}

{-| This is a partially constructed `Part` for a structure.

The types are:

  * `a` is the type of structure we are ultimately building the `Part` for
  * `b` is a partially constructed `a`
  * `w` is the type of the version request passed into decode
  * `v` is the type of the version of this structure (could be ())
  * `x` is the type of the version that we know so far

"partially constructed `a`" is a type used during parsing. You could parse the
above `PLock` with:

    Parser.map3 PLock
      Parser.uint8 Parser.unit8 (Parser.array 64 Parser.uint16be)

This works because the name of the type acts as a constrcutor:

    PLock : Int -> Int -> Array Int -> PLock

`StructBuilder` builds up the parser, adding one more field for each `field`
call, so the parse looks like

    Parser.succeed PLock
      |> Parser.andThen (\c0 -> Parser.map c0 (Parser.uint8))
      |> Parser.andThen (\c1 -> Parser.map c1 (Parser.uint8))
      |> Parser.andThen (\c2 -> Parser.map c2 (Parser.array 64 Parser.uin16be))

Each `cn` is the constructor with n arguments applied.

"version that we know so far": Remember that some structures have an embedded
version field, but until we parse that far into the structure, we don't know
what version it is. During this part of the parse, `x` will be `()` which
ensures that we don't try to do anything with the version. Once the version
field is parsed, `x` will be the same as `v` (generally `Int`). Note that the
type of the `version` field reflects this, and changes as the fields are parsed.
-}
type StructBuilder a b w v x = SB
  { encoders : List (a -> Encoder)
  , decoder : w -> Decoder (b, x, Parser.Mark)
  , views : List (a -> List (Html.Html Never))
  , version : a -> x
  }


{-| This starts the construction of a Struct for a structure.

For the astute: The decoder return type is `(f, v, Parser.Mark)`
  * `f` is the partially constructed structure
  * `v` is the version we know, which at the start is `()`
  * `Paser.Mark` is the point in the byte stream where we start parsing
-}
struct : b -> StructBuilder a b w v ()
struct b = SB
  { encoders = []
  , decoder = (\_ -> Parser.mark |> Parser.map (\m -> (b, (), m)))
  , views = []
  , version = always ()
  }

{-| Add a field to a partially build Struct.

The arguments are:

  * `fext`    - a function that extracts the field from the structure,
                like `.paramId` from the PLock example
  * `flabel`  - a string to label this field in views
  * `pb`      - a `Part` for the type of the field
  * `(SB sb)` - the `StructBuilder` "so far"
-}
field :
  (a -> b) -> String -> Part b
  -> StructBuilder a (b -> c) w v x
  -> StructBuilder a c w v x
field fext flabel pb (SB sb) =
  SB
    { encoders = sb.encoders ++ [fext >> pb.encoder]
    , decoder =
        sb.decoder
        >> Parser.andThen (\(fbc, x, m) ->
          pb.decoder
          |> Parser.map (\b -> (fbc b, x, m))
          |> Parser.mapError (\s -> flabel ++ ": " ++ s)
        )
    , views = sb.views ++ [fext >> pb.view flabel]
    , version = sb.version
    }

{-| Like field, but the `Part` for the field depends on the version
of this object. The third argument is now `v -> Part b`: a function
from the version to a Part.
-}
fieldV :
  (a -> b) -> String -> (v -> Part b)
  -> StructBuilder a (b -> c) w v v
  -> StructBuilder a c w v v
fieldV fext flabel fpb (SB sb) =
  SB
    { encoders = sb.encoders ++ [\a -> fext a |> (fpb (sb.version a)).encoder]
    , decoder =
        sb.decoder
        >> Parser.andThen (\(fbc, x, m) ->
          (fpb x).decoder
          |> Parser.map (\b -> (fbc b, x, m))
          |> Parser.mapError (\s -> flabel ++ ": " ++ s)
        )
    , views = sb.views ++ [\a -> fext a |> (fpb (sb.version a)).view flabel]
    , version = sb.version
    }

{-| This is just like `field` but used when a version of a structure doesn't
have this field.  You still need to fill out the value in the elm object,
and this provides that value.
-}
omitField :
  (a -> b) -> b
  -> StructBuilder a (b -> c) w v x
  -> StructBuilder a c w v x
omitField fext b (SB sb) =
  SB
    { encoders = sb.encoders
    , decoder =
        sb.decoder
        >> Parser.map (\(fbc, x, m) -> (fbc b, x, m))
    , views = sb.views
    , version = sb.version
    }

{-| For the most part, elk-herd cares only about a few fields in each structure.
For example, in a sound, all we care about is the sample slot parameter. All
the data elk-herd doesn't care about must be preserved and re-encoded when
sent back to the instrument or exported a file.

This is a special kind of field that skips from whatever has been parsed
or built so far, to the known offset of the next field we care about. The
skipped over bytes are stored in a field of the structure, usually called
`.skipn`.

This is also used to gather up all the bytes from the last field we needed
to the end of the data structure.

The offset is given as a function, `foff` from the known version to maybe
an offset. This is because the offset needed can vary with the version of
the structure. See the module `Elektron.Digitakt.CppStructs` for these
version-to-offset functions.
-}
skipTo_ :
  Bool ->
  (a -> ByteArray) -> (v -> Maybe Int)
  -> StructBuilder a (ByteArray -> c) w v v
  -> StructBuilder a c w v v
skipTo_ omit fext foff (SB sb) =
  SB
    { encoders = sb.encoders ++ [fext >> Builder.bytes]
    , decoder =
        sb.decoder
        >> Parser.andThen (\(fbc, v, m) ->
          case foff v of
            Just offset ->
              Parser.upToOffset m offset
              |> Parser.map (\b -> (fbc b, v, m))
            Nothing ->
              if omit
                then Parser.succeed (fbc ByteArray.empty, v, m)
                else Parser.fail "no offset for version"
        )
    , views = sb.views ++ [ \a ->
        [ Html.div [ Attr.class "field field-fullwidth" ]
          [ Html.span [ Attr.class "label" ] [ Html.text "skipped" ]
          , Html.span [ Attr.class "value hexdump" ]
            [ Html.text <|
              "-- "
              ++ (String.fromInt <| ByteArray.length <| fext a)
              ++ " bytes --"
            ]
          ]
        ]]
    , version = sb.version
    }

skipTo :
  (a -> ByteArray)
  -> (v -> Maybe Int)
  -> StructBuilder a (ByteArray -> c) w v v
  -> StructBuilder a c w v v
skipTo = skipTo_ False

skipToOrOmit :
  (a -> ByteArray)
  -> (v -> Maybe Int)
  -> StructBuilder a (ByteArray -> c) w v v
  -> StructBuilder a c w v v
skipToOrOmit = skipTo_ True

{-| This is very similar to `field`, only this is the field that supplies the
version of this object. Rather than a `Part b`, this takes a `Struct w v b`
so that it can extract the version from that value once built, and consider
that the version of the structure being built.

The substructure might be a version itself... or it might be a more complex
structure that has a version.
-}
version :
  (a -> b) -> String
  -> Struct w v b
  -> StructBuilder a (b -> c) w v x
  -> StructBuilder a c w v v
version fext flabel sv (SB sb) =
  SB
  { encoders = sb.encoders ++ [fext >> sv.encoder]
  , decoder = (\w ->
      sb.decoder w
      |> Parser.andThen (\(fbc, x, m) ->
        sv.decoder w
        |> Parser.map (\b -> (fbc b, sv.version b, m))
        |> Parser.mapError (\s -> flabel ++ ": " ++ s)
    )
  )
  , views = sb.views ++ [fext >> sv.view flabel]
  , version = fext >> sv.version
  }


{-| The last step in constructing the part.

Notice the type signature for the argument:

  * The first two types are the same: The type we are building for, and the
    "partially completed" type must be the same, meaning we've handled all
    the fields.
  * The last two types are the same: The version we are expecting must be
    the version type we have.

These two things ensure that you can't leave a field out, and you can't
forget the version if the structure should have one.
-}
build : String -> StructBuilder a a w v v -> Struct w v a
build name (SB sb) =
  { encoder = \o -> Builder.list (\e -> e o) sb.encoders
  , decoder =
      sb.decoder
      >> Parser.map (\(r, _, _) -> r)
      >> Parser.mapError (\s -> name ++ "." ++ s)
  , view = \label v ->
    [ Html.div [ Attr.class "field field-fullwidth" ]
      ([ Html.span [ Attr.class "label" ] [ Html.text label ]
      ]
      ++ List.concatMap (\e -> e v) sb.views
      )
    ]
  , version = sb.version
  }


{-| For a non-versioned struct, this `build` ensures it is completely built,
has no version, and is then available as a `Part`.
-}
buildAsPart : String -> StructBuilder a a () () () -> Part a
buildAsPart name sb = forVersionSpec (build name sb) ()

