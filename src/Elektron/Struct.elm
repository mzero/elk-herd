module Elektron.Struct exposing
  ( VersionedPart
  , fixedVersion

  , object
  , field
  , omitField
  , skipTo
  , version
  , versionStruct
  , fieldWithVersion
  , variant
  , build
  , buildVersioned
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
import Missing.Maybe as Maybe
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
The `decoder` function will parse the version from the structure itself.
The `decoderVersion` function will parse, expecting a particular version of
the structure, and if the version field doesn't match, fail to parse.
The `version` function extracts the version from the structure.
-}
type alias VersionedPart v a =
  { encoder : a -> Encoder
  , decoder : Decoder a
  , decoderVersion : v -> Decoder a
  , view : String -> a -> List (Html.Html Never)
  , version : a -> v
  }

{-| Return a `Part` for a particular version of a `VersionedPart`.
-}
fixedVersion : VersionedPart v a -> v -> Part a
fixedVersion pa v =
  { encoder = pa.encoder
  , decoder = pa.decoderVersion v
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
      object PLock
        |> field   .paramId    "paramId"   uint8
        |> field   .track      "track"     uint8
        |> field   .steps      "steps"     (array 64 uint16be)
        |> build

The `object Plock` starts building a `Part`, but is "partially constructed".
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
  * `v` is the type of the version of this structure (could be ())
  * `x` is the type of the version that we know so far

"partially constructed" is a type used during parsing. You could parse the
above `PLock` with:

    Parser.map3 PLock
      Parser.uint8 Parser.unit8 (Parser.array 64 Parser.uint16be)

This works because the name of the type acts as a constrcutor:

    PLock : Int -> Int -> Array Int -> PLock

`ObjectBuilder` builds up the parser, adding one more field for each `field`
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
type ObjectBuilder a b v x = OB
  { encoders : List (a -> Encoder)
  , decoder : Maybe v -> Decoder (b, x, Parser.Mark)
  , views : List (a -> List (Html.Html Never))
  , version : a -> x
  }


{-| Call this with the n-argument constructor for the structure (of n fields).

For the astute: The decoder return type is `(b, v, Parser.Mark)`
  * `b` is the partially constructed structure
  * `v` is the version we know, which at the start is `()`
  * `Paser.Mark` is the point in the byte stream where we start parsing
-}
object : b -> ObjectBuilder a b v ()
object b = OB
  { encoders = []
  , decoder = (\_ -> Parser.mark |> Parser.map (\m -> (b, (), m)))
  , views = []
  , version = always ()
  }

{-| The arguments are:

  * `fext`    - a function that extracts the field from the structure,
             like `.paramId` from the PLock example
  * `flabel`  - a string to label this field in views
  * `pb`      - a `Part` for the type of the field
  * `(OB ob)` - the `ObjectBuilder` "so far"
-}
field :
  (a -> b) -> String -> Part b
  -> ObjectBuilder a (b -> c) v x
  -> ObjectBuilder a c v x
field fext flabel pb (OB ob) =
  OB
    { encoders = ob.encoders ++ [fext >> pb.encoder]
    , decoder =
        ob.decoder
        >> Parser.andThen (\(fbc, x, m) ->
          pb.decoder
          |> Parser.map (\b -> (fbc b, x, m))
        )
    , views = ob.views ++ [fext >> pb.view flabel]
    , version = ob.version
    }

{-| This is just like `field` but used when a version of a structure doesn't
have this field.  You still need to fill out the value in the elm object,
and this provides that value.
-}
omitField :
  (a -> b) -> b
  -> ObjectBuilder a (b -> c) v x
  -> ObjectBuilder a c v x
omitField fext b (OB ob) =
  OB
    { encoders = ob.encoders
    , decoder =
        ob.decoder
        >> Parser.map (\(fbc, x, m) -> (fbc b, x, m))
    , views = ob.views
    , version = ob.version
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
skipTo :
  (a -> ByteArray) -> (x -> Maybe Int)
  -> ObjectBuilder a (ByteArray -> c) v x
  -> ObjectBuilder a c v x
skipTo fext foff (OB ob) =
  OB
    { encoders = ob.encoders ++ [fext >> Builder.bytes]
    , decoder =
        ob.decoder
        >> Parser.andThen (\(fbc, x, m) ->
          case foff x of
            Just offset ->
              Parser.upToOffset m offset
              |> Parser.map (\b -> (fbc b, x, m))
            Nothing -> Parser.fail "no offset for version"
        )
    , views = ob.views ++ [ \a ->
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
    , version = ob.version
    }


{- This code is experimental, but hard enough to write that I'm keeping it
here until I decide to use it or not.

offset : Int
  -> ObjectBuilder a c v x
  -> ObjectBuilder a c v x
offset n (OB ob) =
  OB
  { encoders = ob.encoders
  , decoder = \mv ->
      ob.decoder mv
      |> Parser.andThen (\(r, x, m) ->
        Parser.map (\_ -> (r, x, m)) (Parser.offset m n))
  , views = ob.views
  , version = ob.version
  }

type alias Sizable = { bytes : ByteArray }

size : Int
  -> ObjectBuilder a Sizable v x
  -> ObjectBuilder a Sizable v x
size n (OB ob) =
  OB
  { encoders = ob.encoders
  , decoder = \mv ->
      ob.decoder mv
      |> Parser.andThen (\(r, x, m) ->
          Parser.jump m
          |> Parser.andThen (\_ ->
            Parser.nextBytes n
              |> Parser.andThen (\bs ->
                Parser.succeed ({r|bytes=bs} , x, m))))
  , views = ob.views
  , version = ob.version
  }
-}


{-| This is just like `field`, but used when the field is a sub-structure,
and furthermore, the version of that sub-structure will supply the version
for the object we are building.

This has one use: a `patternKit` has a version, but no version field. Instead,
the version of it's first component, `pattern` provides it's version.
 -}
versionStruct :
  (a -> b) -> String -> VersionedPart v b -> (b -> v)
  -> ObjectBuilder a (b -> c) v x
  -> ObjectBuilder a c v v
versionStruct fext flabel pb fver (OB ob) =
  OB
  { encoders = ob.encoders ++ [fext >> pb.encoder]
  , decoder = \mv ->
      ob.decoder mv
      |> Parser.andThen (\(fbc, x, m) ->
        Maybe.unwrap pb.decoder pb.decoderVersion mv
        |> Parser.map (\b -> (fbc b, fver b, m))
      )
  , views = ob.views ++ [fext >> pb.view flabel]
  , version = fext >> fver
  }

{-| This is exactly like `field`, only this is the field that supplies the
version of this object.

FIXME: This should be giving a parse error if we are parsing a particular
version (the arguement to the decoder is Just v) but the structure has a
different value.
-}
version :
  (a -> v) -> String -> Part v
  -> ObjectBuilder a (v -> c) v x
  -> ObjectBuilder a c v v
version fext flabel pb (OB ob) =
  let
    (OB ob_) = field fext flabel pb (OB ob)
  in
    OB
    { encoders = ob_.encoders
    , decoder =
        ob.decoder
        >> Parser.andThen (\(fbc, x, m) ->
          pb.decoder
          |> Parser.map (\b -> (fbc b, b, m))
        )
    , views = ob_.views
    , version = fext
    }


{-| Declare a field, but the `Part` for the field depends on the version
of this object.

TODO: This function's name isn't very clear
-}
fieldWithVersion :
  (a -> b) -> String -> (v -> Part b)
  -> ObjectBuilder a (b -> c) v v
  -> ObjectBuilder a c v v
fieldWithVersion fext flabel fpb (OB ob) =
  OB
    { encoders = ob.encoders ++ [\a -> fext a |> (fpb (ob.version a)).encoder]
    , decoder =
        ob.decoder
        >> Parser.andThen (\(fbc, x, m) ->
          (fpb x).decoder
          |> Parser.map (\b -> (fbc b, x, m))
        )
    , views = ob.views ++ [\a -> fext a |> (fpb (ob.version a)).view flabel]
    , version = ob.version
    }

{-| Declare a field, using the version to pick from a list of parts.
-}
variant :
  (a -> b) -> String -> List (v, Part b)
  -> ObjectBuilder a (b -> c) v v
  -> ObjectBuilder a c v v
variant fext flabel vars =
  let
    findVar v =
      case List.lookup v vars of
        Just pb -> pb
        Nothing -> varientError

    varientError : Part b
    varientError =
      { encoder = always Builder.empty
      , decoder = Parser.fail ("unsupport variant for " ++ flabel)
      , view = \label _ -> [ fieldView label "can't happen!" ]
      }
  in
    fieldWithVersion fext flabel findVar


{-| The last step in constructing the part.

Notice the type signature for the argument:

  * The first two types are the same: The type we are building for, and the
    "partially completed" type must be the same, meaning we've handled all
    the fields.
  * The second two types are the same: The version we are expecting must be
    the version type we have.

These two things ensure that you can't leave a field out, and you can't
forget the version if the structure should have one.
-}
buildVersioned : ObjectBuilder a a v v -> VersionedPart v a
buildVersioned (OB ob) =
  { encoder = \o -> Builder.list (\e -> e o) ob.encoders
  , decoder = Parser.map (\(r, _, _) -> r) (ob.decoder Nothing)
  , decoderVersion = Parser.map (\(r, _, _) -> r) << ob.decoder << Just
  , view = \label v ->
    [ Html.div [ Attr.class "field field-fullwidth" ]
      ([ Html.span [ Attr.class "label" ] [ Html.text label ]
      ]
      ++ List.concatMap (\e -> e v) ob.views
      )
    ]
  , version = ob.version
  }


{-| For a non-versioned struct, this `build` ensures it is built.
-}
build : ObjectBuilder a a () () -> Part a
build ob = fixedVersion (buildVersioned ob) ()

