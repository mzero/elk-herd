module SysEx.ApiUtil exposing
  ( DirEntry

  , Arg
  , argBool, argByte, argBytes, argUint32be, argString0win1252
  , argDirEntry, argByteList
  , argListAll

  , Msg0, Msg1, Msg2, Msg3, Msg4, Msg5, Msg6

  , MsgBuilder
  , msg0, msg1, msg2, msg3, msg4, msg5, msg6

  , Api
  , buildApi
  )

{-| Message descriptors for the Elektron API. Descriptors encapsulate how to
convert between bytes on the wire (well, from SysEx messages) and Elm objects.

See the module SysEx.Message for where this is used.
-}

import Char
import Html

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import ByteArray.Parser as Parser
import SysEx.Internal exposing (..)
import Util


{- Builders & Parsers specific to the API's encoding of Bool
-}


buildBool : Bool -> Builder.Builder
buildBool b = Builder.byte <| if b then 1 else 0

parseBool : Parser.Parser Bool
parseBool =
  let
    asBool b = case b of
      0 -> Ok False
      1 -> Ok True
      n -> Err <| "not a boolean byte: " ++ String.fromInt n
  in
    Parser.byte |> Parser.andThen (Parser.result << asBool)


{- Argument Descriptors

`Arg a` represents everything that can be done with a particular kind of
message argument:
  * Build the wire format from an `a`
  * parse an `a` from the wire format
  * create an Html fragment showing the value `a`

There is an `argFoo` function for each type of API argument, which creates
an `Arg a` value, where `a` is the Elm value type.
-}

type alias Arg a =
  { build : a -> Builder.Builder
  , parse : Parser.Parser a
  , view : a -> List (Html.Html Never)
  }

argBool : String -> Arg Bool
argBool fieldName =
  { build = buildBool
  , parse = parseBool
  , view = List.singleton << fieldView fieldName
            << (\b -> if b then "true" else "false")
  }

argByte : String -> Arg Int
argByte fieldName =
  { build = Builder.byte
  , parse = Parser.byte
  , view = List.singleton << hexFieldView fieldName << Util.hexByteString
  }

argBytes : String -> Arg ByteArray
argBytes fieldName =
  { build = Builder.bytes
  , parse = Parser.rest
  , view = List.singleton << hexFieldView fieldName <<
    (\bytes ->
      if ByteArray.length bytes <= 128
        then ByteArray.hexDump bytes
        else ByteArray.hexDump (ByteArray.section 0 128 bytes)
          ++ "...\n"
    )
  }

argUint32be : String -> Arg Int
argUint32be fieldName =
  { build = Builder.uint32be
  , parse = Parser.uint32be
  , view = List.singleton << fieldView fieldName << String.fromInt
  }

argString0win1252 : String -> Arg String
argString0win1252 fieldName =
  { build = Builder.string0win1252
  , parse = Parser.string0win1252
  , view = List.singleton << fieldView fieldName
  }

type alias DirEntry =
  { hash: Int, size: Int, locked: Bool, type_: Char, name: String }

argDirEntry : Arg DirEntry
argDirEntry =
  { build = (\entry -> Builder.sequence
      [ Builder.uint32be entry.hash
      , Builder.uint32be entry.size
      , buildBool entry.locked
      , Builder.byte (Char.toCode entry.type_ )
      , Builder.string0win1252 entry.name
      ]
    )
  , parse = Parser.map5 DirEntry
      Parser.uint32be
      Parser.uint32be
      parseBool
      (Parser.map Char.fromCode Parser.byte)
      Parser.string0win1252
  , view = (\entry ->
      [ fieldView "hash" <| String.fromInt entry.hash
      , fieldView "size" <| String.fromInt entry.size
      , fieldView "locked" <| if entry.locked then "🔒" else "-"
      , fieldView "type" <| String.fromChar entry.type_
      , fieldView "name" entry.name
      ]
    )
  }

argByteList : String -> Arg (List Int)
argByteList fieldName =
  { build = \bs -> Builder.byte (List.length bs)
                   |> Builder.and (Builder.byteList bs)
  , parse = Parser.byte
    |> Parser.andThen (\n -> Parser.repeat n Parser.byte)
  , view = (\bs -> List.singleton <|
      hexFieldView fieldName
        <| String.join ", "
        <| List.map ((++) "0x" << Util.hexByteString) bs
    )
  }

argListAll : Arg a -> Arg (List a)
argListAll argA =
  { build = Builder.list argA.build
  , parse = Parser.repeatToEnd argA.parse
  , view = List.singleton << Html.ul [] << List.map (Html.li [] << argA.view)
  }


{- Message Descriptors

`MsgN` is just like the `Arg a` descriptors, but for a message of N arguments.
Like Arg, this includes building, parsing, and viewing.

The `build` and `view` members are functions that take N arguments, which are
the arguments of the message, in order.

The `parse` member parses an entire message, `r`.
-}

type alias Msg0 r =
  { build : Builder.Builder
  , parse : Parser.Parser r
  , view : List (Html.Html Never)
  }

type alias Msg1 r a =
  { build : a -> Builder.Builder
  , parse : Parser.Parser r
  , view : a -> List (Html.Html Never)
  }

type alias Msg2 r a b =
  { build : a -> b -> Builder.Builder
  , parse : Parser.Parser r
  , view : a -> b -> List (Html.Html Never)
  }

type alias Msg3 r a b c =
  { build : a -> b -> c -> Builder.Builder
  , parse : Parser.Parser r
  , view : a -> b -> c -> List (Html.Html Never)
  }

type alias Msg4 r a b c d =
  { build : a -> b -> c -> d -> Builder.Builder
  , parse : Parser.Parser r
  , view : a -> b -> c -> d -> List (Html.Html Never)
  }

type alias Msg5 r a b c d e =
  { build : a -> b -> c -> d -> e -> Builder.Builder
  , parse : Parser.Parser r
  , view : a -> b -> c -> d -> e -> List (Html.Html Never)
  }

type alias Msg6 r a b c d e f =
  { build : a -> b -> c -> d -> e -> f -> Builder.Builder
  , parse : Parser.Parser r
  , view : a -> b -> c -> d -> e -> f -> List (Html.Html Never)
  }

{- `msgN` builds a `MsgN`

Each function takes:
  * a construtor (of N arguments) which builds the Elm message value from N
    values once parsed
  * N Arg descriptors for each of the arguments of the mssage

The remaining parameters are supplied by `buildAPI`, which combines two
related messages into a request/response pair:
  * message id
  * message name

Each `msgN` is the same:
  * build the message by building the id, followed by all the arguments
  * parse the message by parsing each argument in turn, and applying the
    constructor
  * build a view of the message name, and concat all the argument views

Hint: Start by looking at how `msg2` works.
-}

type alias MsgBuilder m = Int -> String -> m

msg0: r -> MsgBuilder (Msg0 r)
msg0 r id name =
  { build = Builder.byte id
  , parse = Parser.succeed r
  , view = [ fieldView "message" name ]
  }

msg1 : (a -> r) -> Arg a -> MsgBuilder (Msg1 r a)
msg1 fn argA id name =
  { build = \a -> Builder.byte id |> Builder.and (argA.build a)
  , parse = Parser.map fn argA.parse
  , view = \a -> fieldView "message" name :: argA.view a
  }

msg2 : (a -> b -> r) -> Arg a -> Arg b -> MsgBuilder (Msg2 r a b)
msg2 fn argA argB id name =
  { build = \a b -> Builder.sequence
      [ Builder.byte id, argA.build a, argB.build b]
  , parse = Parser.map2 fn argA.parse argB.parse
  , view = \a b -> fieldView "message" name :: argA.view a ++ argB.view b
  }

msg3 : (a -> b -> c -> r) -> Arg a -> Arg b -> Arg c
  -> MsgBuilder (Msg3 r a b c)
msg3 fn argA argB argC id name =
  { build = \a b c -> Builder.sequence
      [ Builder.byte id, argA.build a, argB.build b, argC.build c ]
  , parse = Parser.map3 fn argA.parse argB.parse argC.parse
  , view = \a b c ->
      fieldView "message" name :: argA.view a ++ argB.view b ++ argC.view c
  }

msg4 : (a -> b -> c -> d -> r) -> Arg a -> Arg b -> Arg c -> Arg d
  -> MsgBuilder (Msg4 r a b c d)
msg4 fn argA argB argC argD id name =
  { build = \a b c d -> Builder.sequence
      [ Builder.byte id
      , argA.build a, argB.build b, argC.build c, argD.build d
      ]
  , parse = Parser.map4 fn argA.parse argB.parse argC.parse argD.parse
  , view = \a b c d ->
      fieldView "message" name
      :: argA.view a ++ argB.view b ++ argC.view c ++ argD.view d
  }

msg5 : (a -> b -> c -> d -> e -> r)
  -> Arg a -> Arg b -> Arg c -> Arg d -> Arg e
  -> MsgBuilder (Msg5 r a b c d e)
msg5 fn argA argB argC argD argE id name =
  { build = \a b c d e -> Builder.sequence
      [ Builder.byte id
      , argA.build a, argB.build b, argC.build c
      , argD.build d, argE.build e
      ]
  , parse = Parser.map5 fn
      argA.parse argB.parse argC.parse
      argD.parse argE.parse
  , view = \a b c d e ->
      fieldView "message" name
      :: argA.view a ++ argB.view b ++ argC.view c
      ++ argD.view d ++ argE.view e
  }

msg6 : (a -> b -> c -> d -> e -> f -> r)
  -> Arg a -> Arg b -> Arg c -> Arg d -> Arg e -> Arg f
  -> MsgBuilder (Msg6 r a b c d e f)
msg6 fn argA argB argC argD argE argF id name =
  { build = \a b c d e f -> Builder.sequence
      [ Builder.byte id
      , argA.build a, argB.build b, argC.build c
      , argD.build d, argE.build e, argF.build f
      ]
  , parse = Parser.map6 fn
      argA.parse argB.parse argC.parse
      argD.parse argE.parse argF.parse
  , view = \a b c d e f ->
      fieldView "message" name
      :: argA.view a ++ argB.view b ++ argC.view c
      ++ argD.view d ++ argE.view e ++ argF.view f
  }


{- Api Descriptor

An Api describes a pair of messages: request and response. The opcode
value (|0x80 for the response opcode) and a descriptive name are also
supplied.
-}

type alias Api reqArg respArg =
  { id : Int
  , op : String
  , request : reqArg
  , response : respArg
  }

buildApi : Int -> String -> MsgBuilder m -> MsgBuilder n -> Api m n
buildApi id op buildReq buildResp =
  Api id op
    (buildReq   id         (op ++ ".Request"))
    (buildResp (id + 0x80) (op ++ ".Response"))

