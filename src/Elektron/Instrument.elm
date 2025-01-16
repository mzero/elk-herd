module Elektron.Instrument exposing
  ( Device(..)
  , productName

  , Instrument
  , fromDeviceResponse

  , report

  , hasDriveSamples, hasProjects

  , DigitakStorageVersions
  , digitaktStorageVersions
  )


type Device
  = Digitakt
  | Digitakt2
  | Unknown


productName : Device -> String
productName d =
  case d of
    Digitakt  -> "Digitakt"
    Digitakt2 -> "Digitakt2"
    Unknown   -> "Unknown Device"


{-| Information about an Elektron instrument that it provides in response to
DeviceRequest and VersionRequest API messages. See `SysEx.Connect` module for
where this information is probed and an `Instrument` value created.

Notes:
  * `deviceName` is _usually_ what you expect, but we've seen devices that have
    a clearly customized name in this field. I suspect they are all owned by
    Elektron engineers.
  * `version` is intended for humans, `build` is an increasing number, though
    transmitted as a string, zero padded to four characters.
-}
type alias Instrument =
  { productId : Int
  , device : Device
  , deviceName : String
  , supportedMessages : List Int
  , build : String
  , version : String
  }


fromDeviceResponse : Int -> List Int -> String -> Instrument
fromDeviceResponse productId msgs deviceName =
  let
    device =
      case productId of
        12 -> Digitakt
        42 -> Digitakt2
        _  -> Unknown
  in
    Instrument productId device deviceName msgs "????" "?.??"


{-| This string is used when reporting the device to the stats server.
-}
report : Instrument -> String
report inst =
  String.join ","
    [ inst.deviceName
    , inst.version
    , String.fromInt inst.productId
    , inst.build
    ]

{-| You must be at least this tall to ride this ride.
-}
hasDriveSamples : Instrument -> Bool
hasDriveSamples inst =
  List.all (\e -> List.member e inst.supportedMessages)
    [ 0x10  -- dirList
    , 0x11  -- dirCreate
    , 0x12  -- dirDelete
    , 0x20  -- fileDelete
    , 0x21  -- itemRename
    ]

hasProjects : Instrument -> Bool
hasProjects inst =
  case inst.device of
    Digitakt -> True
    Digitakt2 -> True
    _ -> False

type alias DigitakStorageVersions =
  { projectSettingsVersion : Int
  , patternAndKitVersion : Int
  }

{-| For a given build of a device, these are the versions of the structures that
it uses. It can load older versions, but will always produce these versions.

The application sometimes needs "blank" versions of a project or a pattern.
This table determines which versino of those structures are needed. See
`Elektron.Digitakt.Blank` for the actual blank versions.
-}
digitaktVersions : List (String, DigitakStorageVersions)
  -- assoc list of build to versions
digitaktVersions =
  [ ({- 1.08   -} "0019", { projectSettingsVersion = 1, patternAndKitVersion = 2 })
  , ({- 1.10   -} "0021", { projectSettingsVersion = 1, patternAndKitVersion = 2 })
  , ({- 1.11b1 -} "0022", { projectSettingsVersion = 2, patternAndKitVersion = 3 })
  , ({- 1.11b7 -} "0029", { projectSettingsVersion = 2, patternAndKitVersion = 4 })
  , ({- 1.11   -} "0032", { projectSettingsVersion = 2, patternAndKitVersion = 4 })
  , ({- 1.??   -} "0033", { projectSettingsVersion = 3, patternAndKitVersion = 4 })
  , ({- 1.20   -} "0037", { projectSettingsVersion = 3, patternAndKitVersion = 5 })
  , ({- 1.30   -} "0053", { projectSettingsVersion = 5, patternAndKitVersion = 6 })
  , ({- 1.30B  -} "0064", { projectSettingsVersion = 5, patternAndKitVersion = 6 })
  , ({- 1.40   -} "0070", { projectSettingsVersion = 6, patternAndKitVersion = 7 })
  , ({- 1.40A  -} "0073", { projectSettingsVersion = 6, patternAndKitVersion = 7 })
  , ({- 1.40B  -} "0077", { projectSettingsVersion = 6, patternAndKitVersion = 7 })
  , ({- 1.50   -} "0084", { projectSettingsVersion = 7, patternAndKitVersion = 9 })
  , ({- 1.51   -} "0088", { projectSettingsVersion = 7, patternAndKitVersion = 9 })
  , ({- 1.51A  -} "0089", { projectSettingsVersion = 7, patternAndKitVersion = 9 })
  , ({- 1.52   -} "0095", { projectSettingsVersion = 7, patternAndKitVersion = 9 })
  ]

{-| Returns the versions for a given instrument by returning the last entry in
the table above that doesn't exceed the build number of the instrument.

This code (and the table able) is not based on the version string, because that
is a human readable string, and I don't know for sure the stucture is well
defined, or garunteed to be easily comparable. There is an unfortunate side
effect of this:

If an instrument appears with a version of "1.52A" or even "1.53", we could
probably safely assume that it uses the same version of the structures as all
the other "1.5x" releases. However, we have no way of telling that from the
build string. At present, such instruments are deemed incompatible, which
always leaves people hanging for me to update elk-herd when a new Digitakt
OS comes out.

TODO: Get clarification from Elketron on the way version strings work,
especially vis-a-vis storage structure settings.
-}
digitaktStorageVersions : Instrument -> Maybe DigitakStorageVersions
digitaktStorageVersions inst =
  let
    go dvs vers =
      case dvs of
        [] -> vers
        (b, v) :: rest ->
          if inst.build < b
            then vers
            else go rest (Just v)
  in
    if inst.device == Digitakt
      then go digitaktVersions Nothing
      else Nothing
