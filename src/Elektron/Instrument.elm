module Elektron.Instrument exposing
  ( Device(..)
  , productName

  , Instrument
  , fromDeviceResponse, updateFromVersionResponse


  , report

  , hasDriveSamples, hasProjects
  , supportsStereo

  , ProjectSpec
  , Version
  , StorageVersions
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
  , projectSpec : Maybe ProjectSpec
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
    Instrument productId device deviceName msgs "????" "?.??" Nothing


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

supportsStereo : Instrument -> Bool
supportsStereo inst =
  case inst.device of
    Digitakt -> False
    Digitakt2 -> True
    Unknown -> False
  {- TODO: The "right" way to do this is to use the new Query API and use key
  sample_file.interleaved_stereo_support to see if stereo is supported.
  But that's a lot of work, and since only these two instruments are
  supported, and one is always mono, the other has always supported stereo,
  this is expedient.
  -}
hasProjects : Instrument -> Bool
hasProjects inst = inst.projectSpec /= Nothing


type alias ProjectSpec =
  { storageVersions : StorageVersions
  , numSampleSlots : Int
  }

{- Elektron structures are versioned. The versions in the binary dumps are
just small integers... and each structure stars with version 0 and progresses.

Different devices reuse the same version sequence for the same structures.
Thus, a Digitakt v2 kitStorage is not the same as Digitakt2 v2 kitStorage.
Since elk-herd handles both devices with the same set of structures, the
device is made part of this Version type.

NB: Only the integer portion is serialized in the dumps.

NB: Be careful not to confuse version numbers for different structures. Ask
Mark why this doesn't have a phantom type to help with that.
-}
type alias Version = { device : Device, int : Int }


type alias StorageVersions =
  { projectSettings : Int
  , patternAndKit : Int
  }


updateFromVersionResponse : String -> String -> Instrument -> Instrument
updateFromVersionResponse build version inst =
  let
    storageVersions = case inst.device of
      Digitakt  -> findStorageVersions digitaktVersions build
      Digitakt2 -> findStorageVersions digitakt2Versions build
      _ -> Nothing

    numSampleSlots = case inst.device of
        Digitakt  -> Just  128
        Digitakt2 -> Just 1024     -- see also same data in Elektron.Digitakt.Dump
        _         -> Nothing
    projectSpec = Maybe.map2 ProjectSpec storageVersions numSampleSlots
  in
    { inst | build = build, version = version, projectSpec = projectSpec}




type alias VersionList = List (String, StorageVersions)


{-| For a given build of a device, these are the versions of the structures that
it uses. It can load older versions, but will always produce these versions.

The application sometimes needs "blank" versions of a project or a pattern.
This table determines which version of those structures are needed. See
`Elektron.Digitakt.Blank` for the actual blank versions.
-}
digitaktVersions : VersionList
  -- assoc list of build to versions
digitaktVersions =
  [ ({- 1.08   -} "0019", { projectSettings = 1, patternAndKit = 2 })
  , ({- 1.10   -} "0021", { projectSettings = 1, patternAndKit = 2 })
  , ({- 1.11b1 -} "0022", { projectSettings = 2, patternAndKit = 3 })
  , ({- 1.11b7 -} "0029", { projectSettings = 2, patternAndKit = 4 })
  , ({- 1.11   -} "0032", { projectSettings = 2, patternAndKit = 4 })
  , ({- 1.??   -} "0033", { projectSettings = 3, patternAndKit = 4 })
  , ({- 1.20   -} "0037", { projectSettings = 3, patternAndKit = 5 })
  , ({- 1.30   -} "0053", { projectSettings = 5, patternAndKit = 6 })
  , ({- 1.30B  -} "0064", { projectSettings = 5, patternAndKit = 6 })
  , ({- 1.40   -} "0070", { projectSettings = 6, patternAndKit = 7 })
  , ({- 1.40A  -} "0073", { projectSettings = 6, patternAndKit = 7 })
  , ({- 1.40B  -} "0077", { projectSettings = 6, patternAndKit = 7 })
  , ({- 1.50   -} "0084", { projectSettings = 7, patternAndKit = 9 })
  , ({- 1.51   -} "0088", { projectSettings = 7, patternAndKit = 9 })
  , ({- 1.51A  -} "0089", { projectSettings = 7, patternAndKit = 9 })
  , ({- 1.52   -} "0095", { projectSettings = 7, patternAndKit = 9 })
  , ({- 1.52A  -} "0097", { projectSettings = 7, patternAndKit = 9 })
  ]

digitakt2Versions : VersionList
  -- assoc list of build to versions
digitakt2Versions =
  [ ({- 1.02   -} "0035", { projectSettings = 0, patternAndKit = 0 })
  , ({- 1.03   -} "0039", { projectSettings = 0, patternAndKit = 0 })
  , ({- 1.03A  -} "0041", { projectSettings = 0, patternAndKit = 0 })
  , ({- 1.10   -} "0048", { projectSettings = 1, patternAndKit = 3 })
  , ({- 1.10A  -} "0053", { projectSettings = 1, patternAndKit = 3 })
  , ({- 1.15   -} "0065", { projectSettings = 1, patternAndKit = 4 })
  , ({- 1.15A  -} "0069", { projectSettings = 1, patternAndKit = 4 })
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
findStorageVersions : VersionList -> String -> Maybe StorageVersions
findStorageVersions vlist build =
  let
    go dvs vers =
      case dvs of
        [] -> vers
        (b, v) :: rest ->
          if build < b
            then vers
            else go rest (Just v)
  in
    go vlist Nothing
