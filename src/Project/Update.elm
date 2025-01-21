module Project.Update exposing
  ( update
  , subscriptions
  )

import Dict
import Process
import Task

import Alert
import Bank exposing (Index(..))
import Bank.Shuffle
import Browser.Dom
import ByteArray exposing (ByteArray)
import Elektron.Digitakt.HighLevel as DT
import Elektron.Digitakt.Related as DT
import Elektron.Digitakt.Verify as DT
import Elektron.Digitakt.Shuffle
import Elektron.Digitakt.Types as DT
import Elektron.Drive as Drive exposing (Drive)
import Job exposing (Job, Step(..))
import Missing.Time as Time
import Progress
import Project.Base exposing (..)
import Project.Import as Import
import Project.Selection.Project as Sel
import Project.Util exposing (..)
import Portage
import SysEx.Client exposing (..)
import SysEx.Dump as Dump
import SysEx.Message as Message
import SysEx.SysEx
import Undo



returnM : Model -> Update
returnM m = (m, Cmd.none, [])

returnMC : Model -> Cmd Msg -> Update
returnMC m c = (m, c, [])

returnMR : Model -> Requests Msg -> Update
returnMR m r = (m, Cmd.none, r)

returnMCR : Model -> Cmd Msg -> Requests Msg -> Update
returnMCR m c r = (m, c, r)

thenUpdate : (Model -> Update) -> Update -> Update
thenUpdate updateFn (m0, c0, r0) =
  let
    (m1, c1, r1) = updateFn m0
  in
    (m1, Cmd.batch [ c0, c1 ], r0 ++ r1)


addToUndoStack : Undo.Tag -> Model -> Model
addToUndoStack tag m =
  { m | undoStack = Undo.note tag (m.project, m.selection) m.undoStack }

undoable : String -> Model -> Model
undoable = Undo.tag >> addToUndoStack

combiningUndoable : String -> Model -> Model
combiningUndoable = Undo.combiningTag >> addToUndoStack


focus : String -> Cmd Msg
focus s = Task.attempt (\_ -> NoOp) (Browser.Dom.focus s)

showAlert : Alert.Model -> Model -> Update
showAlert alert model =
  returnMC
    { model | alert = alert }
    (focus "project-alert")


adjustSelection : Sel.Selection -> Model -> Model
adjustSelection selection model =
  let
    related =
      Sel.computeRelated model.project.crossReference selection

    nameEditing =
      if Sel.selectedKind selection == Sel.selectedKind model.selection
        then model.nameEditing
        else False
  in
    { model
    | selection = selection
    , related = related
    , nameEditing = nameEditing
    }


updateProject : (DT.Project -> DT.Project) -> Model -> Model
updateProject fProj model =
  adjustSelection Sel.initSelection
    { model | project = DT.rebuildCrossReference <| fProj model.project }

makeEmptyProject : Model -> DT.Project
makeEmptyProject model =
  DT.emptyProject model.instrument.device model.projectSpec

processDrop : Sel.DropInfo -> Model -> Model
processDrop dropInfo model =
  let
    reselect get set shuf =
      get >> List.filterMap (Bank.Shuffle.rereference shuf) >> set

    reselectPatterns = reselect Sel.selectedPatterns Sel.selectPatterns
    reselectSamples = reselect Sel.selectedSamples Sel.selectSamples
    reselectSounds = reselect Sel.selectedSounds Sel.selectSounds

    process bankFn projFn selFn =
      let
        shuf =
          case dropInfo.dst of
            Nothing ->
              Bank.Shuffle.nullShuffle  -- the destination was not valid
            Just dst ->
              Elektron.Digitakt.Shuffle.dragAndDrop
                (shuffleSpec dropInfo.kind model)
                (List.map Bank.Index dropInfo.srcs)
                (Bank.Index dst)
                (bankFn model.project)
      in
        updateProject (projFn shuf) model
        |> adjustSelection (selFn shuf model.selection)
        |> undoable ("Rearrange " ++ bankName dropInfo.kind)

  in
    case dropInfo.kind of
      KPattern -> process .patterns   DT.shufflePatterns reselectPatterns
      KSample  -> process .samplePool DT.shuffleSamples  reselectSamples
      KSound   -> process .soundPool  DT.shuffleSounds   reselectSounds

updateSelection : (Sel.Selection -> Sel.Selection) -> Model -> Model
updateSelection selFn model =
  let
    selection = selFn model.selection
  in
    case Sel.dropInfo selection of
      Nothing -> adjustSelection selection model
      Just dropInfo -> processDrop dropInfo model

validateProject :
  String -> String -> Drive -> DT.Project -> PendingReceive -> Model -> Update
validateProject verb origin drive project0 pr model =
  let
    knownNames = Dict.union (Drive.fileNamesByHash drive) model.extraFileNames
    project = DT.updateSampleNames knownNames project0
    (userMessage, ok) = DT.validateProject project
    action = verb ++ " " ++ origin

    dispFn disp =
      case disp of
        LoadProject -> updateProject (always project) >> undoable action
        StartImport -> \m ->
          { m | importing = Just <| Import.init origin m.project project }

    nameFn name m = { m | projectName = name }

    modelFn =
      case pr of
        NothingPending -> identity
        FromDevice disp _ -> dispFn disp
        FromFile LoadProject name -> nameFn name >> dispFn LoadProject
        FromFile StartImport _  -> dispFn StartImport

    getSampleNames m =
      if ok
        then returnMR m
          (DT.neededSampleNames project
          |> List.map (\(hash, size) ->
            RequestMessage
            (Message.SampleFileInfoRequest hash size)
              -- yes, backwards from normal!
            ReceiveSampleFileInfo
          ))
        else returnM m

    model_ =
      if ok
        then modelFn model
        else model

    alert str =
      if ok
        then
          Alert.alert Alert.Warning
            (action ++ " warning:")
            str
        else
          Alert.alert Alert.Danger
            (action ++ " could not load the project:")
            str

    versionWarning = Nothing -- FIXME
--       case DT.projectVersions project of
--         Nothing -> Nothing
--         Just vers ->
--           if vers.projectSettingsVersion > model.versions.projectSettingsVersion
--           || vers.patternAndKitVersion > model.versions.patternAndKitVersion
--             then Just """
-- This project has patterns from a newer version of the Digitakt OS than your
-- machine has. Sending this project to your Digitakt will not work. Consider
-- upgrading your Digitakt to the lastest OS release.
--   """
--             else Nothing

    postUpdate =
      case List.filterMap identity [userMessage, versionWarning] of
        [] -> identity
        msgs -> thenUpdate <| showAlert (alert (String.join "\n\n" msgs))

  in
    returnM model_ |> thenUpdate getSampleNames |> postUpdate


receiveSampleFileInfo : Message.ElkMessage -> Model -> Update
receiveSampleFileInfo msg model =
  case msg of
    (Message.SampleFileInfoResponse ok size hash path) ->
      if ok
        then
          let
            name =
              case String.split "/" path |> List.reverse of
                [] -> "/"
                (n :: _) -> n
            hashSize = Drive.hashSize hash size
            justOne = Dict.singleton hashSize name
            extraFileNames = Dict.insert hashSize name model.extraFileNames
          in
          returnM
            { model
            | project = DT.updateSampleNames justOne model.project
            , extraFileNames = extraFileNames
            }
        else
          returnM model
    _ -> returnM model


startEdit : Model -> Update
startEdit model =
  case Sel.firstSelected model.selection of
    Just (k, i) ->
      if k == KPattern || k == KSound
        then
          returnMC
            { model | nameEditing = True }
            (focus (itemInputId k i))
        else
          returnM model
    Nothing -> returnM model

endEdit : Model -> Update
endEdit model = returnM { model | nameEditing = False }




receiveDump : Dump.ElkDump -> Drive -> Model -> Update
receiveDump dump drive model =
  let
    step m =
      case dump.message of
        Dump.DTPatternKitResponse i _     -> continue (Progress.update (i, 128)) m
        Dump.DTSoundResponse i _          -> continue (Progress.update (i, 0))   m
        Dump.DTProjectSettingsResponse _  -> allDone   Progress.finish           m
        _ -> alert "Unexpected dump type." m

    continue pfn m =
      returnM { m | progress = pfn m.progress }

    allDone pfn m =
      case m.pendingReceive of
        FromDevice disp p ->
          validateProject "Fetch" "from Digitakt" drive p m.pendingReceive
            { m
            | pendingReceive = NothingPending
            , progress = pfn m.progress
            }
          |> thenUpdate finished
        _ -> alert "Finished, but no project. Should never happen" m

    alert error m =
      showAlert (Alert.alert Alert.Danger "Error in Received Project" error)
        { m
        | pendingReceive = NothingPending
        , progress = Progress.finish m.progress
        }
     |> thenUpdate finished

    finished m = returnMR m [FinishDump]
  in
    case model.pendingReceive of
      FromDevice disp project ->
        case DT.updateFromDump dump project of
          Ok p -> step { model | pendingReceive = FromDevice disp p }
          Err error -> alert error model
      _ -> finished model


sendSysExJob : DT.Project -> UpdateJob
sendSysExJob project =
  Job.singleton DT.toSysExDumpsForSend project
  |> Job.mapProgress (\_ -> ((0, 0), 1 * Time.millisecond, Cmd.none))
  |> Job.andThen (\dumpsToWrite ->
    let
      n = List.length dumpsToWrite

      step i dumps =
        case dumps of
          [] -> Complete returnM  -- does nothing on completion!
          d :: ds ->
            let
              bytes = SysEx.SysEx.asByteArray d
              ms = toFloat (ByteArray.length bytes) / 175.0 + 20.0
            in
              NextStep
                ((i, n), ms * Time.millisecond,
                  Portage.sendMidi bytes)
                (\_ -> step (i + 1) ds)
    in
      \_ -> step 0 dumpsToWrite
    )


type alias ProcessJob = Job (Int, Int) (Model -> Update)

processJob : ProcessJob -> UpdateJob
processJob = Job.mapProgress (\ii -> (ii, 1 * Time.millisecond, Cmd.none))


writeSysExJob : String -> DT.Project -> Job (Int, Int) (Cmd Msg)
writeSysExJob fileName project =
  Job.singleton DT.toSysExDumpsForFile project
  |> Job.mapProgress (\_ -> (0, 0))
  |> Job.andThen (\dumpsToWrite ->
    let
      n = List.length dumpsToWrite
    in
      Job.indexedList (\i d -> SysEx.SysEx.asByteArray d) dumpsToWrite
      |> Job.mapProgress (\(i, a) -> (i, n))
      |> Job.map (Portage.writeBinaryFile fileName "application/octet-stream")
    )

readSysExJob : DT.Project -> ByteArray -> Job (Int, Int) (Result String DT.Project)
readSysExJob emptyProject allBytes =
  let
    n = ByteArray.length allBytes
    p b = (n - ByteArray.length b, n)

    step bytes project =
      if ByteArray.isEmpty bytes
        then Complete (Ok project)  -- should check that all read!
        else
          case SysEx.SysEx.readNextSysEx bytes of
            Ok (dump, rest) ->
              case DT.updateFromSysEx dump project of
                Ok proj_ -> NextStep (p rest) (\_ -> step rest proj_)
                Err error -> Complete (Err error)
            Err error -> Complete (Err error)

  in
    \_ -> NextStep (p allBytes) (\_ -> step allBytes emptyProject)


updateJob : Model -> UpdateJob -> Update
updateJob model job =
  case job () of
    NextStep (counts, pause, cmd) jobNext ->
      returnMC
        { model | progress = Progress.update counts model.progress }
        <| Cmd.batch
          [ cmd
          , Task.perform (\_ -> StepJob jobNext)
              (Process.sleep pause
                |> Task.andThen Task.succeed)
          ]
    Complete updateF ->
      updateF { model | progress = Progress.finish model.progress }


update : Msg -> Drive -> Model -> Update
update msg drive model =
  case msg of
    NoOp -> returnM model
    ClearProject ->
      returnM
        (updateProject (\_ -> makeEmptyProject model) model
        |> undoable "Clear project"
        )

    RequestProject disp ->
      returnMR
        { model
        | pendingReceive =
            FromDevice disp (makeEmptyProject model)
        , progress = Progress.start "Fetching project from Digitakt..."
        }
        [ StartDump
            (Dump.ElkDump model.instrument.device Dump.DTWholeProjectRequest)
            ReceiveDump
        ]

    ReceiveDump dump ->
      receiveDump dump drive model

    ReceiveSampleFileInfo info ->
      receiveSampleFileInfo info model

    SendProject ->
      let
        job = sendSysExJob model.project
        model_ = { model | progress = Progress.start "Sending project to Digitakt..."}
      in
        updateJob model_ job

    StartWriteProjectFile ->
      returnM { model | aboutToWriteFile = True }

    UpdateProjectName s ->
      returnM { model | projectName = s }

    CancelWriteProjectFile ->
      returnM { model | aboutToWriteFile = False }

    WriteProjectFile ->
      let
        finish cmd = \m -> returnMC m cmd
        fileName = model.projectName ++ ".syx"
        job = writeSysExJob fileName model.project |> Job.map finish
        title = "Writing project file " ++ fileName
        model_ =
          { model
          | aboutToWriteFile = False
          , progress = Progress.start title
          }
      in
        updateJob model_ (processJob job)

    SelectProjectFile disp items ->
      case items of
        (name, file) :: _ ->
          returnMC
            { model
            | pendingReceive =
                FromFile disp
                  <| if String.endsWith ".syx" name
                      then String.dropRight 4 name
                      else name
              , progress = Progress.start ("Reading project file " ++ name)
            }
            (Portage.readBinaryFile file)
        [] -> returnM model

    ReadProjectFile byteContents ->
      let
        finish result =
          case result of
            Ok p ->
              validateProject "Open" "Project File" drive p model.pendingReceive
            Err message ->
              showAlert
              <| Alert.alert Alert.Danger "Error reading project:" message

        emptyProject = makeEmptyProject model
        job = readSysExJob emptyProject byteContents |> Job.map finish
        model_ =
          { model
          | pendingReceive = NothingPending
          }
      in
        updateJob model_ (processJob job)

    CancelReadProjectFile message ->
      returnM { model | pendingReceive = NothingPending }


    StepJob job -> updateJob model job

    ImportMsg importMsg ->
      returnM
      <| case model.importing of
        Just i ->
          case Import.update importMsg i of
            Import.InProgress i_ -> { model | importing = Just i_ }
            Import.Canceled -> { model | importing = Nothing }
            Import.Imported origin p ->
              updateProject (always p) { model | importing = Nothing }
              |> undoable ("Import from " ++ origin)
        Nothing ->
          model

    SelectRelated k ->
      let
        model_ =
          adjustSelection (Sel.selectRelated k model.related) model
          |> undoable ("Select Related " ++ bankName k)
        cmd_ = focus (bankId k)
      in
        returnMC model_ cmd_

    SelectUnused k ->
      let
        sel = case k of
          KSample ->
            Sel.selectSamples
            <| DT.unusedSamples model.project.samplePool model.project.crossReference

          KSound ->
            Sel.selectSounds
            <| DT.unusedSounds model.project.soundPool model.project.crossReference

          _ -> model.selection

        model_ =
          adjustSelection sel model
          |> undoable ("Select Unused " ++ bankName k)
        cmd_ = focus (bankId k)
      in
        returnMC model_ cmd_

    SetSamplePoolOffset n ->
      returnM { model | samplePoolOffset = n }

    RenameItem k ->
      if Sel.kindStatus k model.selection == Sel.Selected
        then
          if model.nameEditing then endEdit model else startEdit model
        else
          returnM model

    ChangeItemName k i s ->
      let
        project_ =
          case k of
            KPattern -> DT.setPatternName (Bank.Index i) s model.project
            KSound -> DT.setSoundName (Bank.Index i) s model.project
            _ -> model.project
        model_ =
          { model | project = project_ }
          |> combiningUndoable ("Rename " ++ itemName k i)
      in
        returnM model_

    KeyDown k key ->
      if model.nameEditing && Sel.kindStatus k model.selection == Sel.Selected
        then
          case key of
            "Escape" -> endEdit model
            "Enter" -> endEdit model
            _ -> returnM model
        else
          case key of
            "Enter" -> startEdit model
            _ -> returnM model

    CompactItems k ->
      let
        compact = Elektron.Digitakt.Shuffle.compactDown (shuffleSpec k model)

        compactPatterns p = DT.shufflePatterns (compact p.patterns) p
        compactSamples p = DT.shuffleSamples (compact p.samplePool) p
        compactSounds p = DT.shuffleSounds (compact p.soundPool) p

        model_ =
          undoable ("Compact " ++ bankName k)
          <| case k of
            KPattern -> updateProject compactPatterns model
            KSample -> updateProject compactSamples model
            KSound -> updateProject compactSounds model
        cmd_ = focus (bankId k)
      in
        returnMC model_ cmd_

    SortItems k ->
      let
        sort = Elektron.Digitakt.Shuffle.sort .name (shuffleAllSpec k model)

        compactPatterns p = DT.shufflePatterns (sort p.patterns) p
        compactSamples p = DT.shuffleSamples (sort p.samplePool) p
        compactSounds p = DT.shuffleSounds (sort p.soundPool) p

        model_ =
          undoable ("Sort " ++ bankName k)
          <| case k of
            KPattern -> updateProject compactPatterns model
            KSample -> updateProject compactSamples model
            KSound -> updateProject compactSounds model
        cmd_ = focus (bankId k)
      in
        returnMC model_ cmd_

    DeleteItems k ->
      let
        go freeFn updateBank candidates =
          if List.all (\i -> freeFn i model.project.crossReference) candidates
            then
              let
                clearAll b = List.foldl Bank.clear b candidates
              in
                returnMC
                  (updateProject (updateBank clearAll) model
                    |> undoable ("Delete from " ++ bankName k)
                  )
                  (focus (bankId k))
            else
              let
                alert = Alert.alertMarkdown Alert.Warning """
## Cannot Delete

Some of these items are still in use.

Look at the patterns and sound sections to see which items are marked with
a **\u{21E2}** symbol. Those items need these items, so you can't delete them.

Use the **Select Unused** button (**\u{2205}**) button to select those items
that are not needed by any other item.
"""
              in
                showAlert alert model
      in
        case k of
          KPattern ->
            Sel.selectedPatterns model.selection
            |> go DT.freePattern (\fn p -> { p | patterns = fn p.patterns })

          KSample ->
            Sel.selectedSamples model.selection
            |> go DT.freeSample (\fn p -> { p | samplePool = fn p.samplePool })

          KSound ->
            Sel.selectedSounds model.selection
            |> go DT.freeSound (\fn p -> { p | soundPool = fn p.soundPool })


    SelectionItemMsg k i selMsg ->
      let
        model_ = updateSelection (Sel.update selMsg) model
        idFn = if model_.nameEditing then itemInputId else itemId
      in
        if Sel.isMouseDown selMsg
          then returnMC model_ (focus (idFn k i))
          else returnM model_

    SelectionGlobalMsg selMsg ->
      returnM <| updateSelection (Sel.update selMsg) model

    AlertMsg alertMsg ->
      returnM { model | alert = Alert.update alertMsg model.alert }

    UndoMsg undoMsg ->
      let
        (undoStack, mCheckpoint) = Undo.update undoMsg model.undoStack

        model_ =
          case mCheckpoint of
            Just (p, s) -> updateProject (always p) model |> adjustSelection s
            Nothing -> model
      in
        returnM { model_ | undoStack = undoStack }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map AlertMsg (Alert.subscriptions model.alert)
    , Sub.map SelectionGlobalMsg (Sel.subscriptions model.selection)
    , case model.importing of
        Nothing -> Sub.none
        Just i -> Sub.map ImportMsg <| Import.subscriptions i
    , case model.pendingReceive of
        FromFile _ _ ->
          Sub.batch
          [ Portage.binaryFileContents ReadProjectFile
          , Portage.binaryFileError CancelReadProjectFile
          ]
        _ -> Sub.none
     ]
