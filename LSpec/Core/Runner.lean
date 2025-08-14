import LSpec.Core.Args
import LSpec.Core.Config
import LSpec.Core.Tree
import LSpec.Core.FailureReport
import LSpec.Core.Example
import LSpec.Core.Formatters.V2
import LSpec.Core.Spec
import LSpec.Core.Runner.Eval
import LSpec.Core.Runner.JobQueue
import LSpec.Core.Runner.Result
import LSpec.Core.Runner.PrintSlowSpecItems
import LSpec.Core.Runner.Cmd

namespace LSpec.Core


open Config (ColorMode UnicodeMode)
open SpecTree (Item)
open Example (Params Hook ProgressCallback Result)
open Formatters
open Runner

universe u

def failWith (reason : String) (item : Item a) : Item a :=
  { item with example_ }
 where
  example_ (params : Params) (hook : Hook a) (progress : ProgressCallback) : BaseIO Result := do
    match (<- item.example_ params hook progress) with
    | { info, status } =>
      pure $ Result.mk info $
        match status with
        | .Failure _ _ => status
        | _ => .Failure .none $ .Reason reason

def failIf (predicate : Item a -> Bool) (reason : String) : SpecForest a -> SpecForest a :=
  SpecForest.mapIf predicate $ failWith reason

def failPending (item : Item a) : Item a :=
  { item with example_ }
 where
  example_ (params : Params) (hook : Hook a) (progress : ProgressCallback) : BaseIO Result := do
    match (<- item.example_ params hook progress) with
    | { info, status } =>
      pure $ Result.mk info $
        match status with
        | .Pending location _ => .Failure location $ .Reason "item is pending; failing due to --fail-on=pending"
        | _ => status

def failPendingItems (config : Config) : SpecForest a -> SpecForest a :=
  if config.failOn.contains .pending then
    SpecForest.map failPending
  else
    id

def failItemsWithEmptyDescription (config : Config) : SpecForest a -> SpecForest a :=
  if config.failOn.contains .emptyDescription then
    failIf (String.isEmpty ∘ Item.requirement) $ "item has no description; failing due to --fail-on=empty-description"
  else
    id

def failFocusedItems (config : Config) : SpecForest a -> SpecForest a :=
  if config.failOn.contains .focused then
    failIf Item.isFocused $ "item is focused; failing due to --fail-on=focused"
  else
    id

def addDefaultDescriptions : SpecForest a -> SpecForest a :=
  SpecForest.map addDefaultDescription
 where
  addDefaultDescription (item : Item a) : Item a :=
    if item.requirement.isEmpty then
      { item with requirement := item.location? |>.elim "(unspecified behavior)" formatDefaultDescription }
    else
      item

def toEvalItemForest (params : Params) : SpecForest Unit -> List Eval.EvalTree :=
  Forest.bimap id toEvalItem ∘ Forest.filter Item.isFocused
 where
  withUnit : Hook Unit := λaction => action ()

  toEvalItem : Item Unit -> Eval.EvalItem
  | { requirement, location?, parallelizable?, example_, .. } =>
    {
      description := requirement,
      location? := location?,
      concurrency := if parallelizable? == .some true then .Concurrent else .Sequential,
      action := λprogress => Clock.measure $ example_ params withUnit progress,
    }

def focusSpec (config : Config) (spec : SpecForest a) : SpecForest a :=
  if config.focusedOnly then
    spec
  else
    spec.focus

def applyDryRun (config : Config) : Eval.EvalForest -> Eval.EvalForest :=
  if config.dryRun then
    Forest.bimap removeCleanup markSuccess
  else
    id
 where
  removeCleanup : IO Unit -> IO Unit := λ_ => pass'

  markSuccess (item : Eval.EvalItem) : Eval.EvalItem :=
    { item with action := λ_ => pure (0, Example.Result.mk "" .Success) }

def applyFilterPredicates (config : Config) : Forest c Eval.EvalItem -> Forest c Eval.EvalItem :=
  Forest.filterWithLabels predicate
 where
  includes : Path -> Bool := config.filter?.getD $ Function.const _ true
  skips : Path -> Bool := config.skip?.getD $ Function.const _ false

  predicate (groups : List String) (item : Eval.EvalItem) : Bool :=
    let path := (groups, item.description)
    includes path && not (skips path)

def specToEvalForest (seed : Seed) (config : Config) : SpecForest Unit -> Eval.EvalForest :=
  id
  >>>> dbgWith (s!"before: {·}")
  >>>> failItemsWithEmptyDescription config
  >>>> dbgWith (s!"after failItemsWithEmptyDescription: {·}")
  >>>> addDefaultDescriptions
  >>>> dbgWith (s!"after addDefaultDescriptions: {·}")
  >>>> failFocusedItems config
  >>>> dbgWith (s!"after failFocusedItems: {·}")
  >>>> failPendingItems config
  >>>> dbgWith (s!"after failPendingItems: {·}")
  -- >>>> Extension.applySpecTransformation config
  -- >>>> dbgTraceVal
  >>>> focusSpec config
  >>>> dbgWith (s!"after focusSpec: {·}")
  >>>> toEvalItemForest params
  >>>> dbgWith (s!"after toEvalItemForest: {·}")
  >>>> applyDryRun config
  >>>> dbgWith (s!"after applyDryRun: {·}")
  >>>> applyFilterPredicates config
  >>>> dbgWith (s!"applyFilterPredicates: {·}")
  >>>> randomize
  >>>> dbgWith (s!"after randomize: {·}")
  -- FIXME:
  >>>> Forest.prune
  >>>> dbgWith (s!"after Forest.prune: {·}")
 where
  params : Params := Params.mk

  randomize {c a : Type} : List (Tree c a) -> List (Tree c a) :=
    if config.randomize then
      Forest.randomize seed.toNat
    else
      id

  debug := false
  dbgWith {α} (f : α -> String) (value : α) : α := Id.run do
    if debug then
      return dbgTraceWith f value
    return value

inductive ProgressReporting where
| Disabled : ProgressReporting
| Enabled : ProgressReporting
deriving Repr, BEq

inductive UseColor where
| Disabled : UseColor
| Enabled (mode : ProgressReporting) : UseColor
deriving Repr, BEq

def UseColor.shouldUseColor : UseColor -> Bool
| .Disabled => false
| .Enabled _ => true

def UseColor.progressReporting : UseColor -> ProgressReporting
| .Disabled => .Disabled
| .Enabled progressReporting => progressReporting

def githubActions : IO Bool :=
  (· == .some "true") <$> IO.getEnv "GITHUB_ACTIONS"

def noColor : IO Bool :=
  Option.isSome <$> IO.getEnv "NO_COLOR"

def colorOutputSupported (mode : ColorMode) (isTerminalDevice : IO Bool) : IO UseColor := do
  let github <- githubActions
  let buildkite <- (· == .some "true") <$> IO.getEnv "BUILDKITE"
  let progress : ProgressReporting :=
    if github || buildkite then
      .Disabled
    else
      .Enabled
  match mode with
  | .auto => (if github || · then .Enabled progress else .Disabled) <$> colorTerminal
  | .never => pure .Disabled
  | .always => pure $ .Enabled progress
 where
  colorTerminal : IO Bool := do
    (· && ·) <$> ((!·) <$> noColor) <*> isTerminalDevice

def unicodeOutputSupported (mode : UnicodeMode) (stream : IO.FS.Stream) : IO Bool :=
  match mode with
  | .auto => (· == .some "UTF-8") <$> Functor.map toString <$> stream.getEncoding
  | .never => pure false
  | .always => pure true

def withHiddenCursor [Monad m] [MonadFinally m] [MonadLift IO m] (progress : ProgressReporting) (stream : IO.FS.Stream) : m a -> m a :=
  match progress with
  | .Disabled => id
  | .Enabled => IO.bracket_ stream.hideCursor stream.showCursor

def getDefaultConcurrentJobs : IO Nat :=
  IO.nproc

def doNotLeakCommandLineArgumentsToExamples : ArgsT m a -> ArgsT m a :=
  withArgs []

def SpecForest.runWithOldFailureReport (oldFailureReport? : Option FailureReport) (spec : SpecForest Unit) (config : Config) : ArgsT IO SpecResult := do
  let (seed, config) <- FailureReport.apply oldFailureReport? config |>.ensureSeed
  let stdout <- IO.getStdout
  let colorMode <- colorOutputSupported config.colorMode $ stdout.supportsANSI
  let outputUnicode <- unicodeOutputSupported config.unicodeMode stdout
  let filteredSpec := specToEvalForest seed config spec
  let filteredCount : Nat := Forest.leafs filteredSpec
  let specCount : Nat := Forest.leafs spec
  -- dbgTraceM s!"filteredCount: {filteredCount}"
  if config.failOn.contains .empty && filteredCount == 0 then
    if specCount != 0 then
      die "all spec items have been filtered; failing due to --fail-on=empty"

  -- TODO
  let concurrentJobs <- config.concurrentJobs?.elim getDefaultConcurrentJobs pure
  let stdout <- IO.getStdout
  let results : SpecResult <- Functor.map toSpecResult ∘ withHiddenCursor colorMode.progressReporting stdout $ do
    let formatConfig : Format.Config := {
      useColor := colorMode.shouldUseColor
      reportProgress := colorMode.progressReporting == .Enabled
      outputUnicode
      useDiff := config.diff
      diffContext? := config.diffContext?
      externalDiff? :=
        if config.diff then
          (· $ config.diffContext?) <$> config.externalDiff?
        else
          .none
      prettyPrint := config.prettyPrint
      prettyPrintFunction :=
        if config.prettyPrint then
          .some $ config.prettyPrintFunction outputUnicode
        else
          .none
      formatException := config.formatException
      printTimes := config.times
      htmlOutput := config.htmlOutput
      printCpuTime := config.printCpuTime
      usedSeed := seed
      expectedTotalCount := filteredCount
      expertMode := config.expertMode
    }
    -- FIXME: do we get rid of the V1.Formatter
    let formatter := config.getFormatter (formatterV1? := .none) |>.getD $ V2.Formatter.toFormat V2.checks
    let format <- config.printSlowItems.elim id printSlowSpecItems <$> formatter formatConfig
    let evalConfig : Eval.Config := {
      format
      concurrentJobs
      failFast := config.failFast
      colorMode := if colorMode.shouldUseColor then .Enabled else .Disabled
    }
    -- dbgTraceM s!"evalConfig: {evalConfig}"
    -- dbgTraceM s!"filteredSpec: {filteredSpec}"
    Eval.runFormatter evalConfig filteredSpec

  return results

def SpecForest.run (spec : SpecForest Unit) (config : Config) : ArgsT IO SpecResult := do
  let oldFailureReport <- FailureReport.readOnRerun config
  SpecForest.runWithOldFailureReport oldFailureReport spec config

def rerunAll (config : Config) (oldFailureReport? : Option FailureReport) (result : SpecResult) : Bool :=
  match oldFailureReport? with
  | .none => false
  | .some oldFailureReport =>
       config.rerunAllOnSuccess
    && config.rerun
    && result.success
    && !oldFailureReport.paths.isEmpty


partial def lspecWithSpecResult (defaults : Config) (spec : Spec) : ArgsT IO SpecResult := do
  match <- spec.evaluate defaults with
  | (config, forest) =>
    let args <- ArgsT.getArgs
    let config <- readConfig cmd config args
    -- dbgTraceM s!"{config}"
    let oldFailureReport? <- FailureReport.readOnRerun config

    let normalMode := do
      -- dbgTraceM "normalMode"
      let results <- ArgsT.withArgs [] do
        SpecForest.runWithOldFailureReport oldFailureReport? forest config
      -- dbgTraceM "after SpecForest.runWithOldFailureReport"
      return results

    let rerunMode := do
      -- dbgTraceM "config.rerunAllOnSuccess"
      let result <- normalMode
      if rerunAll config oldFailureReport? result then
        lspecWithSpecResult defaults spec
      else
        return result

    if config.rerunAllOnSuccess then
      rerunMode
    else
      normalMode

def Summary.evaluate (summary : Summary) : ArgsT IO Unit := do
  -- dbgTraceM s!"{summary}"
  unless summary.isSuccess do
    die "summary is not success"

def SpecResult.evaluate (result : SpecResult) : ArgsT IO Unit := do
  -- dbgTraceM s!"{result}"
  unless result.success do
    die "result is not success"

def lspecWithResult (config : Config) : Spec -> ArgsT IO Summary :=
  Functor.map SpecResult.toSummary ∘ lspecWithSpecResult config

def lspecWith (config : Config) (spec : Spec) : ArgsT IO Unit := do
  SpecResult.evaluate =<< lspecWithSpecResult config spec

def lspec (spec : Spec) : ArgsT IO Unit :=
  lspecWith (config := default) spec

