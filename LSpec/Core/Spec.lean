import LSpec.Core.Args
import LSpec.Core.Tree
import LSpec.Core.Clock
import LSpec.Core.Config
import LSpec.Core.Expectations
import LSpec.Core.FailureReport
import LSpec.Core.Runner
import LSpec.Core.Runner.Cmd
import LSpec.Core.Formatters.V2

namespace LSpec.Core

open Runner
open Formatters

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
      let result <- normalMode
      if rerunAll config oldFailureReport? result then
        lspecWithSpecResult defaults spec
      else
        return result

    if config.rerunAllOnSuccess then
      -- dbgTraceM "config.rerunAllOnSuccess"
      rerunMode
    else
      normalMode

def Summary.evaluate (summary : Summary) : ArgsT IO Unit := do
  -- IO.eprintln! summary
  unless summary.isSuccess do
    die "summary is not success"

def SpecResult.evaluate (result : SpecResult) : ArgsT IO Unit := do
  -- IO.eprintln! result
  unless result.success do
    die "result is not success"

def lspecWith (config : Config) (spec : Spec) : ArgsT IO Unit := do
  SpecResult.evaluate =<< lspecWithSpecResult config spec
  IO.Process.exit 0

def lspec (spec : Spec) : ArgsT IO Unit :=
  lspecWith default spec

def describe (label : String) : SpecWith a -> SpecWith a :=
  withEnv pushLabel ∘ mapSpecForest (pure ∘ specGroup label)
 where
  pushLabel : Env -> Env
  | { specDescriptionPath } => Env.mk $ label :: specDescriptionPath

def context : String -> SpecWith a -> SpecWith a := describe

def it [Example a] (label : String) (action : a) : SpecWith (Example.Arg a) := do
  fromSpecList [specItem label action]

def setParallelizable (value : Bool) (item : SpecTree.Item a) : SpecTree.Item a :=
  { item with parallelizable? := item.parallelizable? <|> .some value }

def parallel : SpecWith a -> SpecWith a :=
  mapSpecItem (setParallelizable true)

def sequential : SpecWith a -> SpecWith a :=
  mapSpecItem (setParallelizable false)

-- FIXME:
-- def pending : ExpectationM Unit := do
--   throw $ .Pending (location ()) .none
--
-- def pending_ : ExpectationM Unit := do
--   throw $ .Pending .none .none

def getSpecDescriptionPath : SpecM a (List String) := do
  List.reverse <$> reads Env.specDescriptionPath
