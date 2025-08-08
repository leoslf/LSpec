import LSpec.Core.Args
import LSpec.Core.Tree
import LSpec.Core.Clock
import LSpec.Core.Config
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
  let filteredCount : Nat := Forest.size filteredSpec
  let specCount : Nat := Forest.size spec
  IO.eprintln! s!"filteredCount: {filteredCount}"
  if config.failOn.contains .empty && filteredCount == 0 then
    if specCount != 0 then
      die "all spec items have been filtered; failing due to --fail-on=empty"
  -- TODO
  let concurrentJobs <- config.concurrentJobs.elim getDefaultConcurrentJobs pure
  let results : SpecResult <- Functor.map toSpecResult ∘ withHiddenCursor colorMode.progressReporting (<- IO.getStdout) $ do
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
    let formatter := config.getFormatter (formatter? := .none) |>.getD $ V2.Formatter.toFormat V2.checks
    let format <- config.printSlowItems.elim id printSlowSpecItems <$> formatter formatConfig
    let evalConfig : Eval.Config := {
      format
      concurrentJobs
      failFast := config.failFast
      colorMode := if colorMode.shouldUseColor then .Enabled else .Disabled
    }
    IO.eprintln! s!"evalConfig: {evalConfig}"
    IO.eprintln! s!"filteredSpec: {filteredSpec}"
    Eval.runFormatter evalConfig filteredSpec

  pure results

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
    IO.eprintln s!"{config}"
    let oldFailureReport? <- FailureReport.readOnRerun config

    let normalMode := do
      IO.eprintln "normalMode"
      ArgsT.withArgs [] do
        SpecForest.runWithOldFailureReport oldFailureReport? forest config

    let rerunMode := do
      let result <- normalMode
      if rerunAll config oldFailureReport? result then
        lspecWithSpecResult defaults spec
      else
        return result

    if config.rerunAllOnSuccess then
      IO.eprintln "config.rerunAllOnSuccess"
      rerunMode
    else
      normalMode

def Summary.evaluate (summary : Summary) : ArgsT IO Unit := do
  unless summary.isSuccess do
    die "summary is not success"

def SpecResult.evaluate (result : SpecResult) : ArgsT IO Unit := do
  unless result.success do
    die "result is not success"

def lspecWith (config : Config) (spec : Spec) : ArgsT IO Unit := do
  lspecWithSpecResult config spec >>= SpecResult.evaluate

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
