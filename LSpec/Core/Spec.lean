import LSpec.Core.Tree
import LSpec.Core.Clock
import LSpec.Core.Config
import LSpec.Core.Runner
import LSpec.Core.FailureReport

import LSpec.Core.Spec.Monad

namespace LSpec.Core

open Runner

def rerunAll (config : Config) (oldFailureReport? : Option FailureReport) (result : SpecResult) : Bool :=
  match oldFailureReport? with
  | .none => false
  | .some oldFailureReport =>
       config.rerunAllOnSuccess
    && config.rerun
    && result.success
    && !oldFailureReport.paths.isEmpty

partial def lspecWithSpecResult (defaults : Config) (spec : Spec) : IO SpecResult := do
  match (<- evalSpec defaults spec) with
  | (config, forest) =>
    let config <- IO.getArgs >>= readConfig config
    let oldFailureReport? <- FailureReport.readOnRerun config

    let normalMode : IO SpecResult :=
      doNotLeakCommandLineArgumentsToExamples $ runSpecForest_ oldFailureReport? forest config

    if config.rerunAllOnSuccess then
      let result <- normalMode
      if rerunAll config oldFailureReport? result then
        lspecWithSpecResult defaults spec
      else
        return result
    else
      normalMode

def evaluateSummary (summary : Summary) : IO Unit := do
  unless summary.isSuccess do
    IO.Process.exit 1

def evaluateResult (result : SpecResult) : IO Unit := do
  unless result.success do
    IO.Process.exit 1

def lspecWith (defaults : Config) (spec : Spec) : IO Unit := do
  lspecWithSpecResult defaults spec >>= evaluateResult

def lspec : Spec -> IO Unit :=
  lspecWith default

def describe (label : String) : SpecWith a -> SpecWith a :=
  withEnv pushLabel ∘ mapSpecForest (pure ∘ specGroup label)
 where
  pushLabel : Env -> Env
  | { specDescriptionPath } => Env.mk $ label :: specDescriptionPath

def context : String -> SpecWith a -> SpecWith a := describe

def it [Example a] (label : String) (action : a) : SpecWith (Example.Arg a) :=
  fromSpecList [specItem label action]
