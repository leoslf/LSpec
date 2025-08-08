import LSpec.Core.Args
import LSpec.Core.Config
import LSpec.Core.Tree
import LSpec.Core.FailureReport
import LSpec.Core.Example
import LSpec.Core.Runner.Eval
import LSpec.Core.Runner.JobQueue
import LSpec.Core.Runner.Result
import LSpec.Core.Runner.PrintSlowSpecItems
import LSpec.Core.Spec.Monad

namespace LSpec.Core.Runner

open SpecTree (Item)
open Config (ColorMode UnicodeMode)
open Example (Params ActionWith ProgressCallback Result)

universe u

def failWith (reason : String) (item : Item a) : Item a :=
  { item with example_ }
 where
  example_ (params : Params) (hook : ActionWith a -> IO Unit) (progress : ProgressCallback) : IO Result := do
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
  example_ (params : Params) (hook : ActionWith a -> IO Unit) (progress : ProgressCallback) : IO Result := do
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
  withUnit (action : ActionWith Unit) : IO Unit :=
    action ()

  toEvalItem : Item Unit -> Eval.EvalItem
  | { requirement, location?, parallelizable, example_, .. } =>
    {
      description := requirement,
      location? := location?,
      concurrency := if parallelizable == .some true then .Concurrent else .Sequential,
      action := λprogress => Clock.measure $ example_ params withUnit progress,
    }

def specToEvalForest (seed : Seed) (config : Config) : SpecForest Unit -> Eval.EvalForest :=
  failItemsWithEmptyDescription config
  >>>> addDefaultDescriptions
  >>>> failFocusedItems config
  >>>> failPendingItems config
  -- >>>> Extension.applySpecTransformation config
  -- >>>> focusSpec config
  >>>> toEvalItemForest params
  -- >>>> applyDryRun config
  -- >>>> applyFilterPredicates config
  >>>> randomize
  -- FIXME:
  -- >>>> Forest.prune
 where
  params : Params := Params.mk

  randomize {c a : Type} : List (Tree c a) -> List (Tree c a) :=
    if config.randomize then
      Forest.randomize seed.toNat
    else
      id

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

def withHiddenCursor (progress : ProgressReporting) (stream : IO.FS.Stream) : IO a -> IO a :=
  match progress with
  | .Disabled => id
  | .Enabled => IO.bracket_ stream.hideCursor stream.showCursor

def getDefaultConcurrentJobs : IO Nat :=
  IO.nproc

def doNotLeakCommandLineArgumentsToExamples : ArgsT m a -> ArgsT m a :=
  withArgs []
