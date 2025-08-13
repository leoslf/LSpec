import LSpec.Core.Path
import LSpec.Core.Tree
import LSpec.Core.Clock
import LSpec.Core.Timer
import LSpec.Core.Config.Definition
import LSpec.Core.Example
import LSpec.Core.Format
import LSpec.Core.Runner.JobQueue

namespace LSpec.Core.Runner.Eval

open LSpec.Core (Example)
open LSpec.Core.Timer

structure EvalItem where
  mk ::
  description : String
  location? : Option Location
  concurrency : Concurrency
  action : Example.ProgressCallback -> BaseIO (Clock.Seconds × Example.Result)
deriving Repr

abbrev EvalTree := LSpec.Core.Tree (IO Unit) EvalItem
abbrev EvalForest := List EvalTree

structure Item (a) where
  description : String
  location? : Option Location
  action : a

def Item.map (f : a -> b) (self : Item a) : Item b :=
  { self with action := f self.action }

instance : Functor Item where
  map := Item.map

variable {m : Type -> Type} [Monad m]

abbrev RunningItem (m : Type -> Type) := Item (Path -> m (Clock.Seconds × Example.Result))
abbrev RunningTree c (m : Type -> Type) := LSpec.Core.Tree c (RunningItem m)
abbrev RunningForest c (m : Type -> Type) := List (RunningTree c m)

abbrev RunningItem_ (m : Type -> Type) := Item (Job m Example.Progress (Clock.Seconds × Example.Result))
abbrev RunningTree_ (m : Type -> Type) := LSpec.Core.Tree (IO Unit) (RunningItem_ m)
abbrev RunningForest_ (m : Type -> Type) := List (RunningTree_ m)

def _root_.LSpec.Core.JobQueue.enqueueItem [MonadLift IO m] (self : _root_.LSpec.Core.JobQueue) (item : EvalItem) : IO (RunningItem_ m) := do
  let job <- self.enqueue item.concurrency item.action
  return {
    description := item.description
    location? := item.location?
    action := job >=> liftM ∘ Except.elim exceptionToResult pure
  }
 where
  exceptionToResult (error : IO.Error) : IO (Clock.Seconds × Example.Result) :=
    (0, ·) <$> Example.Result.mk "" <$> Example.Result.Status.of error

def _root_.LSpec.Core.JobQueue.enqueueItems [MonadLift IO m] (self : _root_.LSpec.Core.JobQueue) : EvalForest -> IO (RunningForest_ m) :=
  List.mapM (traverse self.enqueueItem)

inductive ColorMode where
| Enabled : ColorMode
| Disabled : ColorMode
deriving Repr, BEq

structure Config where
  mk ::
  format : Format
  concurrentJobs : Nat
  failFast : Bool
  colorMode : ColorMode
deriving Repr

instance : ToString Config where
  toString := reprStr

structure Env where
  mk ::
  config : Config
  abort : IO.Ref Bool
  results : IO.Ref (List (Path × Format.Item))

def Env.new (config : Config) : BaseIO Env := do
  Env.mk config <$> IO.mkRef false <*> IO.mkRef []

abbrev EvalM := ReaderT Env IO

instance [TypeName a] : ToString (EvalM a) where
  toString _ := s!"EvalM {parenthesize $ reprStr $ TypeName.typeName a}"

instance : MonadLift IO EvalM := inferInstance

def abort : EvalM Unit := do
  let ref <- reads (·.abort)
  ref.set true

def shouldAbort : EvalM Bool := do
  let ref <- reads (·.abort)
  ref.get

def addResult (path : Path) (item : Format.Item) : EvalM Unit := do
  (<- reads (·.results)).modify ((path, item) :: ·)

def _root_.LSpec.Core.Format.Event.emit (event : Format.Event) : EvalM Unit := do
  let format <- reads (·.config.format)
  format event

def applyFailFast : (Example.Result -> Bool) -> RunningTree Unit IO -> RunningTree Unit EvalM :=
  Functor.map ∘ Functor.map ∘ instBinaryFunctionFunctor.map ∘ applyToItem
 where
  -- fmap''' (f : IO (Clock.Seconds × Example.Result) -> EvalM (Clock.Seconds × Example.Result)) : (Path -> IO (Clock.Seconds × Example.Result)) -> (Path -> EvalM (Clock.Seconds × Example.Result)) := instBinaryFunctionFunctor.map

  applyToItem (abortEarly : Example.Result -> Bool) (action : IO (Clock.Seconds × Example.Result)) : EvalM (Clock.Seconds × Example.Result) := do
    let result@(_, r) <- action
    if abortEarly r then
      abort
    return result

def mergeResults (callSite? : Option (String × Location)) (result : Example.Result) (status : Example.Result.Status) : Example.Result :=
  {
    result with
    status := result.status.merge status $ (·.snd) <$> callSite?
  }

def addCleanupToItem (shouldRunCleanup : Example.Result -> Bool) (location? : Option (String × Location)) (cleanup : IO Unit) (item : RunningItem IO) : RunningItem IO :=
  { item with action }
 where
  action (path : Path) : IO (Clock.Seconds × Example.Result) := do
    let result@(t, r) <- item.action path
    if shouldRunCleanup (r : Example.Result) then
      let action : ExpectationM Example.Result.Status := do
        liftM cleanup
        return .Success
      let (t', r') <- Clock.measure $ Example.Result.Status.safeEvaluate action
      return (t + t', mergeResults location? r r')
    return result

def mapHead (f : a -> a) : List a -> List a
| [] => []
| x :: xs => f x :: xs

partial def forLastLeaf (p : a -> a) : List (LSpec.Core.Tree Unit a) -> List (LSpec.Core.Tree Unit a) :=
  go
 where
  go :=
    let goNode : LSpec.Core.Tree Unit a -> LSpec.Core.Tree Unit a
    | .Node description children => .Node description $ go children
    | .NodeWithCleanup location? () children => .NodeWithCleanup location? () $ go children
    | .Leaf item => .Leaf $ p item
    List.reverse ∘ mapHead goNode ∘ List.reverse

def forEachLeaf (f : a -> b) : List (LSpec.Core.Tree Unit a) -> List (LSpec.Core.Tree Unit b) :=
  Functor.map (Functor.map f)

def applyCleanupAction (abortEarly : Example.Result -> Bool) (location? : Option (String × Location)) (cleanup : IO Unit) : RunningForest Unit IO -> RunningForest Unit IO :=
  forLastLeaf (addCleanupOn (not ∘ abortEarly)) ∘ forEachLeaf (addCleanupOn abortEarly)
 where
  addCleanupOn (predicate : Example.Result -> Bool) := addCleanupToItem predicate location? cleanup

def applyCleanup (abortEarly : Example.Result -> Bool) : RunningForest (IO Unit) IO -> RunningForest Unit EvalM :=
  List.map λtree => applyFailFast abortEarly $ go tree
 where
  go : RunningTree (IO Unit) IO -> RunningTree Unit IO
  | .Node label children =>
    .Node label $ children.map go
  | .NodeWithCleanup location? cleanup children =>
    .NodeWithCleanup location? () $ applyCleanupAction abortEarly location? cleanup $ children.map go
  | .Leaf item => .Leaf item

def sequenceActions : List (EvalM Unit) -> EvalM Unit :=
  go
 where
  go : List (EvalM Unit) -> EvalM Unit
  | [] => pure ()
  | action :: actions => do
    dbgTraceM' "action"
    action

    if <- shouldAbort then
      dbgTraceM' "shouldAbort"
      return ()

    go actions


structure FoldTree (c : Type) (a : Type) (r : Type) where
  onGroupStarted : Path -> r
  onGroupDone : Path -> r
  onCleanup : Option (String × Location) -> List String -> c -> r
  onLeaf : List String -> a -> r

variable {c a r : Type}

def FoldTree.fold (self : FoldTree c a r) : LSpec.Core.Tree c a -> List r :=
  go []
 where
  go (rGroups : List String) : LSpec.Core.Tree c a -> List r
  | .Node group xs =>
    let path := (rGroups.reverse, group)
    let start := self.onGroupStarted path
    let children := xs.flatMap $ go (group :: rGroups)
    let done := self.onGroupDone path
    start :: children.concat done
  | .NodeWithCleanup location? action xs =>
    let children := xs.flatMap $ go rGroups
    let cleanup := self.onCleanup location? rGroups.reverse action
    children.concat cleanup
  | .Leaf a => [self.onLeaf rGroups.reverse a]

open Format.Event in
def groupStarted : Path -> EvalM Unit :=
  emit ∘ GroupStarted

open Format.Event in
def groupDone : Path -> EvalM Unit :=
  emit ∘ GroupDone

def reportItemStarted (path : Path) : EvalM Unit :=
  Format.Event.ItemStarted path |>.emit

def reportItemDone (path : Path) (item : Format.Item) : EvalM Unit := do
  addResult path item
  Format.Event.ItemDone path item |>.emit

def reportResult (path : Path) (location? : Option Location) : Clock.Seconds × Example.Result -> EvalM Unit
| (duration, result) => do
  let colorMode <- reads (·.config.colorMode)
  reportItemDone path $ Format.Item.mk location? duration result.info $
    match result.status with
    | .Success => .Success
    | .Pending location? reason? =>
      .Pending location? reason?
    | .Failure location? error@(.Error _ _) =>
      .Failure (location? <|> /- TODO: .some extractLocation exception -/ none) error
    | .Failure location? error =>
      .Failure location? $
        match colorMode with
        | .Enabled => error
        | .Disabled =>
          match error with
          | .NoReason
          | .Reason _
          | .Canceled
          | .ExpectedButGot _ _ _ => error
          | .Error _ _ => error
          | .ColorizedReason reason => .Reason reason.stripAnsi

def reportItem (path : Path) (location? : Option Location) (action : EvalM (Clock.Seconds × Example.Result)) : EvalM Unit := do
  reportItemStarted path
  reportResult path location? =<< action

def eval (specs : RunningForest Unit EvalM) : EvalM Unit := do
  dbgTraceM' "eval"
  sequenceActions $ specs.flatMap foldSpec
 where
  evalItem (groups : List String) (item : RunningItem EvalM) : EvalM Unit := do
    let path : Path := (groups, item.description)
    reportItem path item.location? $ item.action path

  runCleanup (_ : Option (String × Location)) (_ : List String) : Unit -> EvalM Unit :=
    pure

  foldSpec : RunningTree Unit EvalM -> List (EvalM Unit) :=
    FoldTree.fold {
      onGroupStarted := groupStarted
      onGroupDone := groupDone
      onCleanup := runCleanup
      onLeaf := evalItem
    }

def runFormatter (config : Config) (specs : EvalForest) : IO (List (Path × Format.Item)) := do
  withJobQueue config.concurrentJobs λqueue => do
    dbgTraceM' "withJobQueue"
    withTimer 0.05 λtimer => do
      dbgTraceM' "withTimer"
      let env <- Env.new config
      let runningSpecs_ <- queue.enqueueItems specs
      let applyReportProgress (item : RunningItem_ IO) : RunningItem IO :=
        (· ∘ reportProgress timer) <$> item
      let abortEarly (result : Example.Result) : Bool :=
        config.failFast && result.status.isFailure
      let runningSpecs : RunningForest Unit EvalM :=
        applyCleanup abortEarly $ runningSpecs_.map $ Functor.map applyReportProgress
      let getResults := env.results.get
      let formatDone := getResults >>= format ∘ .Done
      format .Started

      try
        ReaderT.run (eval runningSpecs) env
      finally
        dbgTraceM' "before formatDone"
        formatDone

      let results <- getResults
      return results
 where
  format := config.format

  reportProgress (timer : BaseIO Bool) (path : Path) (progress : Example.Progress) : IO Unit := do
    if <- timer then
      format $ .Progress path progress
