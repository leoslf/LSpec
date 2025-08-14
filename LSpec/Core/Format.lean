import Std.Sync.SharedMutex
import Std.Internal.Async.Basic

import Concurrency.MVar

import LSpec.Prelude
import LSpec.Core.Seed
import LSpec.Core.Path
import LSpec.Core.DiffContext
import LSpec.Core.Clock

import LSpec.Core.Example

namespace LSpec.Core

open Std.Internal.IO.Async
open Concurrency (MVar)

open LSpec.Core.Example
open LSpec.Core.Clock

namespace Format

structure Item where
  mk ::
  location? : Option Location
  duration : Seconds
  info : String
  result : Result.Status
deriving Repr, BEq, TypeName

instance : ToString Item where
  toString := reprStr

inductive Event where
| Started : Event
| GroupStarted (path : Path) : Event
| GroupDone (path : Path) : Event
| Progress (path : Path) (progress : Progress) : Event
| ItemStarted (path : Path) : Event
| ItemDone (path : Path) (item : Item) : Event
| Done (results : List (Path × Item)) : Event
deriving Repr, BEq, Inhabited, TypeName

def Stream := IO.Ref IO.FS.Stream

instance : Repr Stream where
  reprPrec _ _ := "<stream>"

structure Config where
  mk ::
  useColor : Bool := false
  reportProgress : Bool := false
  outputUnicode : Bool := false
  useDiff : Bool := false
  diffContext? : Option DiffContext := .none
  externalDiff? : Option (String -> String -> IO Unit) := .none
  prettyPrint : Bool := false
  prettyPrintFunction : Option (String -> String -> String × String) := .none
  formatException : IO.Error -> String := IO.Error.formatExceptionWith toString
  printTimes : Bool := false
  htmlOutput : Bool := false
  printCpuTime : Bool := false
  usedSeed : Seed := 0
  expectedTotalCount : Nat := 0
  expertMode : Bool := false
  stream? : Option Stream := .none
deriving Repr, Inhabited, TypeName

inductive Signal where
| Ok : Signal
| NotOk (e : IO.Error) : Signal
deriving Inhabited, TypeName

end Format

open Format

abbrev Format := Format.Event -> IO Unit

partial def monadic [Monad m] [MonadLift BaseIO m] [MonadFinally m] (run : m Unit -> IO Unit) (format : Format.Event -> m Unit) : BaseIO Format := do
  let event : MVar Format.Event <- MVar.empty
  let done : MVar Signal <- MVar.empty

  let putEvent : Event -> BaseIO Unit := event.put

  let takeEvent {n} [Monad n] [MonadLift BaseIO n] : n Event :=
    liftM $ event.take

  let signal {n} [MonadLift BaseIO n] : Signal -> n Unit :=
    liftM ∘ done.put

  let wait : BaseIO Signal :=
    done.take

  let rec go : m Unit := do
    let event <- takeEvent
    format event
    match event with
    | .Done _ => pure ()
    | _ => do
      signal .Ok
      go

  let worker <- IO.asTask $ do
    try
      run go
      signal .Ok
    catch
    | e => do
      signal $ .NotOk e

  let format : Format := λevent => do
    if <- isRunning worker then
      putEvent event
      match <- wait with
      | .Ok => pure ()
      | .NotOk e => do
        let _ <- IO.wait worker
        throw e

  return format
 where
  isRunning {a} (task : Task a) : BaseIO Bool := do
    let state <- IO.getTaskState task
    return state == .running

