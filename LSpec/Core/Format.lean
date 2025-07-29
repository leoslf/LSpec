import Std.Sync.SharedMutex

import LSpec.Prelude
import LSpec.Core.Path
import LSpec.Core.Location
import LSpec.Core.DiffContext
import LSpec.Core.Clock

import LSpec.Core.Example

namespace LSpec.Core

open LSpec.Core.Example
open LSpec.Core.Clock

namespace Format

structure Event.Item where
  location : Option Location
  duration : Seconds
  info : String
  result : Result.Status
deriving Repr, BEq

inductive Event where
| Created : Event
| Started : Event
| GroupStarted (path : Path) : Event
| GroupDone (path : Path) : Event
| Progress (path : Path) (progress : Progress) : Event
| ItemStarted (path : Path) : Event
| ItemDone (path : Path) (item : Event.Item) : Event
| Done (results : List (Path × Event.Item)) : Event
deriving Repr, BEq

structure Config where
  mk ::
  useColor : Bool
  reportProgress : Bool
  outputUnicode : Bool
  useDiff : Bool
  diffContext : Option DiffContext
  externalDiff : Option (String -> String -> IO Unit)
  prettyPrint : Bool
  prettyPrintFunction : Option (String -> String -> String × String)
  formatException : IO.Error -> String
  printTimes : Bool
  htmlOutput : Bool
  printCpuTime : Bool
  usedSeed : Nat
  expectedTotalCount : Nat
  expertMode : Bool
-- deriving Repr

instance : Inhabited Config where
  default := {
    useColor := false,
    reportProgress := false,
    outputUnicode := false,
    useDiff := false,
    diffContext := .none,
    externalDiff := .none,
    prettyPrint := false,
    prettyPrintFunction := .none,
    formatException := IO.Error.formatExceptionWith toString,
    printTimes := false,
    htmlOutput := false,
    printCpuTime := false,
    usedSeed := 0,
    expectedTotalCount := 0,
    expertMode := false,
  }

inductive Signal where
| Ok : Signal
| NotOk (e : IO.Error) : Signal

end Format


open Format

abbrev Format := Format.Event -> IO Unit

partial def monadic [MonadIO m] (run : m Unit -> IO Unit) (format : Format.Event -> m Unit) : IO Format := do
  let event <- Std.SharedMutex.new Event.Created
  let done <- Std.SharedMutex.new Signal.Ok

  let putEvent : Event -> IO Unit :=
    event.atomically ∘ set

  let takeEvent {n} [MonadIO n] : n Event :=
    MonadIO.liftIO $ event.atomicallyRead $ read

  let signal {n} [MonadIO n] : Signal -> n Unit :=
    MonadIO.liftIO ∘ done.atomically ∘ set

  let wait : IO Signal :=
    done.atomicallyRead $ read

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

  let result (event : Event) : IO Unit := do
    unless (<- isRunning worker) do
      putEvent event
      match (<- wait) with
      | .Ok => pure ()
      | .NotOk e => do
        let _ <- IO.wait worker
        throw e

  pure result
 where
  isRunning {a} (task : Task a) : IO Bool := do
    (· == .running) <$> IO.getTaskState task

abbrev Formatter := String × (Format.Config -> IO Format)

