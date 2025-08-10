import Concurrency

import LSpec.Prelude

namespace LSpec.Core

inductive Concurrency where
| Sequential : Concurrency
| Concurrent : Concurrency
deriving Repr, DecidableEq

structure Semaphore where
  mk ::
  wait : IO Unit
  signal : IO Unit

def Semaphore.new (capacity : Nat) : BaseIO Semaphore := do
  let semaphore <- Concurrency.Semaphore.new capacity
  return Semaphore.mk semaphore.wait semaphore.signal

def Semaphore.bracket (self : Semaphore) : IO a -> IO a :=
  IO.bracket_ self.wait self.signal

abbrev CancelQueue := IO.Ref (List (Task Unit))

def CancelQueue.new (jobs : List (Task Unit) := []) :=
  IO.mkRef jobs

structure JobQueue where
  mk ::
  semaphore : Semaphore
  cancelQueue : CancelQueue

def JobQueue.new [Monad m] [MonadLift IO m] (concurrency : Nat := 0) : m JobQueue :=
  JobQueue.mk <$> Semaphore.new concurrency <*> CancelQueue.new []

def JobQueue.cancelAll [Monad m] [MonadLift IO m] (self : JobQueue) : m Unit :=
  self.cancelQueue.get >>= cancelMany
 where
  notifyCancel {a : Type _} (task : Task a) : m Unit := do
    IO.cancel task

  cancelMany {a : Type _} (jobs : List (Task a)) : m Unit := do
    jobs.forM notifyCancel
    jobs.forM λjob => IO.wait job *> pure ()

def withJobQueue [Monad m] [MonadFinally m] [MonadLift IO m] (concurrency : Nat) : (JobQueue -> m a) -> m a :=
  IO.bracket (JobQueue.new concurrency) JobQueue.cancelAll


variable {a : Type}
variable {progress : Type}
variable {m : Type -> Type} [Monad m]

abbrev Job (m : Type -> Type) (progress : Type) (a : Type) := (progress -> m Unit) -> m a

set_option linter.dupNamespace false

inductive Partial progress (a : Type) where
| Partial : progress -> Partial progress a
| Done : Partial progress a
deriving Inhabited

def Functor.void [Functor f] : f a -> f Unit :=
  Functor.map $ Function.const _ ()

#synth ∀{a : Type}, Nonempty (Except IO.Error a)
#synth ∀{a : Type}, Nonempty (Except IO.Error a)

partial def runConcurrently [ToString progress] [Monad m] [MonadLift IO m] (semaphore : Semaphore) (cancelQueue : CancelQueue) (action : Job BaseIO progress a) : IO (Job m progress (Except IO.Error a)) := do
  let result : Concurrency.MVar (Partial progress a) <- Concurrency.MVar.empty
  let worker : IO a := semaphore.bracket do
    try
      if (<- IO.checkCanceled) then
        throw $ IO.userError "canceled"

      let partialResult (p : progress) : BaseIO Unit := do
        result.put $ Partial.Partial p
      action partialResult
    finally
      result.put Partial.Done

  let pushOnCancelQueue (task : Task (Except IO.Error a)) : IO Unit := do
    cancelQueue.modify (·.concat $ task.map λ_ => ())

  let job <- IO.bracket (EIO.asTask worker) pushOnCancelQueue pure
  let rec waitForResult (notifyPartial : progress -> m Unit) : m (Except IO.Error a) := do
    match <- result.take with
    | .Partial progress => notifyPartial progress *> waitForResult notifyPartial
    | .Done => IO.wait job
  return waitForResult

def runSequentially [ToString progress] [Monad m] [MonadLift IO m] (cancelQueue : CancelQueue) (action : Job BaseIO progress a) : IO (Job m progress (Except IO.Error a)) := do
  let barrier : Concurrency.MVar Unit <- Concurrency.MVar.empty
  let wait : IO Unit := barrier.take
  let signal : m Unit := do
    barrier.put ()
  let pass := pure ()
  let job <- runConcurrently (Semaphore.mk wait pass) cancelQueue action
  return λnotifyPartial => signal *> job notifyPartial

def JobQueue.enqueue [ToString progress] [Monad m] [MonadLift IO m] (self : JobQueue) (concurrency : Concurrency) : Job BaseIO progress a -> IO (Job m progress (Except IO.Error a)) :=
  match concurrency with
  | .Sequential => runSequentially self.cancelQueue
  | .Concurrent => runConcurrently self.semaphore self.cancelQueue

