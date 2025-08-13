import Std.Internal.Async.Basic

open Std.Internal.IO.Async

namespace LSpec.Core.Clock

abbrev Seconds := Float

namespace Seconds

def toMilliseconds (seconds : Seconds) : Float :=
  seconds * 10 ^ 3

def toMicroseconds (seconds : Seconds) : Float :=
  seconds * 10 ^ 6

def fromMilliseconds (ms : Float) : Seconds :=
  ms / 10 ^ 3

def getMonotonicTime : BaseIO Seconds :=
  fromMilliseconds <$> Float.ofNat <$> IO.monoMsNow

def measure [Monad m] [MonadLift BaseIO m] (action : m a) : m (Seconds × a) := do
  let t0 <- Seconds.getMonotonicTime
  let a <- action
  let t1 <- Seconds.getMonotonicTime
  return (t1 - t0, a)

def sleep (seconds : Seconds) : BaseIO Unit := do
  IO.sleep $ seconds.toMilliseconds.toUInt32

def timeout [Monad m] [MonadFinally m] [MonadLift BaseIO m] /- [MonadAwait Task m] [MonadAsync t m] [MonadAwait t m] [Inhabited a] -/ (seconds : Seconds) (action : m a) : m (Option a) := do
  -- let watchdog : m (Option a) := do
  --   seconds.sleep
  --   return .none
  -- let action' : m (Option a) := do
  --   .some <$> action
  -- race (m := m) (t := t) watchdog action'
  -- FIXME:
  .some <$> action

end Seconds

export Seconds (measure sleep timeout)

