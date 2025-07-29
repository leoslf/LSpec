namespace LSpec.Core.Clock

abbrev Seconds := Float

namespace Seconds

def toMilliseconds (seconds : Seconds) : Float :=
  seconds * 10 ^ 3

def toMicroseconds (seconds : Seconds) : Float :=
  seconds * 10 ^ 6

def fromMilliseconds (ms : Float) : Seconds :=
  ms / 10 ^ 3

def getMonotonicTime : IO Seconds :=
  fromMilliseconds <$> Float.ofNat <$> IO.monoMsNow

end Seconds

def measure (action : IO a) : IO (Seconds × a) := do
  let t0 <- Seconds.getMonotonicTime
  let a <- action
  let t1 <- Seconds.getMonotonicTime
  return (t1 - t0, a)

def sleep (seconds : Seconds) : IO Unit := do
  IO.sleep $ seconds.toMilliseconds.toUInt32

def timeout (seconds : Seconds) (action : IO a) : IO (Option a) := do
  sorry
  -- let token <- IO.CancelToken.new
  -- let watchdog <- IO.asTask $ do
  --   sleep seconds
  --   token.set
  -- let result <- do
  --   try
  --     action
  --   catch
  --   | e => do
  --     IO.cancel watchdog
  -- IO.cancel watchdog
  -- return result

