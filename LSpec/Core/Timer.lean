import LSpec.Prelude
import LSpec.Core.Clock

namespace LSpec.Core.Timer

open LSpec.Core.Clock (Seconds)

def worker (delay : Seconds) (ref : IO.Ref Bool) : BaseIO Unit := do
  while not (<- IO.checkCanceled) do
    dbgTrace s!"delay: {delay}" pure
    delay.sleep
    ref.set true
  ref.set false

def withTimer [Monad m] [MonadFinally m] [MonadLift IO m] (delay : Seconds) (action : BaseIO Bool -> m a) : m a := do
  let ref <- IO.mkRef false
  let before : m (Task Unit) := BaseIO.asTask do
    worker delay ref
  let after (task : Task Unit) : m Unit := do
    IO.eprintln! "cancelling task"
    IO.cancel task

  IO.bracket before after λ(task : Task Unit) => do
    action $ ref.modifyGet (false, ·)
