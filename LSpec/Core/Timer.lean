import LSpec.Prelude
import LSpec.Core.Clock

namespace LSpec.Core.Timer

open LSpec.Core.Clock (Seconds)

def worker (delay : Seconds) (ref : IO.Ref Bool) : BaseIO Unit := do
  while true do
    delay.sleep
    ref.set true

def withTimer (delay : Seconds) (action : IO Bool -> IO a) : IO a := do
  let ref <- IO.mkRef false
  IO.bracket
    (before := (BaseIO.asTask $ worker delay ref : BaseIO (Task Unit)))
    (after := λtask => (IO.cancel task : BaseIO Unit))
    λ_ => do
      action $ ref.modifyGet (false, ·)
