import LSpec.Core.Expectations

namespace LSpec.SlimCheck.Utils

open LSpec.Core

def liftHook [Monad m] [MonadLift BaseIO m] (definition : r) (hook : (a -> m Unit) -> m Unit) (inner : a -> m r) : m r := do
  let ref <- IO.mkRef definition
  hook $ inner >=> ref.set
  ref.get
