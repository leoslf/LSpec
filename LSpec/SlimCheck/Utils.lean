namespace LSpec.SlimCheck.Utils

def liftHook (definition : r) (hook : (a -> IO Unit) -> IO Unit) (inner : a -> IO r) : IO r := do
  let ref <- IO.mkRef definition
  hook $ inner >=> ref.set
  ref.get
