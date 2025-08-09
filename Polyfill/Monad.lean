/-- Promote a function to a monad -/
def liftM1 [Monad m] (f : a -> b) (ma : m a) : m b := do
  let a <- ma
  return (f a)

instance : MonadLift m m where
  monadLift := id
