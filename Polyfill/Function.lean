infixr:1 " >>>> " => λf g => g ∘ f

universe u

variable {r : Type _}

instance (priority := high) instBinaryFunctionFunctor : Functor (r -> ·) where
  map := (· ∘ ·)

instance (priority := high) instBinaryFunctionApplicative : Applicative (r -> ·) where
  pure := Function.const _
  seq f g x := f x $ g () x

instance (priority := high) instBinaryFunctionMonad : Monad (r -> ·) where
  bind f k := λr => k (f r) r

/-- Transform the arguments via t before applying them on op -/
def on (op : b -> b -> c) (t : a -> b) : a -> a -> c :=
  λx y => op (t x) (t y)

