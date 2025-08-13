-- import Mathlib.Control.Traversable.Basic

universe u

def Prod.first (f : a -> a') : a × b -> a' × b
| (a, b) => (f a, b)

def Prod.second (g : b -> b') : a × b -> a × b'
| (a, b) => (a, g b)

export Prod (first second)

instance : Functor (Prod a) where
  map := Prod.second

def Prod.traverse {m : Type u -> Type u} [Applicative m] {b b' : Type u} (f : b -> m b') : a × b -> m (a × b')
| (x , y) => (x, ·) <$> f y

-- instance : Traversable (a × ·) where
--   traverse := Prod.traverse
