universe u v

variable {m : Type u -> Type v} [Monad m]

instance {a : Type u} : Repr (m a) where
  reprPrec _ _ := "M (a)"

instance {a : Type u} {b : Type v} : Repr (a -> b) where
  reprPrec _ _ := " ->"

instance [Repr a] : ToString a where
  toString := reprStr

