import Batteries.Data.List.Basic

namespace LSpec


class Traversable (t : Type u -> Type u) extends Functor t where
  /-- The function commuting a traversable functor t with an arbitrary applicative functor m. -/
  traverse {f : Type u -> Type u} [Applicative f] {α β : Type u} : (α -> f β) -> t α -> f (t β)

export Traversable (traverse)

variable {t : Type u -> Type u} [Traversable t]
variable {f : Type u -> Type u} [Applicative f]
variable {α β : Type u}

def sequence : t (f α) -> f (t α) :=
  traverse id

instance : Traversable Id := ⟨id⟩

instance : Traversable Option where
  traverse f
  | .none => pure .none
  | .some x => .some <$> f x

instance : Traversable List := ⟨List.traverse⟩

