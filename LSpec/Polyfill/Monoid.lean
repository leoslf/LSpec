import LSpec.Polyfill.Semigroup

namespace LSpec

universe u
variable {α β : Type u}

class Monoid (α) extends Semigroup α where
  mempty : α
  mappend : α -> α -> α := Semigroup.op
  mconcat : List α -> α := List.foldr mappend mempty

export Monoid (mempty mappend mconcat)

instance : Monoid Unit where
  mempty := ()
  mappend _ _ := ()
  mconcat _ := ()

instance : Monoid (List α) where
  mempty := []
  mappend := Semigroup.op
  mconcat := List.flatten

instance [Monoid α] [Monoid β] : Monoid (α × β) where
  mempty := (mempty, mempty)

instance : Monoid Ordering where
  mempty := .eq
  mappend := Semigroup.op
  mconcat := List.foldr Semigroup.op .eq

instance [Semigroup α] : Monoid (Option α) where
  mempty := .none
  mappend := Semigroup.op
  mconcat := List.foldr Semigroup.op .none

instance [Monoid α] : Applicative (α × ·) where
  pure x := (mempty, x)
  seq mf ma :=
    match mf, ma () with
    | (u, f), (v, x) => (u <> v, f x)

instance [Monoid α] : Monad (α × ·) where
  bind
  | (u, a), k =>
    match k a with
    | (v, b) => (u <> v, b)

instance [Monoid a] : Monoid (IO a) where
  mempty := pure mempty
