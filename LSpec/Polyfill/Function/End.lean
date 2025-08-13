-- import Mathlib.Algebra.Group.Prod
import LSpec.Polyfill.Semigroup
import LSpec.Polyfill.Monoid

namespace LSpec

variable (α) in
def Function.End := α -> α

-- instance : Monoid (Function.End α) where
--   one := id
--   mul := (· ∘ ·)
--   mul_assoc _ _ _ := rfl
--   mul_one _ := rfl
--   one_mul _ := rfl
--   npow n f := f^[n]
--   npow_succ _ _ := Function.iterate_succ _ _

instance : Semigroup (Function.End α) where
  op := Function.comp
  stimes n f _ := Id.run do
    let mut result := f
    for _ in [0:n] do
      result := result ∘ f
    return result
  sconcat fs _ := fs.foldl Function.comp id

instance : Monoid (Function.End α) where
  mempty := id
  mappend := Semigroup.op
  mconcat := List.foldr Semigroup.op id
