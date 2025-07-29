import Mathlib.Algebra.Group.Defs
import Mathlib.Data.List.Monad

instance : Monoid (List α) where
  one := []
  mul := List.append
  mul_assoc := List.append_assoc
  one_mul := List.nil_append
  mul_one := List.append_nil
