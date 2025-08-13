-- import Mathlib.Algebra.Group.Defs
-- import Mathlib.Data.List.Monad

import LSpec.Polyfill.Monoid

-- instance : Monoid (List α) where
--   one := []
--   mul := List.append
--   mul_assoc := List.append_assoc
--   one_mul := List.nil_append
--   mul_one := List.append_nil

instance : Monad List.{u} where
  pure x := [x]
  bind xs f := xs.flatMap f
  map f xs := xs.map f

partial def List.groupBy (predicate : a -> a -> Bool) : List a -> List (List a)
| [] => []
| x :: xs =>
  let (group, rest) := xs.partition (predicate x)
  (x :: group) :: List.groupBy predicate rest
