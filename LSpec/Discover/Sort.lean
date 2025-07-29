import LSpec.Prelude

namespace LSpec.Discover

inductive Chunk where
| Numeric (n : Nat) (length : Nat) : Chunk
| Textual (pairs : List (Char × Char)) : Chunk
deriving Repr, Ord, DecidableEq

abbrev NaturalSortKey := List Chunk

partial def naturalSortKey : String -> NaturalSortKey :=
  chunks ∘ String.toList
 where
  chunks : List Char -> NaturalSortKey
  | [] => []
  | s@(c :: _) =>
    if c.isDigit then
      let (num, rest) := s.span Char.isDigit |>.first List.asString
      .Numeric (num.toNat!) num.length :: chunks rest
    else
      let (str, rest) := s.span Char.isAlpha
      .Textual (str.map λc => (c.toLower, c)) :: chunks rest

def compareNaturallyBy (f : a -> String × Nat) : a -> a -> Ordering :=
  compareOn $ (Prod.first naturalSortKey) ∘ f
