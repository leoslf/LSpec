import Init.Data.Ord
import LSpec.Prelude

namespace LSpec.Discover

universe u

abbrev Lex (α : Type u) := α

instance [Ord α] [Ord β] : Ord (Lex (α × β)) := lexOrd

#synth Ord (Lex (Char × Char))
#synth Ord (List (Lex (Char × Char)))

inductive Chunk where
| Numeric (n : Nat) (length : Nat) : Chunk
| Textual (pairs : List (Lex (Char × Char))) : Chunk
deriving Repr, Ord, BEq, DecidableEq

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
  compareOn (β := Lex (NaturalSortKey × Nat)) $ (Prod.first naturalSortKey) ∘ f
