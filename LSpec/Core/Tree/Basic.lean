import LSpec.Prelude

import LSpec.Core.Location
import LSpec.Core.Example

namespace LSpec.Core

open Example

universe u v

variable {c : Type u}
variable {a : Type v}

inductive Tree (c : Type u) (a : Type v) where
| Node (group : String) (children : List (Tree c a)) : Tree c a
| NodeWithCleanup (location? : Option (String × Location)) (cleanup : c) (children : List (Tree c a)) : Tree c a
| Leaf (item : a) : Tree c a
deriving Repr, BEq

abbrev Forest c a := List (Tree c a)

mutual
  def Tree.size : Tree c a -> Nat
  | .Node _ children => 1 + Forest.size children
  | .NodeWithCleanup _ _ children => 1 + Forest.size children
  | .Leaf _ => 1

  def Forest.size (forest : Forest c a) : Nat :=
    forest.map Tree.size |>.sum
end

def Tree.children : Tree c a -> Forest c a
| .Node _ children => children
| .NodeWithCleanup _ _ children => children
| .Leaf _ => []
