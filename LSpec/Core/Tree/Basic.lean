import LSpec.Prelude

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
deriving BEq

abbrev Forest c a := List (Tree c a)

mutual
  def Tree.reprPrec [Repr a] (self : Tree c a) (prec : Nat) : Std.Format :=
    match self with
    | .Node group children =>
      s!"Node {group} {Forest.reprPrec children prec}"
    | .NodeWithCleanup location? _ children =>
      s!"NodeWithCleanup {location?} <cleanup> {Forest.reprPrec children prec}"
    | .Leaf item => s!"Leaf {Repr.reprPrec item prec}"

  def Forest.reprPrec [Repr a] (self : Forest c a) (prec : Nat) : Std.Format :=
    "[" ++ (", " : Std.Format).joinSep (self.map (Tree.reprPrec · prec)) ++ "]"
end

@[default_instance]
instance (priority := high) [Repr a] : Repr (Tree c a) where
  reprPrec := Tree.reprPrec

@[default_instance]
instance (priority := high) [Repr a] : ToString (Tree c a) where
  toString self := self.reprPrec 0 |>.pretty


instance [Repr a] : Repr (Forest c a) where
  reprPrec := Forest.reprPrec

def Tree.foldl (f : b -> Tree c a -> b) (initial : b) (self : Tree c a) : b :=
  match self with
  | .Node _ children => children.foldl f $ f initial self
  | .NodeWithCleanup _ _ children => children.foldl f $ f initial self
  | .Leaf _ => f initial self

def Tree.foldr (f : Tree c a -> b -> b) (initial : b) (self : Tree c a) : b :=
  match self with
  | .Node _ children => children.foldr f $ f self initial
  | .NodeWithCleanup _ _ children => children.foldr f $ f self initial
  | .Leaf _ => f self initial

mutual
  def Tree.size : Tree c a -> Nat
  | .Node _ children => 1 + Forest.size children
  | .NodeWithCleanup _ _ children => 1 + Forest.size children
  | .Leaf _ => 1

  def Forest.size (forest : Forest c a) : Nat :=
    forest.map Tree.size |>.sum
end

def Tree.leafs (self : Tree c a) : Nat :=
  self.foldr (Function.const _ Nat.succ) 0

def Forest.leafs (self : Forest c a) : Nat :=
  self.map Tree.leafs |>.sum

def Tree.children : Tree c a -> Forest c a
| .Node _ children => children
| .NodeWithCleanup _ _ children => children
| .Leaf _ => []

def location /- [HasCallStack] -/ (_ : Unit) : Option Location :=
  (Location.of ∘ Prod.snd) <$> callSite ()

def toModuleName (path : System.FilePath) : String :=
  ".".intercalate $ path.parent.get!.components.concat path.fileStem.get!

def formatDefaultDescription : Location -> String
| { file, line, column } => s!"{toModuleName file}[{line}:{column}]"

