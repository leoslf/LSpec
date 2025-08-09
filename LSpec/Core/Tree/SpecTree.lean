import LSpec.Prelude

import LSpec.Core.Annotations
import LSpec.Core.Example
import LSpec.Core.Expectations

import LSpec.Core.Tree.Basic
import LSpec.Core.Tree.Instances

namespace LSpec.Core

open Example

namespace SpecTree

structure Item (a : Type u) where
  mk ::
  /-- Textual description of behaviour -/
  requirement : String
  location? : Option Location
  /-- A flag that indicates whether it is safe to evaluate this spec item in parallel with other spec items -/
  parallelizable? : Option Bool
  /-- A flag that indicates whether this spec item is focused -/
  isFocused : Bool
  annotations : Annotations
  example_ (params : Params) (hook : Hook a) (progress : ProgressCallback) : BaseIO Result

def Item.reprPrec (self : Item a) (prec : Nat) : Std.Format :=
  "SpecTree.Item"
    ++ " (requirement: " ++ Repr.reprPrec self.requirement prec ++ ") "
    ++ " (location?: " ++ Repr.reprPrec self.location? prec ++ ")"
    ++ " (parallelizable?: " ++ Repr.reprPrec self.parallelizable? prec ++ ")"
    ++ s!" (isFocused: {self.isFocused})"
    ++ " (<annotations>)"
    ++ " (<example_>)"

instance : Repr (Item a) where
  reprPrec := Item.reprPrec

def Item.setAnnotation [TypeName V] (value : V) (item : Item a) : Item a :=
  { item with annotations := item.annotations.setValue value }

def Item.getAnnotation [TypeName V] (item : Item a) : Option V :=
  item.annotations.getValue

end SpecTree

abbrev SpecTree a := Tree (IO Unit) (SpecTree.Item a)

partial def SpecTree.any (predicate : SpecTree.Item a -> Bool) : SpecTree a -> Bool
| .Node _ children => children.any $ SpecTree.any predicate
| .NodeWithCleanup _ _ children => children.any $ SpecTree.any predicate
| .Leaf item => predicate item

abbrev SpecForest a := List (SpecTree a)

def SpecForest.map (f : SpecTree.Item a -> SpecTree.Item b) : SpecForest a -> SpecForest b :=
  List.map (Tree.map f)

def SpecForest.mapIf (predicate : SpecTree.Item a -> Bool) (f : SpecTree.Item a -> SpecTree.Item a) : SpecForest a -> SpecForest a :=
  SpecForest.map λitem =>
    if predicate item then
      f item
    else
      item

def SpecForest.any (predicate : SpecTree.Item a -> Bool) (self : SpecForest a) : Bool :=
  List.any self $ SpecTree.any predicate

def SpecForest.focus (self : SpecForest a) : SpecForest a :=
  if self.any SpecTree.Item.isFocused then
    self
  else
    self.map λitem => { item with isFocused := true }

/-- Combines a list of specs into a larger spec -/
def specGroup (label : String) : SpecForest a -> SpecTree a :=
  .Node message
 where
  message : String :=
    if label.isEmpty then
      location () |>.elim "(no description given)" formatDefaultDescription
    else
      label

def specItem [Example e] (label : String) (example_ : e) : SpecTree (Arg e) :=
  .Leaf {
    requirement := label
    location? := location ()
    parallelizable? := .none
    isFocused := false
    annotations := {}
    example_ := Example.safeEvaluate example_
  }
