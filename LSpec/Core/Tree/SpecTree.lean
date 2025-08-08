import LSpec.Prelude

import LSpec.Core.Annotations
import LSpec.Core.Example

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
  parallelizable : Option Bool
  /-- A flag that indicates whether this spec item is focused -/
  isFocused : Bool
  annotations : Annotations
  example_ : Params -> (ActionWith a -> IO Unit) -> ProgressCallback -> IO Result

def Item.setAnnotation [TypeName V] (value : V) (item : Item a) : Item a :=
  { item with annotations := item.annotations.setValue value }

def Item.getAnnotation [TypeName V] (item : Item a) : Option V :=
  item.annotations.getValue

end SpecTree

abbrev SpecTree a := Tree (IO Unit) (SpecTree.Item a)

abbrev SpecForest a := List (SpecTree a)

def SpecForest.map (f : SpecTree.Item a -> SpecTree.Item b) : SpecForest a -> SpecForest b :=
  List.map (Tree.map f)

def SpecForest.mapIf (predicate : SpecTree.Item a -> Bool) (f : SpecTree.Item a -> SpecTree.Item a) : SpecForest a -> SpecForest a :=
  SpecForest.map λitem =>
    if predicate item then
      f item
    else
      item

def location /- [HasCallStack] -/ (_ : Unit) : Option Location :=
  (Location.of ∘ Prod.snd) <$> callSite ()

def toModuleName (path : System.FilePath) : String :=
  ".".intercalate $ path.parent.get!.components.concat path.fileStem.get!

def formatDefaultDescription : Location -> String
| { file, line, column } => s!"{toModuleName file}[{line}:{column}]"

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
    parallelizable := .none
    isFocused := false
    annotations := {}
    example_ := Example.safeEvaluate example_
  }
