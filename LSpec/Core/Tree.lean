import LSpec.Prelude

import LSpec.Core.Annotations
import LSpec.Core.Location
import LSpec.Core.Example

namespace LSpec.Core

open Example

universe u v

inductive Tree (c : Type u) (a : Type v) where
| Node (group : String) (children : List (Tree c a)) : Tree c a
| NodeWithCleanup (location : Option (String × Location)) (action : c) (children : List (Tree c a)) : Tree c a
| Leaf (item : a) : Tree c a
deriving Repr, BEq

def Tree.map (f : a -> b) : Tree c a -> Tree c b
| .Node group children =>
  .Node group $ children.map $ Tree.map f
| .NodeWithCleanup location action children =>
  .NodeWithCleanup location action $ children.map $ Tree.map f
| .Leaf item =>
  .Leaf $ f item

instance : Functor (Tree c) where
  map := Tree.map

def Tree.bimap (g : a -> b) (f : c -> d) : Tree a c -> Tree b d := go
 where
  go
  | .Node d children =>
    .Node d $ children.map go
  | .NodeWithCleanup location action children =>
    .NodeWithCleanup location (g action) (children.map go)
  | .Leaf item =>
    .Leaf $ f item

abbrev Forest c a := List (Tree c a)

mutual
  def Tree.count : Tree c a -> Nat
  | .Node _ children => Forest.count children
  | .NodeWithCleanup _ _ children => Forest.count children
  | .Leaf _ => 1

  def Forest.count (forest : Forest c a) : Nat :=
    forest.map Tree.count |>.sum
end


-- def mapOption (f : a -> Option b) : List a -> List b
-- | [] => []
-- | x :: xs =>
--   let rs := mapOption f xs
--   match f x with
--   | .none => rs
--   | .some r => r :: rs

mutual
  partial def Forest.filter_ (groups : List String) : (List String -> a -> Bool) -> Forest c a -> Forest c a :=
    List.filterMap ∘ Tree.filter_ groups

  partial def Tree.filter_ (groups : List String) (p : List String -> a -> Bool) : Tree c a -> Option (Tree c a)
  | .Node group children =>
    .some $ .Node group $ Forest.filter_ (groups.concat group) p children
  | .NodeWithCleanup location action children =>
    .some $ .NodeWithCleanup location action $ Forest.filter_ groups p children
  | .Leaf item =>
    .Leaf <$> guarded (p groups) item
end

mutual
  partial def Forest.prune : Forest c a -> Forest c a :=
    List.filterMap Tree.prune
  partial def Tree.prune : Tree c a -> Option (Tree c a)
  | .Node group children =>
    .Node group <$> prune' children
  | .NodeWithCleanup location action children =>
    .NodeWithCleanup location action <$> prune' children
  | node@(.Leaf _) => .some node
  where
    prune' := guarded (¬List.isEmpty .) ∘ Forest.prune
end

def Tree.filterWithLabels : (List String -> a -> Bool) -> Tree c a -> Option (Tree c a) :=
  Tree.filter_ []

def Tree.filter : (a -> Bool) -> Tree c a -> Option (Tree c a) :=
  Tree.filterWithLabels ∘ Function.const _

def Forest.filterWithLabels : (List String -> a -> Bool) -> Forest c a -> Forest c a :=
  Forest.filter_ []

def Forest.filter : (a -> Bool) -> Forest c a -> Forest c a:=
  Forest.filterWithLabels ∘ Function.const _

def Forest.bimap (g : a -> b) (f : c -> d) : Forest a c -> Forest b d :=
  List.map $ Tree.bimap g f

mutual
  def Forest.shuffle [RandomGen G] (ref : ST.Ref s G) (forest : Forest c a) : ST s (Forest c a) :=
    List.shuffle ref forest >>= List.mapM (Tree.shuffle ref)

  def Tree.shuffle [RandomGen G] (ref : ST.Ref s G) : Tree c a -> ST s (Tree c a)
  | .Node group children =>
    .Node group <$> children.shuffle ref
  | .NodeWithCleanup location action children =>
    .NodeWithCleanup location action <$> children.shuffle ref
  | .Leaf item => pure $ .Leaf item
end

def Forest.randomize (seed : Nat) (forest : Forest c a) : Forest c a :=
  runST $ λ _ => do
    let ref <- ST.mkRef $ mkStdGen seed
    forest.shuffle ref

namespace SpecTree

structure Item (a : Type u) where
  mk ::
  /-- Textual description of behaviour -/
  requirement : String
  location : Option Location
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
    location := location ()
    parallelizable := .none
    isFocused := false
    annotations := {}
    example_ := safeEvaluateExample example_
  }
