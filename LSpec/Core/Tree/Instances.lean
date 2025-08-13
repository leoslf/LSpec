import LSpec.Prelude

import LSpec.Core.Tree.Basic
import LSpec.Core.Tree.Lemmas

namespace LSpec.Core

universe u v

section Functor

def Tree.map (f : a -> b) : (self : Tree c a) -> Tree c b
| .Node group children =>
  .Node group $ children.map $ Tree.map f
| .NodeWithCleanup location action children =>
  .NodeWithCleanup location action $ children.map $ Tree.map f
| .Leaf item =>
  .Leaf $ f item

def Forest.map (f : a -> b) : (self : Forest c a) -> Forest c b :=
  List.map (Tree.map f)

instance : Functor (Tree c) where
  map := Tree.map

instance : Functor (Forest c) where
  map := Forest.map

end Functor

section Bifunctor

def Tree.bimap (g : a -> b) (f : c -> d) : (self : Tree a c) -> Tree b d :=
  go
 where
  go
  | .Node d children =>
    .Node d $ children.map go
  | .NodeWithCleanup location? action children =>
    .NodeWithCleanup location? (g action) (children.map go)
  | .Leaf item =>
    .Leaf $ f item

-- instance : Bifunctor Tree where
--   bimap := Tree.bimap

def Forest.bimap (g : a -> b) (f : c -> d) : Forest a c -> Forest b d :=
  List.map $ Tree.bimap g f

-- instance : Bifunctor Forest where
--   bimap := Forest.bimap

end Bifunctor

section Traversable

mutual
  def Tree.traverse {a b : Type u} {F : Type u -> Type u} [Applicative F] (f : a -> F b) : (self : Tree c a) -> F (Tree c b)
  | .Node group children =>
    .Node group <$> Forest.traverse f children
  | .NodeWithCleanup location? cleanup children =>
    .NodeWithCleanup location? cleanup <$> Forest.traverse f children
  | .Leaf x =>
    .Leaf <$> f x
  termination_by
    self => self.size
  decreasing_by
    simp_wf
    apply Tree.children_size_lt_Node_size
    apply Tree.children_size_lt_NodeWithCleanup_size

  def Forest.traverse {a b : Type u} {F : Type u -> Type u} [Applicative F] (f : a -> F b) : (self : Forest c a) -> F (Forest c b)
  | self => List.traverse (λtree => tree.traverse f) self
  termination_by
    self => self.size
  decreasing_by
    -- FIXME
    sorry
end

instance : Traversable (Tree c) where
  traverse := Tree.traverse

instance : Traversable (Forest c) where
  traverse := Forest.traverse

end Traversable

mutual
  partial def Tree.filter_ (groups : List String) (p : List String -> a -> Bool) : Tree c a -> Option (Tree c a)
  | .Node group children =>
    .some $ .Node group $ Forest.filter_ (groups.concat group) p children
  | .NodeWithCleanup location action children =>
    .some $ .NodeWithCleanup location action $ Forest.filter_ groups p children
  | .Leaf item =>
    .Leaf <$> guarded (p groups) item

  partial def Forest.filter_ (groups : List String) : (List String -> a -> Bool) -> Forest c a -> Forest c a :=
    List.filterMap ∘ Tree.filter_ groups
end

mutual
  partial def Tree.prune : Tree c a -> Option (Tree c a)
  | .Node group children =>
    .Node group <$> prune' children
  | .NodeWithCleanup location action children =>
    .NodeWithCleanup location action <$> prune' children
  | node@(.Leaf _) => .some node
  where
    prune' := guarded (not ∘ List.isEmpty) ∘ Forest.prune

  partial def Forest.prune : Forest c a -> Forest c a :=
    List.filterMap Tree.prune
end

def Tree.filterWithLabels : (List String -> a -> Bool) -> Tree c a -> Option (Tree c a) :=
  Tree.filter_ []

def Forest.filterWithLabels : (List String -> a -> Bool) -> Forest c a -> Forest c a :=
  Forest.filter_ []

def Tree.filter : (a -> Bool) -> Tree c a -> Option (Tree c a) :=
  Tree.filterWithLabels ∘ Function.const _

def Forest.filter : (a -> Bool) -> Forest c a -> Forest c a:=
  Forest.filterWithLabels ∘ Function.const _

mutual
  def Tree.shuffle [RandomGen G] (ref : ST.Ref s G) : Tree c a -> ST s (Tree c a)
  | .Node group children =>
    .Node group <$> children.shuffle ref
  | .NodeWithCleanup location action children =>
    .NodeWithCleanup location action <$> children.shuffle ref
  | .Leaf item => pure $ .Leaf item

  def Forest.shuffle [RandomGen G] (ref : ST.Ref s G) (forest : Forest c a) : ST s (Forest c a) :=
    List.shuffle ref forest >>= List.mapM (Tree.shuffle ref)
end

def Forest.randomize (seed : Nat) (forest : Forest c a) : Forest c a :=
  runST $ λ_ => do
    let ref <- ST.mkRef $ mkStdGen seed
    forest.shuffle ref

