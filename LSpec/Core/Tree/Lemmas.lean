-- import Mathlib.Algebra.Order.BigOperators.Group.List
-- import Mathlib.Algebra.Order.Group.Nat
-- import Mathlib.Control.Traversable.Basic

import LSpec.Prelude

import LSpec.Core.Annotations
import LSpec.Core.Example
import LSpec.Core.Tree.Basic

namespace LSpec.Core

universe u v

variable {c : Type u}
variable {a : Type v}
variable {group : String}
variable {location? : Option (String × Location)}
variable {cleanup : c}
variable {children : Forest c a}

theorem Tree.size_gt_zero
  (tree : Tree c a) :
  0 < tree.size := by
  cases tree with
  | Leaf _ =>
    simp [Tree.size]
  | Node _ children =>
    sorry -- simp [Tree.size]
  | NodeWithCleanup _ _ children =>
    sorry -- simp [Tree.size]

theorem Forest.child_size_le_size
  {child : Tree c a}
  {children : Forest c a}
  (h : child ∈ children) :
  child.size <= children.size := by
  have h' : Tree.size child ∈ (children.map Tree.size) :=
    -- List.mem_map_of_mem h
    sorry
  -- Unfold Forest.size
  rw [Forest.size]
  -- Apply le_sum_of_mem to get the inequality
  -- exact List.le_sum_of_mem h'
  sorry

theorem Tree.size_eq_one_plus_children_size
  (tree : Tree c a) :
  tree.size = 1 + tree.children.size := by
  cases tree with
  | Leaf _ =>
    simp [Tree.size, Tree.children, Forest.size]
  | Node _ children =>
    simp [Tree.size, Tree.children, Forest.size]
  | NodeWithCleanup _ _ children =>
    simp [Tree.size, Tree.children, Forest.size]

theorem Tree.children_size_lt_Node_size
  {children : Forest c a} :
  children.size < (Tree.Node group children).size := by
  let tree := Tree.Node group children
  rw [tree.size_eq_one_plus_children_size]
  rw [Nat.add_comm]
  apply Nat.lt_add_one

theorem Tree.children_size_lt_NodeWithCleanup_size
  {children : Forest c a} :
  children.size < (Tree.NodeWithCleanup location? cleanup children).size := by
  let tree := Tree.NodeWithCleanup location? cleanup children
  rw [tree.size_eq_one_plus_children_size]
  rw [Nat.add_comm]
  apply Nat.lt_add_one

theorem Tree.children_size_lt_size
  (tree : Tree c a) :
  tree.children.size < tree.size := by
  rw [tree.size_eq_one_plus_children_size]
  rw [Nat.add_comm]
  apply Nat.lt_add_one

theorem Tree.child_size_lt_parent_Node_size
  (child : Tree c a)
  (children : Forest c a)
  (h_membership : child ∈ children) :
  child.size < (Tree.Node group children).size := by
  let parent := Tree.Node group children
  -- we are proving 1 + child.size ≤ parent.size instead as 1 + m <= n <-> m < n
  suffices 1 + child.size ≤ parent.size from Nat.one_add_le_iff.mp this
  -- parent.size = 1 + children.size
  rw [parent.size_eq_one_plus_children_size]
  -- 1 + child.size <= parent.size
  exact Nat.add_le_add_left (Forest.child_size_le_size h_membership) 1

theorem Tree.child_size_lt_parent_NodeWithCleanup_size
  (child : Tree c a)
  (children : Forest c a)
  (h_membership : child ∈ children) :
  child.size < (Tree.NodeWithCleanup location? cleanup children).size := by
  let parent := Tree.NodeWithCleanup location? cleanup children
  -- we are proving 1 + child.size ≤ parent.size instead as 1 + m <= n <-> m < n
  suffices 1 + child.size ≤ parent.size from Nat.one_add_le_iff.mp this
  -- parent.size = 1 + children.size
  rw [parent.size_eq_one_plus_children_size]
  -- 1 + child.size <= parent.size
  exact Nat.add_le_add_left (Forest.child_size_le_size h_membership) 1


