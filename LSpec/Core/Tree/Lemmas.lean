import Mathlib.Algebra.Order.BigOperators.Group.List
import Mathlib.Algebra.Order.Group.Nat
import Mathlib.Control.Traversable.Basic

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
    simp [Tree.size]
  | NodeWithCleanup _ _ children =>
    simp [Tree.size]

theorem Forest.child_size_le_size
  {child : Tree c a}
  {children : Forest c a}
  (h : child ∈ children) :
  child.size <= children.size := by
  have h' : Tree.size child ∈ (children.map Tree.size) :=
    List.mem_map_of_mem h
  -- Unfold Forest.size
  rw [Forest.size]
  -- Apply le_sum_of_mem to get the inequality
  exact List.le_sum_of_mem h'

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

-- theorem Tree.child_size_le_forest_size
--   (child : Tree c a)
--   (children : List (Tree c a))
--   (h : child ∈ children) :
--   child.size ≤ Forest.size children := by
--   -- Use that child.size ∈ (children.map Tree.size)
--   have h' : Tree.size child ∈ (children.map Tree.size) :=
--     List.mem_map_of_mem h
--   -- Unfold Forest.size
--   rw [Forest.size]
--   -- Apply le_sum_of_mem to get the inequality
--   exact List.le_sum_of_mem h'
--
-- theorem Forest.size_lt_parent_Node_size
--   (group : String)
--   (children : List (Tree c a)) :
--   Forest.size children < (Tree.Node group children).size := by
--   -- Unfold the RHS
--   rw [Tree.size]
--   -- Goal becomes: Forest.size children < 1 + Forest.size children
--   apply Nat.lt_succ_self
--
-- theorem Forest.size_lt_parent_NodeWithCleanup_size
--   (location? : Option (String × Location))
--   (cleanup : c)
--   (children : List (Tree c a)) :
--   Forest.size children < (Tree.NodeWithCleanup location? cleanup children).size := by
--   -- Unfold RHS
--   rw [Tree.size]
--   -- same logic: Forest.size children < 1 + Forest.size children
--   apply Nat.lt_succ_self
--
-- theorem Tree.child_size_lt_Node_size
--   (child : Tree c a)
--   (children : List (Tree c a))
--   (group : String)
--   (h : child ∈ children) :
--   child.size < (Tree.Node group children).size := by
--   have h' : child.size ≤ Forest.size children :=
--     Tree.child_size_le_forest_size child children h
--   have h'' : Forest.size children < (Tree.Node group children).size :=
--     Forest.size_lt_parent_Node_size group children
--   exact Nat.lt_of_le_of_lt h' h''
--
--
-- theorem Nat.add_pos_of_pos_of_nonneg
--   {a b : Nat}
--   (h : 0 < a) :
--   0 < a + b := by
--   apply Nat.lt_add_right b h
--
-- theorem Nat.add_pos_of_nonneg_of_pos
--   {a b : Nat}
--   (h : 0 < b) :
--   0 < a + b := by
--   apply Nat.lt_add_left a h
--
-- theorem List.pos_sum_of_mem_map
--   {α : Type _}
--   (f : α -> Nat)
--   {xs : List α} {x : α}
--   (h_mem : x ∈ xs)
--   (h_pos : 0 < f x) :
--   0 < (xs.map f).sum := by
--   induction xs with
--   | nil =>
--     simp at h_mem
--
--   | cons y ys ih =>
--     simp only [List.map, List.sum, List.foldr]
--     by_cases h : x = y
--     · subst h
--       exact Nat.add_pos_of_pos_of_nonneg h_pos
--
--     · simp at h_mem
--       have x_in_ys : x ∈ ys := h_mem.resolve_left h
--       have ih_pos := ih x_in_ys
--       exact Nat.add_pos_of_nonneg_of_pos ih_pos
--
-- theorem Forest.sum_of_nonempty_positive
--   {forest : Forest c a}
--   (h : forest ≠ []) :
--   0 < forest.size := by
--   cases forest with
--   | nil => contradiction
--   | cons hd tl =>
--     have size_pos := Tree.size_gt_zero hd
--     have tail_size_nonneg : 0 ≤ Forest.size tl := Nat.zero_le _
--     simp [Forest.size]
--     apply Nat.add_pos_of_pos_of_nonneg size_pos tail_size_nonneg
--
-- theorem Forest.head_size_lt_cons_size
--   {t : Tree c a} {forest : List (Tree c a)} (h : forest ≠ []) :
--   t.size < Forest.size (t :: forest) := by
--   simp [Forest.size]
--   -- Goal becomes: t.size < t.size + (forest.map Tree.size).sum
--   have pos : 0 < (forest.map Tree.size).sum :=
--     Forest.sum_of_nonempty_positive h
--   exact Nat.lt_add_of_pos_right pos

