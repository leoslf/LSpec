namespace LSpec.Core.Formatters

inductive Diff where
| first : String -> Diff
| second : String -> Diff
| both : String -> Diff
deriving Repr, BEq, Inhabited

inductive LineDiff where
| first : List String -> LineDiff
| second : List String -> LineDiff
| both : List String -> LineDiff
| singleLineDiff : List Diff -> LineDiff
| omitted : Nat -> LineDiff
deriving Repr, BEq, Inhabited


def lineDiff (context? : Option Nat) (expected : String) (actual : String) : List LineDiff :=
  -- FIXME:
  []
 --  context?.elim id applyContext $ singleLineDiffs diffs
 -- where
 --  diffs : List LineDiff

