namespace LSpec.Core

variable {a} [Repr a]

def matchList [BEq a] (xs : List a) (ys : List a) : Option String :=
  if extra.isEmpty && missing.isEmpty then
    .none
  else
    .some $
      "Actual list is not a permutation of expected list!\n" ++
      s!"  expected elements: {reprStr ys}\n" ++
      s!"  actual elements:   {reprStr xs}\n" ++
      if missing.isEmpty then "" else s!"  missing elements:  {reprStr missing}\n" ++
      if extra.isEmpty then "" else s!"  extra elements:  {reprStr extra}\n"
 where
  extra := xs.removeAll ys
  missing := ys.removeAll xs

