universe u

/--
Church encoding of fix points (CPS encoding)
Fix f = ∀r, (f r -> r) -> r
-/
abbrev FixF (f : Type u -> Type u) :=  ∀{r : Type u}, (f r -> r) -> r

variable {f : Type u -> Type u} [Functor f]

def FixF.fold {r : Type u} (ϕ : f r -> r) (self : FixF f) : r :=
  self ϕ
