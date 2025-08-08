import Lake.Util.EStateT
import Lean
import Mathlib.Control.ULiftable

set_option pp.universes true

namespace Lake.EStateT

instance : MonadLift (Lake.EStateT ε σ Id) (EStateM ε σ) where
  monadLift := Lake.EStateT.toEStateM
instance : MonadLift (EStateM ε σ) (Lake.EStateT ε σ Id) where
  monadLift := .ofEStateM

/-- A universe-polymorphic version of `map`, just for `Id`. -/
def map' {α β : Type*} (f : α → β) (x : EStateT ε σ Id α) : EStateT ε σ Id β :=
  λs => return (x s).map f

instance : ULiftable (EStateT.{_,_,u} ε σ Id) (EStateT.{_,_,v} ε σ Id) where
  congr {α β} e := {
    toFun f := f.map' e
    invFun f := f.map' e.symm
    left_inv f := sorry
    right_inv f := sorry
  }

instance : ULiftable (EStateM ε σ) (EStateT.{_,_,v} ε σ Id) where
  congr {α β} e := {
    toFun f := (ofEStateM f).map' e
    invFun f := (f.map' e.symm).toEStateM
    left_inv := sorry
    right_inv := sorry
  }

end Lake.EStateT

abbrev EIO' (ε : Type u) (α : Type v) : Type _ := Lake.EStateT ε IO.RealWorld Id α
abbrev IO' : Type u → Type u := EIO' IO.Error
abbrev BaseIO' := EIO' Empty

instance : MonadLift (EIO' ε) (EIO ε) where
  monadLift := Lake.EStateT.toEStateM

instance : MonadLift (EIO ε) (EIO' ε) where
  monadLift := Lake.EStateT.ofEStateM

instance : MonadLift IO' IO := by unfold IO EIO; infer_instance
instance : MonadLift IO IO':= by unfold IO EIO; infer_instance
instance : MonadLift BaseIO BaseIO':= by unfold BaseIO EIO; infer_instance

instance : ULiftable BaseIO BaseIO' := inferInstance
