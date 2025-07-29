import LSpec.Core.Format

namespace LSpec.Core.Runner

namespace SpecResult

namespace Item

inductive Status where
| Success : Status
| Pending : Status
| Failure : Status
deriving Repr, DecidableEq

def Status.isFailure : Status -> Bool
| .Failure => true
| _ => false

end Item

structure Item where
  path : Path
  status : Item.Status
deriving Repr, DecidableEq

def Item.isFailure (item : Item) : Bool :=
  item.status.isFailure

end SpecResult

structure SpecResult where
  items : List SpecResult.Item
  success : Bool
deriving Repr, DecidableEq, Inhabited

structure Summary where
  examples : Nat
  failures : Nat
deriving Repr, DecidableEq

instance : Inhabited Summary where
  default := {
    examples := 0,
    failures := 0,
  }

def Summary.isSuccess (summary : Summary) : Bool :=
  summary.failures == 0

def SpecResult.toSummary (result : SpecResult) : Summary :=
  {
    examples := result.items.length,
    failures := result.items.filter (·.isFailure) |>.length,
  }

