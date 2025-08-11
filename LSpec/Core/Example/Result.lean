-- import LUnit

import LSpec.Prelude

namespace LSpec.Core.Example

namespace Result

inductive FailureReason where
| NoReason : FailureReason
| Reason (reason : String) : FailureReason
| ColorizedReason (reason : String) : FailureReason
| ExpectedButGot (preface? : Option String) (expected : String) (actual : String) : FailureReason
| Error (info? : Option String) (exception : Exception) : FailureReason
| Canceled : FailureReason
deriving Repr, BEq, Nonempty, Inhabited, TypeName

instance : ToString FailureReason where
  toString := reprStr

-- def FailureReason.of : LUnit.Failure.Reason -> FailureReason
-- | .Reason reason => .Reason reason
-- | .ExpectedButGot preface? expected actual => .ExpectedButGot preface? expected actual
-- | .Error info? exception => .Error info? exception
-- | .Canceled => .Canceled

inductive Status where
| Success : Status
| Pending (location? : Option Location) (reason? : Option String) : Status
| Failure (location? : Option Location) (reason : FailureReason) : Status
deriving Repr, BEq, Nonempty, Inhabited, TypeName

def Status.isSuccess : Status -> Bool
| .Success => true
| _ => false

def Status.isPending : Status -> Bool
| .Pending _ _  => true
| _ => false

def Status.isFailure : Status -> Bool
| .Failure _ _  => true
| _ => false

def Status.merge (left : Status) (right : Status) (location? : Option Location := .none): Status :=
  match left, right with
  | _, .Success => left
  | .Failure _ _, _ => left
  | .Pending _ _, .Pending _ _ => left
  | .Success, .Pending _ _ => right
  | _, .Failure location'? reason =>
    .Failure (location'? <|> location?) $
      match reason with
      | .Error info? exception => .Error (info? <|> hookFailed?) exception
      | _ => reason
 where
  hookFailed? : Option String :=
    match location? with
    | .none => .none
    | .some name => .some s!"in {name}-hook:"

end Result

structure Result where
  mk ::
  info : String
  status : Result.Status
deriving Repr, BEq, Nonempty, Inhabited, TypeName

