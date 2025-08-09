import LSpec.Prelude
import LSpec.Core.Example.Result

namespace LSpec.Core

structure Failure where
  mk ::
  info? : Option String
  location? : Option Location
  reason : Example.Result.FailureReason
deriving Repr, BEq, TypeName, Inhabited

def Failure.toStatus (self : Failure) : Example.Result.Status :=
  .Failure self.location? self.reason

instance : ToString Failure where
  toString self := toString self.reason

abbrev ExpectationM := EIO Failure
notation:max "Expectation" => (ExpectationM Unit)

-- instance : MonadLift LUnit.AssertionM ExpectationM where
--   monadLift :=
--     EStateM.adaptExcept λ
--       | { location?, reason } => .Failure location? $ Example.Result.FailureReason.of reason

instance : MonadLift IO ExpectationM where
  monadLift action := do
    match <- action.toBaseIO with
    | .ok result => pure result
    | .error error =>
      -- FIXME: callstack
      throw $ Failure.mk .none .none $ .Error .none $ Exception.of error

