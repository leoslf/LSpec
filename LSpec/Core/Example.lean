import LSpec.Core.Location
import LSpec.Core.Expectations
import LSpec.SlimCheck.Utils

namespace LSpec.Core

open LSpec.SlimCheck.Utils

namespace Example

structure Params where
  mk ::
  -- quickCheckArgs : QC.Args
  -- smallCheckDepth : Option Int
deriving Repr, BEq

instance : Inhabited Params where
  default := {
    -- quickCheckArgs := default,
    -- smallCheckDepth := .none
  }

abbrev Progress := Nat × Nat
abbrev ProgressCallback := Progress -> IO Unit

abbrev ActionWith a := a -> IO Unit

structure Exception where
  mk ::
  message : String
deriving Repr, BEq

namespace Exception

def of (e : IO.Error) : Exception :=
  .mk $ e.toString

end Exception

namespace Result

inductive FailureReason where
| NoReason : FailureReason
| Reason (reason : String) : FailureReason
| ColorizedReason (reason : String) : FailureReason
| ExpectedButGot (preface : Option String) (expected : String) (actual : String) : FailureReason
| Error (info : Option String) (exception : Exception) : FailureReason
deriving Repr, BEq

inductive Status where
| Success : Status
| Pending (location : Option Location) (reason : Option String) : Status
| Failure (location : Option Location) (reason : FailureReason) : Status
deriving Repr, BEq

def Status.isSuccess : Status -> Bool
| .Success => true
| _ => false

def Status.isPending : Status -> Bool
| .Pending _ _  => true
| _ => false

def Status.isFailure : Status -> Bool
| .Failure _ _  => true
| _ => false

end Result

structure Result where
  mk ::
  info : String
  status : Result.Status
deriving Repr, BEq

end Example

open Example

class Example e where
  Arg : Type
  evaluate (example_ : e) (params : Params) (hook : ActionWith Arg -> IO Unit) (callback : ProgressCallback) : IO Result

instance : Example (a -> Result) where
  Arg := a
  evaluate example_ _ hook _ := do
    liftHook (Result.mk "" .Success) hook (pure ∘ example_)

instance : Example Result where
  Arg := Unit
  evaluate e := evaluate $ fun () => e

instance : Example (a -> Bool) where
  Arg := a
  evaluate predicate _ hook _ := do
    let example_ (arg : a) : Result :=
      Result.mk "" $
        if predicate arg then
          .Success
        else
          .Failure .none .NoReason
    liftHook (Result.mk "" .Success) hook (pure ∘ example_)

instance : Example Bool where
  Arg := Unit
  evaluate e := Example.evaluate $ fun () => e

instance : Example (a -> Expectation) where
  Arg := a
  evaluate e _ hook _ := do
    let () <- hook e
    pure $ Result.mk "" .Success

instance : Example Expectation where
  Arg := Unit
  evaluate e := Example.evaluate $ fun () => e

def safeEvaluate (action : IO Result) : IO Result := action


def safeEvaluateExample [Example e] (example_ : e) (params : Params) (around : ActionWith (Arg e) -> IO Unit) : ProgressCallback -> IO Result :=
  safeEvaluate ∘ Example.evaluate example_ params around
