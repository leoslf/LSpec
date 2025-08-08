import LSpec.Exception
import LSpec.Core.Location
import LSpec.Core.Expectations
import LSpec.SlimCheck.Utils

namespace LSpec.Core

open LSpec.SlimCheck.Utils

namespace Example

def safeTry (action : IO a) : BaseIO (Except IO.Error a) := do
  IO.wait =<< IO.asTask action

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

namespace Result

inductive FailureReason where
| NoReason : FailureReason
| Reason (reason : String) : FailureReason
| ColorizedReason (reason : String) : FailureReason
| ExpectedButGot (preface : Option String) (expected : String) (actual : String) : FailureReason
| Error (info? : Option String) (exception : Exception) : FailureReason
deriving Repr, BEq

inductive Status where
| Success : Status
| Pending (location? : Option Location) (reason? : Option String) : Status
| Failure (location? : Option Location) (reason : FailureReason) : Status
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

-- NOTE: lean is a strict language
def Status.force : Status -> Status := id

mutual
  partial def Status.of : IO.Error -> IO Status :=
    safeEvaluate ∘ pure ∘ toResultStatus
   where
    toResultStatus : IO.Error -> Result.Status
    -- FIXME
    | e => .Failure .none $ .Error .none $ Exception.of e

  partial def Status.safeEvaluate (action : IO Status) : IO Status := do
    match <- safeTry $ Result.Status.force <$> action with
    | .error e => of e
    | .ok status => return status
end

end Result

structure Result where
  mk ::
  info : String
  status : Result.Status
deriving Repr, BEq

-- NOTE: lean is a strict language
def Result.force : Result -> Result := id

def Result.safeEvaluate (action : IO Result) : IO Result := do
  match <- safeTry $ Result.force <$> action with
  | .error e => Result.mk "" <$> Result.Status.of e
  | .ok result => return result

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
  evaluate e := evaluate λ() => e

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
  evaluate e := Example.evaluate λ() => e

instance : Example (a -> Expectation) where
  Arg := a
  evaluate e _ hook _ := do
    let () <- hook e
    pure $ Result.mk "" .Success

instance : Example Expectation where
  Arg := Unit
  evaluate e := Example.evaluate λ() => e

def Example.safeEvaluate [Example e]
  (example_ : e)
  (params : Params)
  (around : ActionWith (Arg e) -> IO Unit) :
  ProgressCallback -> IO Result :=
  Result.safeEvaluate ∘ Example.evaluate example_ params around
