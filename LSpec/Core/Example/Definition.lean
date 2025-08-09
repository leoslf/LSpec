import LSpec.Core.Example.Result
import LSpec.Core.Expectations.Monad

namespace LSpec.Core

namespace Example

structure Params where
  mk ::
  -- quickCheckArgs : QC.Args
  -- smallCheckDepth : Option Int
deriving Repr, BEq, Inhabited, TypeName

instance : Inhabited Params where
  default := {
    -- quickCheckArgs := default,
    -- smallCheckDepth := .none
  }

abbrev Progress := Nat × Nat
abbrev ProgressCallback := Progress -> BaseIO Unit

abbrev ActionWith a := a -> ExpectationM Unit
abbrev Hook a := ActionWith a -> ExpectationM Unit

end Example

open Example

class Example e where
  Arg : Type
  evaluate (example_ : e) (params : Params) (hook : Hook Arg) (progress : ProgressCallback) : ExpectationM Result

namespace Example.Result

def safeTry (action : EIO ε a) : EIO ε a := do
  let task : Task (Except ε a) <- EIO.asTask action
  let except : Except ε a <- IO.wait task
  EIO.ofExcept except

-- NOTE: lean is a strict language
def Status.force : Status -> Status := id

partial def Status.safeEvaluate (action : ExpectationM Status) : BaseIO Status := do
  EIO.catchExceptions (safeTry $ Result.Status.force <$> action) (pure ∘ Failure.toStatus)

partial def Status.of : IO.Error -> BaseIO Result.Status :=
  Status.safeEvaluate ∘ pure ∘ toResultStatus
 where
  toResultStatus : IO.Error -> Status
  -- FIXME
  | e => .Failure .none $ .Error .none $ Exception.of e

-- NOTE: lean is a strict language
def force : Result -> Result := id

def safeEvaluate (action : ExpectationM Result) : BaseIO Result := do
  match <- EIO.toBaseIO $ safeTry $ Result.force <$> action with
  | .ok result => pure result
  | .error failure => pure $ Result.mk "" failure.toStatus

end Example.Result

def Example.safeEvaluate [Example e]
  (example_ : e)
  (params : Params)
  (hook : Hook (Arg e)) :
  ProgressCallback -> BaseIO Result :=
  Result.safeEvaluate ∘ Example.evaluate example_ params hook
