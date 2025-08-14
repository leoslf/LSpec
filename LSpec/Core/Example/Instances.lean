import LSpec.SlimCheck.Utils
import LSpec.Core.Example.Definition
import LSpec.Core.Args

namespace LSpec.Core

open LSpec.SlimCheck.Utils

universe u
variable {a : Type}

instance : Example (a -> Example.Result) where
  Arg := a
  evaluate example_ _ hook _ := do
    liftHook (Example.Result.mk "" .Success) hook (pure ∘ example_)

instance : Example Example.Result where
  Arg := Unit
  evaluate e := Example.evaluate λ() => e

instance : Example (a -> Bool) where
  Arg := a
  evaluate predicate _ hook _ := do
    let example_ (arg : a) : Example.Result :=
      Example.Result.mk "" $
        if predicate arg then
          .Success
        else
          .Failure .none .NoReason
    liftHook (Example.Result.mk "" .Success) hook (pure ∘ example_)

instance : Example Bool where
  Arg := Unit
  evaluate e := Example.evaluate λ() => e

instance (priority := high) : Example (a -> ExpectationM Unit) where
  Arg := a
  evaluate e _ hook _ := do
    let () <- hook e
    pure $ Example.Result.mk "" .Success


@[default_instance]
instance (priority := high) : Example (ExpectationM Unit) where
  Arg := Unit
  evaluate e := Example.evaluate λ() => e

instance (priority := low) : Example (a -> IO Unit) where
  Arg := a
  evaluate e := Example.evaluate (e := a -> ExpectationM Unit) $ liftM ∘ e

instance (priority := low) : Example (IO Unit) where
  Arg := Unit
  evaluate e := Example.evaluate $ liftM (n := ExpectationM) e

instance (priority := low) : Example (a -> ArgsT IO Unit) where
  Arg := a
  evaluate e := Example.evaluate (e := a -> IO Unit) λarg => do
    ArgsT.run (args := []) $ e arg

instance (priority := low) : Example (ArgsT IO Unit) where
  Arg := Unit
  evaluate e := Example.evaluate (e := Unit -> ArgsT IO Unit) λ() => e

