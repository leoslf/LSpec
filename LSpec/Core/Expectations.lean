-- import LUnit

import LSpec.Prelude
import LSpec.Core.Example.Result
import LSpec.Core.Expectations.Monad
import LSpec.Core.Example.Definition

namespace LSpec.Core

-- FIXME:
def location? (offset : Nat := 0) : Option Location := .none

def expectationFailure (message : String) (offset : Nat := 0) : Expectation := do
  throw $ Failure.mk .none (location? (offset := offset)) $ .Reason message
  -- LUnit.assertFailure message

def expectTrue (message : String) (condition : Bool) (offset : Nat := 0) : Expectation := do
  unless condition do
    expectationFailure message (offset := offset + 1)

def shouldBe [Repr a] [BEq a] (actual : a) (expected : a) : Expectation := do
  unless actual == expected do
    throw $ Failure.mk .none .none $ .ExpectedButGot .none (reprStr expected) (reprStr actual)
  -- LUnit.assertEqual "" expected actual

def shouldSatisfy [Repr a] (value : a) (predicate : a -> Bool) : Expectation :=
  expectTrue ("predicate failed on: " ++ reprStr value) $ predicate value

def shouldThrow [TypeName ε] [ToString ε] [Monad m] [MonadExcept ε m] [MonadLift m ExpectationM] (action : m a) (predicate : ε -> Bool) : Expectation := do
  match <- liftM $ toExcept action with
  | .ok _ =>
    expectationFailure s!"did not get expected exception: {exceptionType}"
  | .error e =>
    expectTrue s!"predicate failed on expected exception: {exceptionType}\n{e}" $ predicate e
 where
  toExcept (action : m a) : m (Except ε a) := do
    try
      Except.ok <$> action
    catch
    | e =>
      pure $ Except.error e

  exceptionType : String :=
    reprStr $ TypeName.typeName ε

infixl:1 " <shouldBe> " => shouldBe
infixl:1 " <shouldSatisfy> " => shouldSatisfy
infixl:1 " <shouldThrow> " => shouldThrow

-- infix:1 `shouldStartWith`
-- infix:1 `shouldEndWith`
-- infix:1 `shouldContain`
-- infix:1 `shouldMatchList`
-- infix:1 `shouldReturn`
-- infix:1 `shouldNotBe`
-- infix:1 `shouldNotSatisfy`
-- infix:1 `shouldNotContain`
-- infix:1 `shouldNotReturn`

