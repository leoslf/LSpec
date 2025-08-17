-- import LUnit

import Batteries.Data.String.Matcher
import Batteries.Data.List.Matcher

import LSpec.Prelude
import LSpec.Core.Example.Result
import LSpec.Core.Example.Definition
import LSpec.Core.Expectations.Monad
import LSpec.Core.Expectations.Matcher

namespace LSpec.Core

set_option linter.unusedVariables false
-- FIXME:
def location? (offset : Nat := 0) : Option Location := .none

set_option linter.unusedVariables true

def expectationFailure (message : String) (offset : Nat := 0) : Expectation := do
  throw $ Failure.mk .none (location? (offset := offset)) $ .Reason message
  -- LUnit.assertFailure message

def expectTrue (message : String) (condition : Bool) (offset : Nat := 0) : Expectation := do
  unless condition do
    expectationFailure message (offset := offset + 1)

variable {a} [Repr a]

def shouldBe [BEq a] (actual : a) (expected : a) : Expectation := do
  unless actual == expected do
    throw $ Failure.mk .none .none $ .ExpectedButGot .none (reprStr expected) (reprStr actual)
  -- LUnit.assertEqual "" expected actual

infixl:1 " <shouldBe> " => shouldBe

def shouldSatisfy (value : a) (predicate : a -> Bool) : Expectation :=
  expectTrue ("predicate failed on: " ++ reprStr value) $ predicate value

infixl:1 " <shouldSatisfy> " => shouldSatisfy

def compareWith (comparator : a -> a -> Bool) (errorDescription : String) (result : a) (expected : a) : Expectation :=
  expectTrue s!"{reprStr result} {errorDescription} {reprStr expected}" $ comparator expected result

class Sequence (a) where
  isPrefixOf : a -> a -> Bool
  isInfixOf : a -> a -> Bool
  isSuffixOf : a -> a -> Bool

instance [BEq a] : Sequence (List a) where
  isPrefixOf := List.isPrefixOf
  isInfixOf p xs := List.Matcher.ofList xs |>.find? p |>.isSome
  isSuffixOf := List.isSuffixOf

instance : Sequence String where
  isPrefixOf := String.isPrefixOf
  isInfixOf p xs := String.Matcher.ofString xs |>.find? p |>.isSome
  isSuffixOf := flip String.endsWith

def shouldStartWith [Sequence a] : (actual : a) -> (prefix_ : a) -> Expectation :=
  compareWith Sequence.isPrefixOf "does not start with"

infixl:1 " <shouldStartWith> " => shouldStartWith

def shouldEndWith [Sequence a] : (actual : a) -> (suffix : a) -> Expectation :=
  compareWith Sequence.isSuffixOf "does not end with"

infixl:1 " <shouldEndWith> " => shouldEndWith

def shouldContain [Sequence a] : (actual : a) -> (infix_ : a) -> Expectation :=
  compareWith Sequence.isInfixOf "does not contain"

infixl:1 " <shouldContain> " => shouldContain

def shouldMatchList [BEq a] (xs : List a) (ys : List a) : Expectation := do
  matchList xs ys |>.elim pass' expectationFailure

infixl:1 " <shouldMatchList> " => shouldMatchList

def shouldReturn [BEq a] (action : IO a) (expected : a) : Expectation := do
  let actual <- action
  actual <shouldBe> expected

infixl:1 " <shouldReturn> " => shouldReturn

def shouldNotBe [BEq a] (actual : a) (notExpected : a) : Expectation :=
  expectTrue ("not expected: " ++ reprStr actual) (actual != notExpected)

infixl:1 " <shouldNotBe> " => shouldNotBe

def shouldNotSatisfy [BEq a] (actual : a) (predicate : a -> Bool) : Expectation :=
  expectTrue ("predicate succeeded on: " ++ reprStr actual) ((not ∘ predicate) actual)

infixl:1 " <shouldNotSatisfy> " => shouldNotSatisfy

def shouldNotContain [Sequence a] : (actual : a) -> (infix_ : a) -> Expectation :=
  compareWith ((not ∘ ·) ∘ Sequence.isInfixOf) "does contain"

infixl:1 " <shouldNotContain> " => shouldNotContain

def shouldNotReturn [BEq a] (action : IO a) (notExpected : a) : Expectation := do
  let actual <- action
  shouldNotBe actual notExpected

infixl:1 " <shouldNotReturn> " => shouldNotReturn

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

infixl:1 " <shouldThrow> " => shouldThrow

def shouldBeOkWith [TypeName ε] [ToString ε] [BEq a] (except : Except ε a) (expected : a) : Expectation := do
  match except with
  | .error error => expectationFailure s!"unexpected .error {reprStr $ TypeName.typeName ε}"
  | .ok actual =>
    actual <shouldBe> expected

infixl:1 " <shouldBeOkWith> " => shouldBeOkWith

