import LSpec

open LSpec.Core

namespace Test.LSpec.Core.ExpectationsSpec

def expectationFailed (reason : Example.Result.FailureReason) (failure : Failure) : Bool :=
  dbgTraceVal failure.reason == dbgTraceVal reason && (setColumn <$> failure.location?) == (setColumn <$> location?)
 where
  location? : Option Location := .none
    -- match List.reverse callStack with
    -- | [] => .none
    -- | (_, location) :: _ => .some location

  setColumn (location : Location) :=
    { location with column := 0 }

def spec : Spec := do
  describe "shouldBe" $ do
    it "succeeds if arguments are equal" $ do
      "foo" <shouldBe> "foo"

    it "fails if arguments are not equal" $ do
      ("foo" <shouldBe> "bar") <shouldThrow> expectationFailed (.ExpectedButGot .none "\"bar\"" "\"foo\"")

  describe "shouldSatisfy" $ do
    it "succeeds if value satisfies predicate" $ do
      "" <shouldSatisfy> String.isEmpty

    it "fails if value does not satisfies predicate" $ do
      ("foo" <shouldSatisfy> String.isEmpty) <shouldThrow> expectationFailed (.Reason "predicate failed on: \"foo\"")
