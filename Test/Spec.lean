import LSpec
-- import Test.LSpec.Core.ExpectationsSpec

open LSpec.Core

-- #check shouldBe 1 2

#lspec discover
-- def spec : Spec := do
--   describe "Hello World" $ do
--     it "should show hello world" $ do
--       IO.println "Hello World"
--       (1 : Nat) <shouldBe> (1 : Nat)
--
--   describe "Hello World 2" $ do
--     it "should show hello world" $ do
--       IO.println "Hello World"
--
--   describe "LSpec.Core.Expectations" $ do
--     Test.LSpec.Core.ExpectationsSpec.spec
--
--
-- -- def main := ArgsT.run $ lspec spec
