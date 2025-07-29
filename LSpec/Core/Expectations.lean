abbrev Assertion := IO Unit
abbrev Expectation := Assertion

def shouldBe [Repr a] [BEq a] (actual : a) (expected : a) : Expectation :=
  sorry

notation:1 actual:10  " shouldBe " expected:11 => shouldBe actual expected

#check λ x y => x shouldBe y

-- infix:1 `shouldSatisfy`
-- infix:1 `shouldStartWith`
-- infix:1 `shouldEndWith`
-- infix:1 `shouldContain`
-- infix:1 `shouldMatchList`
-- infix:1 `shouldReturn`
-- infix:1 `shouldThrow`
-- infix:1 `shouldNotBe`
-- infix:1 `shouldNotSatisfy`
-- infix:1 `shouldNotContain`
-- infix:1 `shouldNotReturn`

