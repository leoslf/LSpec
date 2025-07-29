import LSpec

open LSpec.Core

-- #lspec discover
def spec : Spec := do
  describe "Hello World" $ do
    it "should show hello world" $ do
      IO.println "Hello World"
  
def main := IO.useArgs $ lspec spec
