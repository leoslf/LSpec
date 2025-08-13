import LSpec
import Test.SpecHook
import Test.LSpec.Core.ExpectationsSpec
open LSpec.Core
def spec : Spec := (SpecHook.hook $ describe "LSpec.Core.Expectations" Test.LSpec.Core.ExpectationsSpec.spec)
def main : (args : List String) -> IO Unit := ArgsT.run $ lspec spec
