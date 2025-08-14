import LSpec
import Test.LSpec.Core.ExpectationsSpec
import Test.SpecHook
open LSpec.Core
def spec : Spec := (Test.SpecHook.hook $ (describe "Test.LSpec.Core.ExpectationsSpec" $ describe "Test.LSpec.Core.ExpectationsSpec.spec" Test.LSpec.Core.ExpectationsSpec.spec))
def main : (args : List String) -> IO Unit := ArgsT.run $ lspec spec