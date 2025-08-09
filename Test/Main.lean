import LSpec
import Test.LSpec.Core.ExpectationsSpec
open LSpec.Core
def spec : Spec := describe "LSpec.Core.Expectations" Test.LSpec.Core.ExpectationsSpec.spec
def main := ArgsT.run $ lspec spec