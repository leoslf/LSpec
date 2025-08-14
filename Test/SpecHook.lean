import LSpec

namespace Test.SpecHook

open LSpec.Core

def hook : Spec -> Spec := parallel
