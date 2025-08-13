import LSpec

namespace SpecHook

open LSpec.Core

def hook : Spec -> Spec := parallel
