import Cli

namespace LSpec.Discover

open Cli

structure DiscoverConfig where
  nested : Bool
  formatter : Option String
  noMain : Bool
  module : Option ModuleName
deriving Repr, BEq

instance : Inhabited DiscoverConfig where
  default :=  {
    nested := false,
    formatter := .none,
    noMain := false,
    module := .none,
  }

-- def options :
