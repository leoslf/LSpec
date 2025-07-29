import Cli

import LSpec.Discover.Cmd

open LSpec.Discover

def main (args : List String) : IO UInt32 :=
  discoverCmd.validate args
