import Cli

import LSpec.Discover.Config
import LSpec.Discover.Run

namespace LSpec.Discover

open Cli

def parseConfig (parsed : Parsed) : IO DiscoverConfig := do
  let mut config : DiscoverConfig := default

  if parsed.hasFlag "module" then
    let module := parsed.flag! "module" |>.as! ModuleName
    config := { config with module }

  pure config

def runDiscoverCmd (p : Parsed) : IO UInt32 := do
  let source := p.positionalArg! "source" |>.as! System.FilePath
  let destination? := (·.as! System.FilePath) <$> p.flag? "destination"
  -- let current : String := p.positionalArg! "current" |>.as! String
  let config <- parseConfig p
  let specs <- findSpecs source
  let write : String -> IO Unit :=
    destination?.elim IO.println IO.FS.writeFile
  write $ mkSpecModule source config specs
  return 0

def discoverCmd : Cmd := `[Cli|
  "lspec-discover" VIA runDiscoverCmd; ["0.0.1"]
  "(description)"

  FLAGS:
    module : ModuleName; "Module name"
    -- "output" : String;
    destination | LSPEC_DISCOVER_DESTINATION : String;   "Destination path"

  ARGS:
    source : String;        "Source"
    -- current : String;       "Current"

  EXTENSIONS:
    author "leoslf";
    defaultValues! #[
      ("source", "Test"),
    ];
    envVars
]
