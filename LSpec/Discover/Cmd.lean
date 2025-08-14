import Lake
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
  let package <- IO.currentDir
  dbgTraceM s!"current: {package}"
  -- initSearchPath (<- findSysroot)
  let mut source <- IO.FS.realPath (p.flag! "source" |>.as! System.FilePath)
  dbgTraceM s!"package: {package}, source: {source}"
  source := Lake.relPathFrom package source

  let destination? := (·.as! System.FilePath) <$> p.flag? "destination"
  if let .some destination := destination? then
    IO.FS.createDirAll destination.parent.get!

  let config <- parseConfig p
  let write : String -> IO Unit :=
    destination?.elim IO.println IO.FS.writeFile
  write $ mkSpecModule config (<- findSpecs source)
  return 0

def discoverCmd : Cmd := `[Cli|
  "lspec-discover" VIA runDiscoverCmd; ["0.0.1"]
  "(description)"

  FLAGS:
    module : ModuleName; "Module name"
    -- "output" : String;
    source : System.FilePath;        "Source"
    destination | LSPEC_DISCOVER_DESTINATION : String;   "Destination path"

  ARGS:

  EXTENSIONS:
    author "leoslf";
    defaultValues! #[
      ("source", "Test/Spec.lean"),
    ];
    envVars
]
