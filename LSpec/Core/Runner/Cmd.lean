-- import Lake.CLI
import Cli

import LSpec.Core.Config.Definition

open Cli

namespace LSpec.Core.Runner

set_option diagnostics true

-- def mkLoadConfig (lakeOptions : LakeOptions) : IO LoadConfig := do
--   match <- lakeOptions.mkLoadConfig.toBaseIO with
--   | .ok config =>
--     IO.println "successfully mkLoadConfig"
--     pure config
--   | .error error =>
--     IO.eprintln s!"{error}"
--     throw $ IO.userError s!"{error}"
-- 
-- def loadPackage (name : Lean.Name): CliM Package := do
--   processOptions lakeOption
--   let lakeOptions : LakeOptions <- getThe LakeOptions
--   IO.println "lakeOptions: {lakeOptions}"
--   let loadConfig <- mkLoadConfig lakeOptions
--   IO.println "loadConfig: {loadConfig}"
--   let workspace <- loadWorkspaceRoot loadConfig
--   IO.println s!"packages: {workspace.packages.map (·.name)}"
--   return workspace.packages.find? (·.name = name) |>.get!
-- 
-- def CliM.run' (self : CliM a) (args : List String := []) : IO a := do
--   let (elanInstall?, leanInstall?, lakeInstall?) ← findInstall?
--   let main := self.run' args |>.run' {args, elanInstall?, leanInstall?, lakeInstall?}
--   match <- main.run.toBaseIO with
--   | .ok except => do
--     match except with
--     | .ok a => return a
--     | .error error => panic s!"{error}"
--   | .error error => panic s!"{error}"
-- 
-- def package : IO Package := CliM.run' $ loadPackage `LSpec

instance : ParseableType FailOn where
  name := "FailOn"
  parse?
  | "empty" => .some .empty
  | "focused" => .some .focused
  | "pending" => .some .pending
  | "empty-description" => .some .emptyDescription
  | _ => .none

def version : String := "2.1.0"
def description : String := ""

def cmd : Cmd := `[Cli|
  "lspec" NOOP; [version]
  description

  FLAGS:
    "ignore-dot-lspec" | IGNORE_DOT_LSPEC;  "do not read options from ~/.lspec and .lspec"
    m, "match";                             "only run examples that match given PATTERN"
    skip;                                   "skip examples that match given PATTERN"
    "dry-run";                              "pretend that everything passed; don't verify anything"
    "focused-only";                         "do not run anything, unless there are focused spec items"
    "fail-on" : Array FailOn;               "empty: fail if all spec items have been filtered" ++
                                            "focused: fail on focused spec items" ++
                                            "pending: fail on pending spec items" ++
                                            "empty-description: fail on empty descriptions"
    strict;                                 "same as --fail-on=focused,pending"
    "fail-fast";                            "abort on first failure"
    "randomize";                            "randomize execution order"
    r, rerun;                               "rerun all examples that failed in the " ++
                                            "previous test run (only works in" ++
                                            "combination with --failure-report)"
    "failure-report" : System.FilePath;     "read/write a failure report for use with --rerun"
    "rerun-all-on-success";                 "run the whole test suite after a previously " ++
                                            "failing rerun succeeds for the first time " ++
                                            "(only works in combination with --rerun)"
    j, jobs : Nat;                          "run at most N parallelizable tests simultaneously (default: number of available processors)"
    seed : Seed;                            "used seed for --randomize and QuickCheck properties"
    f, format : String;                     "use a custom formatter; this can be one of checks, specdoc, progress, failed-examples or silent"
  
  EXTENSIONS:
    envVars
]

