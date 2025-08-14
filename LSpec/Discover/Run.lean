import Cli

import LSpec.Prelude
import LSpec.Discover.Config
import LSpec.Discover.Sort
import LSpec.Discover.Parser

namespace LSpec.Discover

open Cli
open Lean

-- set_option linter.dupNamespace false

inductive Spec where
| Spec (name : String) : LSpec.Discover.Spec
| Module (module : String) (specs : List LSpec.Discover.Spec) : LSpec.Discover.Spec
| Hook (module : String) (specs : List LSpec.Discover.Spec) : LSpec.Discover.Spec
deriving Repr, BEq

inductive Hook where
| WithHook : Hook
| WithoutHook : Hook
deriving Repr, DecidableEq

mutual
  inductive Tree : Type where
  | Leaf (name : String) (specs : List String) : Tree
  | Node (name : String) (forest : Forest) : Tree
  deriving Repr, BEq

  structure Forest where
    mk ::
    hook : Hook
    trees : List Tree
  deriving Repr, BEq
end

def isValidModuleChar (c : Char) : Bool :=
  c.isAlphanum || c == '_' || c == '\''

def isValidModuleName (name : String) : Bool :=
  match name.toList with
  | [] => false
  | c :: cs => c.isUpper && cs.all isValidModuleChar

def Tree.sortKey : Tree -> String × Nat
| .Leaf name _ => (name, 0)
| .Node name _ => (name, 1)

def Forest.ensure (hook : Hook) : List Tree -> Option Forest :=
  Functor.map (Forest.mk hook) ∘ guarded (not ·.isEmpty)

def isFile (path : System.FilePath) : BaseIO Bool := do
  return (<- path.metadata.toBaseIO).elim (Function.const _ false) (·.type == IO.FS.FileType.file)

def toSpec [ToString System.FilePath] (file : System.FilePath) : IO (Option Tree) := do
  unless <- isFile file do
    dbgTraceM s!"toSpec: file: {file} is not a file"
    return .none

  let fileStem := file.fileStem.get!
  unless fileStem.endsWith "Spec" && fileStem != "Spec" && file.extension == "lean" do
    dbgTraceM s!"skipping {file}"
    return .none

  try
    dbgTraceM s!"reading {file}"
    let content <- IO.FS.readFile file
    let declarations := (<- extractDeclarations content s!"{file}")
      |>.filter ("spec".toName.isSuffixOf)
      |>.map toString
    dbgTraceM s!"file: {file}, declarations: {declarations}"
    return .some $ .Leaf fileStem declarations
  catch
  | e =>
    dbgTraceM s!"{e}"
    return .none

def Hook.mk (directory : System.FilePath) (files : Array IO.FS.DirEntry) : BaseIO Hook := do
  let hook := directory / "SpecHook.lean"
  let paths := files.map (·.path)
  dbgTraceM s!"checking if hook {hook} exists in directory: {directory}"
  if paths.contains hook && (<- isFile hook) then
    dbgTraceM s!"hook: {hook} found"
    return .WithHook
  dbgTraceM s!"hook: {hook} not found"
  return .WithoutHook

partial def specForest (directory : System.FilePath) : IO (Option Forest) := do
  unless <- directory.isDir do
    IO.eprintln s!"directory: {directory} not found"
    return .none

  let files <- directory.readDir
  let hook <- Hook.mk directory files
  let forests <- files.filterMapM toSpecTree
  return Forest.ensure hook $ Array.toList $ forests.qsort <| (Ordering.isLT ∘ ·) ∘ compareNaturallyBy Tree.sortKey
 where
  toSpecTree (file : IO.FS.DirEntry) : IO (Option Tree) := do
    assert! <- file.path.pathExists

    if isValidModuleName file.fileName then
      if <- file.path.isDir then
        (Tree.Node file.fileName <$> ·) <$> specForest file.path
      else
        return .none
    else
      if <- isFile file.path then
        toSpec file.path
      else
        return .none

def dirname (path : System.FilePath) : IO System.FilePath := do
  if <- path.isDir then
    return path
  return path.parent.getD path

def discover (directory : System.FilePath) : IO (Option Forest) := do
  dbgTraceM s!"directory: {directory}"

  let filterSrc : Forest -> IO (Option Forest)
  | { hook, trees } => do
    let tree? <- toSpec directory
    dbgTraceM s!"tree?: {reprStr tree?}"
    let trees' := (tree?.elim id (List.filter ∘ (λa b => a != b))) trees
    return Forest.ensure hook trees'

  let forest? <- specForest directory
  match forest? with
  | .none => return .none
  | .some forest => filterSrc forest

def mkModule (components : List String) : String :=
  ".".intercalate components

mutual
  def toSpecs.fromForest (names : List String) : Forest -> List Spec
  | { hook := .WithHook, trees } =>
    let module := mkModule $ names.concat "SpecHook"
    [.Hook module $ trees.flatMap (toSpecs.fromTree names)]
  | { hook := .WithoutHook, trees } => trees.flatMap (fromTree names)

  def toSpecs.fromTree (names : List String) : Tree -> List Spec
  | .Leaf name specs => [.Module (mkModule (names.concat name)) $ specs.map .Spec]
  | .Node name forest => toSpecs.fromForest (names.concat name) forest
end

def toSpecs (prefix_ : List String) : Forest -> List Spec :=
  toSpecs.fromForest prefix_

def findSpecs (path : System.FilePath) : IO (List Spec) := do
  let directory <- dirname path
  let forest? <- discover directory
  return forest?.elim [] (toSpecs directory.components)

def linesep :=
  if System.Platform.isWindows then
    "\r\n"
  else
    "\n"

mutual
  partial def moduleNames.fromForest : List Spec -> List String :=
    List.flatMap moduleNames.fromTree

  partial def moduleNames.fromTree : Spec -> List String
  | .Spec _ => []
  | .Module name _ => [name]
  | .Hook name forest => moduleNames.fromForest forest |>.concat name
end

def moduleNames : List Spec -> List String :=
  moduleNames.fromForest

def importList (specs? : Option (List Spec)) : String :=
  linesep.intercalate $ specs?.elim [] moduleNames |>.map (s!"import {·}")

mutual
  partial def formatSpecs.fromForest (specs : List Spec) : String := Id.run do
    if specs.isEmpty then
      return "do pure ()"

    return " *> ".intercalate $ specs.map formatSpecs.fromTree

  partial def formatSpecs.fromTree : Spec -> String
  | .Spec name => s!"describe \"{name}\" {name}"
  | .Module name specs => s!"(describe \"{name}\" $ {formatSpecs.fromForest specs})"
  | .Hook name forest => s!"({name}.hook $ {formatSpecs.fromForest forest})"
end

def formatSpecs (specs : List Spec) : String :=
  formatSpecs.fromForest specs

def moduleName (source : System.FilePath) (config : DiscoverConfig) : Cli.ModuleName :=
  config.module.getD $
    if config.noMain then
      match source.fileName with
      | .none => panic s!"cannot read basename from source: {source}"
      | .some basename => .mkSimple basename
    else
      .mkSimple "Main"

def driverWithFormatter (formatter : String) : String :=
  "" -- FIXME -- sorry

def mkSpecModule (config : DiscoverConfig) (nodes : List Spec) : String :=
  linesep.intercalate $ [
    "import LSpec",
    importList nodes,
    "open LSpec.Core",
    -- NOTE: main has to be in the root namespace
    -- s!"namespace {moduleName source config}",
    "def spec : Spec := " ++ formatSpecs nodes,
    config.formatter.elim driver driverWithFormatter
  ]
 where
  driver :=
    match config.noMain with
    | false =>
      "def main : (args : List String) -> IO Unit := ArgsT.run $ lspec spec"
      -- "def main : IO Unit := do\n  IO.println \"Hello World\""
    | true => ""

