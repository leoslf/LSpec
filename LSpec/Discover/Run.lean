import Cli

import LSpec.Prelude
import LSpec.Discover.Config
import LSpec.Discover.Sort

namespace LSpec.Discover

open Cli

-- set_option linter.dupNamespace false

inductive Spec where
| Spec (module : String) : LSpec.Discover.Spec
| Hook (module : String) (specs : List LSpec.Discover.Spec) : LSpec.Discover.Spec
deriving Repr, BEq

inductive Hook where
| WithHook : Hook
| WithoutHook : Hook
deriving Repr, DecidableEq

mutual
  inductive Tree : Type where
  | Leaf (name : String) : Tree
  | Node (name : String) (forest : Forest) : Tree
  deriving Repr, BEq

  structure Forest where
    mk ::
    hook : Hook
    trees : List Tree
  deriving Repr, BEq
end

def isValidModuleChar (c : Char) : Bool :=
  c.isAlphanum ∨ c == '_' ∨ c == '\''

def isValidModuleName (name : String) : Bool :=
  match name.toList with
  | [] => false
  | c :: cs => c.isUpper ∧ cs.all isValidModuleChar

def Tree.sortKey : Tree -> String × Nat
| .Leaf name => (name, 0)
| .Node name _ => (name, 1)

def Forest.ensure (hook : Hook) : List Tree -> Option Forest :=
  Functor.map (Forest.mk hook) ∘ guarded (¬List.isEmpty .)

def toSpec [ToString FilePath] (file : FilePath) : Option Tree :=
  .Leaf <$> (spec >>= guarded isValidModuleName)
where
  spec := Substring.toString <$> s!"{file}".dropSuffix? "Spec.lean"

def isFile (path : System.FilePath) : BaseIO Bool := do
  match (<- path.metadata.toBaseIO) with
  | .ok metadata => return metadata.type == IO.FS.FileType.file
  | .error _ => return false

def Hook.mk (files : Array IO.FS.DirEntry) : IO Hook :=
  match files.find? (·.fileName == "SpecHook.hs") with
  | .none => pure .WithoutHook
  | .some file => do
      if (<- isFile file.path |>.toIO) then
        pure .WithHook
      else
        pure .WithoutHook

partial def specForest (directory : System.FilePath) : IO (Option Forest) := do
  let files <- directory.readDir
  let hook <- Hook.mk files
  let forests <- Array.reduceOption <$> files.mapM toSpecTree
  pure $ Forest.ensure hook $ Array.toList $ forests.qsort ((Ordering.isLT ∘ ·) ∘ compareNaturallyBy Tree.sortKey)
 where
  toSpecTree (file : IO.FS.DirEntry) : IO (Option Tree) := do
    if isValidModuleName file.fileName then
      if (<- file.path.isDir.toIO) then
        (Tree.Node file.fileName <$> ·) <$> specForest file.path
      else
        pure .none
    else
      if (<- (isFile file.path).toIO) then
        pure $ toSpec file.fileName
      else
        pure .none

def discover (source : System.FilePath) : IO (Option Forest) :=
  let (directory, basename) := System.FilePath.splitFileName source
  let filterSrc : Forest -> Option Forest
  | { hook, trees } => Forest.ensure hook $ (toSpec basename |>.elim id (List.filter ∘ (λa b => a != b))) trees
  (· >>= filterSrc) <$> specForest directory

def mkModule : List String -> String :=
  ".".intercalate ∘ List.reverse

mutual
  def toSpecs.fromForest (names : List String) : Forest -> List Spec
  | { hook := .WithHook, trees } => [.Hook (mkModule $ "SpecHook" :: names) $ trees.flatMap (toSpecs.fromTree names)]
  | { hook := .WithoutHook, trees } => trees.flatMap (fromTree names)

  def toSpecs.fromTree (names : List String) : Tree -> List Spec
  | .Leaf name => [.Spec $ mkModule (name :: names)]
  | .Node name forest => toSpecs.fromForest (name :: names) forest
end

def toSpecs : Forest -> List Spec :=
  toSpecs.fromForest []

def findSpecs : System.FilePath -> IO (Option (List Spec)) :=
  Functor.map (Functor.map toSpecs) ∘ discover

def linesep :=
  if System.Platform.isWindows then
    "\r\n"
  else
    "\n"

mutual
  partial def moduleNames.fromForest : List Spec -> List String :=
    List.flatMap moduleNames.fromTree

  partial def moduleNames.fromTree : Spec -> List String
  | .Spec name => [name ++ "Spec"]
  | .Hook name forest => name :: moduleNames.fromForest forest
end

def moduleNames : List Spec -> List String :=
  moduleNames.fromForest

def importList (specs : Option (List Spec)) : String :=
  linesep.intercalate $ specs.elim [] moduleNames |>.map λspec => s!"import Test.{spec}"

mutual
  partial def formatSpecs.fromForest : List Spec -> String :=
    " *> ".intercalate ∘ List.map formatSpecs.fromTree

  partial def formatSpecs.fromTree : Spec -> String
  | .Spec name => s!"describe \"{name}\" Test.{name}Spec.spec"
  | .Hook name forest => s!"({name}.hook $ {formatSpecs.fromForest forest})"
end

def formatSpecs (specs? : Option (List Spec)) : String :=
  specs?.elim "do\n  pure ()" formatSpecs.fromForest

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

def mkSpecModule (source : System.FilePath) (config : DiscoverConfig) (nodes : Option (List Spec)) : String :=
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

