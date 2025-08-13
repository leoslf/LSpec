import LSpec.Prelude

import LSpec.Core.Seed
import LSpec.Core.Path
import LSpec.Core.Annotations

import LSpec.Core.DiffContext
import LSpec.Core.Format
import LSpec.Core.Formatters
import LSpec.Core.Config.Options

namespace LSpec.Core

open LSpec.Core (Seed)
open LSpec.Core.Formatters

structure SlimCheckConfig where
  seed? : Option Seed
  maxSuccess : Option Nat
  maxDiscardRatio : Option Nat
  maxSize : Option Nat
  maxShrinks : Option Nat
deriving Repr, Inhabited, DecidableEq

structure Config where
  mk ::
  ignoreConfigFile : Bool := false
  dryRun : Bool := false
  focusedOnly : Bool := false
  failOn : Std.HashSet Config.FailOn := {}
  printSlowItems : Option Nat := .none
  printCpuTime : Bool := false
  failFast : Bool := false
  randomize : Bool := false
  seed? : Option Seed := .none
  failureReport? : Option System.FilePath := .none
  rerun : Bool := false
  rerunAllOnSuccess : Bool := false

  /--
    A predicate that is used to filter the spec before it is run.
    Only examples that satisfy the predicate are run.
  -/
  filter? : Option (Path -> Bool) := .none
  skip? : Option (Path -> Bool) := .none

  slimCheck : SlimCheckConfig := default
  smallCheckDepth : Option Nat := .none
  colorMode : Config.ColorMode := default
  unicodeMode : Config.UnicodeMode := default
  diff : Bool := false
  diffContext? : Option DiffContext := .none
  externalDiff? : Option (Option Int -> String -> String -> IO Unit) := .none
  prettyPrint : Bool := false
  prettyPrintFunction : Bool -> String -> String -> String × String := pretty2
  formatException : IO.Error -> String := IO.Error.formatExceptionWith toString
  times : Bool := false
  expertMode : Bool := false
  availableFormatters : List (String × V2.Formatter)
  format? : Option (Format.Config -> IO Format) := .none
  -- FIXME: universe-level problems
  -- formatterV1? : Option V1.Formatter := .none
  htmlOutput : Bool := false
  concurrentJobs? : Option Nat := .none
  annotations : Annotations := {}
deriving Inhabited, Repr

instance : ToString Config where
  toString := reprStr

def Config.mkDefault (formatters : List (String × V2.Formatter)) : Config :=
  {
    availableFormatters := formatters
  }

instance : Inhabited Config where
  default := Config.mkDefault $
    [
      ("checks", V2.checks),
      ("specdoc", V2.specdoc),
      ("progress", V2.progress),
      ("failed-examples", V2.failed_examples),
      ("silent", V2.silent),
    ]

-- set_option diagnostics true

def Config.getFormatter (config : Config) (formatterV1? : Option V1.Formatter := .none) : Option (Format.Config -> IO Format) :=
  config.format? <|> formatterV1?.map (·.toFormat)

def Config.getSeed (config : Config) : Option Seed :=
  config.seed? <|> config.slimCheck.seed?

def Config.ensureSeed (config : Config) : IO (Seed × Config) := do
  let seed <- ensure config.seed?
  return (seed, { config with seed? := .some seed })
 where
  ensure : Option Seed -> IO Seed
  | .none => liftM Seed.new
  | .some seed => pure seed

abbrev Filter := Option (Path -> Bool)

def Filter.or : Filter -> Filter -> Filter
| .some f, .some g => .some $ λpath => f path || g path
| f, g => f <|> g

def Config.addMatch (pattern : String) (config : Config) : Config :=
  { config with filter? := Option.some (Path.filterPredicate pattern) |>.or config.filter? }

def Config.addSkip (pattern : String) (config : Config) : Config :=
  { config with skip? := Option.some (Path.filterPredicate pattern) |>.or config.skip? }


structure ExtensionOptions where
  mk ::
  -- unExtensionOptions : List (String × List (Declarative.Types.Option' Config))
deriving TypeName

def getConfigAnnotation {a : Type u} [TypeName a] : Config -> Option a :=
  Annotations.getValue ∘ Config.annotations

-- def getExtensionOptions (config : Config) : List (String × List (Declarative.Types.Option' Config)) :=
--   getConfigAnnotation config |>.elim [] ExtensionOptions.unExtensionOptions

