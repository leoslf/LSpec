import GetOpt.Declarative

import LSpec.Prelude

import LSpec.Core.Seed
import LSpec.Core.Path
import LSpec.Core.Annotations

import LSpec.Core.DiffContext
import LSpec.Core.Format
import LSpec.Core.Formatters

namespace LSpec.Core

open GetOpt
open LSpec.Core (Seed)
open LSpec.Core.Formatters

namespace Config

inductive ColorMode where
| auto : ColorMode
| never : ColorMode
| always : ColorMode
deriving Repr, DecidableEq

instance : Inhabited ColorMode where
  default := .auto

inductive UnicodeMode where
| auto : UnicodeMode
| never : UnicodeMode
| always : UnicodeMode
deriving Repr, DecidableEq

instance : Inhabited UnicodeMode where
  default := .auto

end Config

open Config

structure SlimCheckConfig where
  seed? : Option Seed
  maxSuccess : Option Nat
  maxDiscardRatio : Option Nat
  maxSize : Option Nat
  maxShrinks : Option Nat
deriving Repr, Inhabited, DecidableEq

#check Format
#check Format.Config

-- #check Path

inductive FailOn where
| empty : FailOn
| focused : FailOn
| pending : FailOn
| emptyDescription : FailOn
deriving Repr, BEq, Hashable

#synth Repr (Path -> Bool)
#synth Repr (Option (Path -> Bool))

structure Config where
  mk ::
  ignoreConfigFile : Bool := false
  dryRun : Bool := false
  focusedOnly : Bool := false
  failOn : Std.HashSet FailOn := {}
  printSlowItems : Option Nat := .none
  printCpuTime : Bool := false
  failFast : Bool := false
  randomize : Bool := false
  seed? : Option Seed := .none
  failureReport : Option System.FilePath := .none
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
  colorMode : ColorMode := default
  unicodeMode : UnicodeMode := default
  diff : Bool := false
  diffContext? : Option DiffContext := .none
  externalDiff? : Option (Option Int -> String -> String -> IO Unit) := .none
  prettyPrint : Bool := false
  prettyPrintFunction : Bool -> String -> String -> String × String := pretty2
  formatException : IO.Error -> String := IO.Error.formatExceptionWith toString
  times : Bool := false
  expertMode : Bool := false
  availableFormatters : List Formatter
  format? : Option (Format.Config -> IO Format) := .none
  -- FIXME: universe-level problems
  -- formatter? : Option V1.Formatter := .none
  htmlOutput : Bool := false
  concurrentJobs : Option Nat := .none
  annotations : Annotations := {}
deriving Inhabited, Repr

#check Config
-- #print Config

-- #check Annotations
-- #check Format
-- #check Format.Config
-- #check Config

def Config.mkDefault (formatters : List Formatter) : Config :=
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
    ] |>.map $ second V2.Formatter.toFormat

-- set_option diagnostics true

def Config.getFormatter (config : Config) (formatter? : Option V1.Formatter := .none) : Option (Format.Config -> IO Format) :=
  config.format? <|> formatter?.map (·.toFormat)

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
| .some f, .some g => .some $ λ path => f path ∨ g path
| f, g => f <|> g

def addMatch (pattern : String) (config : Config) : Config :=
  { config with filter? := Option.some (Path.filterPredicate pattern) |>.or config.filter? }

def addSkip (pattern : String) (config : Config) : Config :=
  { config with skip? := Option.some (Path.filterPredicate pattern) |>.or config.skip? }

-- def argument {Config} (name : String) (parser : String -> Option a) (setter : a -> Config -> Config) : Declarative.Types.Setter Config :=
--   .Arg name $ λ input config => flip setter config <$> parser input

-- def commandLineOnlyOptions : List (Declarative.Types.Option' Config) :=
--   [
--     .mk "ignore-dot-lspec" .none (.NoArg setIgnoreConfigFile) "do not read options from ~/.lspec and .lspec" true,
--     .mk "match" (.some 'm') (argument "PATTERN" pure addMatch) "only run examples that match given PATTERN" true,
--     .mk "skip" .none (argument "PATTERN" pure addSkip) "skip examples that match given PATTERN" true,
--   ]
--  where
--   setIgnoreConfigFile (config : Config) := { config with ignoreConfigFile := true }

structure ExtensionOptions where
  mk ::
  unExtensionOptions : List (String × List (Declarative.Types.Option' Config))
deriving TypeName

def getConfigAnnotation {a : Type u} [TypeName a] : Config -> Option a :=
  Annotations.getValue ∘ Config.annotations

def getExtensionOptions (config : Config) : List (String × List (Declarative.Types.Option' Config)) :=
  getConfigAnnotation config |>.elim [] ExtensionOptions.unExtensionOptions

