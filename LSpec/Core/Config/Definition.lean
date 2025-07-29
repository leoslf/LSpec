import LSpec.Prelude

import GetOpt.Declarative

import LSpec.Core.Seed
import LSpec.Core.Annotations

import LSpec.Core.DiffContext
import LSpec.Core.Format
import LSpec.Core.Formatters

namespace LSpec.Core

open GetOpt
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
  seed : Option Seed
  maxSuccess : Option Nat
  maxDiscardRatio : Option Nat
  maxSize : Option Nat
  maxShrinks : Option Nat
deriving Repr, Inhabited, DecidableEq

-- #check Path

structure Config where
  ignoreConfigFile : Bool
  dryRun : Bool
  focusedOnly : Bool
  failOnEmpty : Bool
  failOnFocused : Bool
  failOnPending : Bool
  failOnEmptyDescription : Bool
  printSlowItems : Option Nat
  printCpuTime : Bool
  failFast : Bool
  randomize : Bool
  seed : Option Seed
  failureReport : Option System.FilePath
  rerun : Bool
  rerunAllOnSuccess : Bool
  /--
    A predicate that is used to filter the spec before it is run.
    Only examples that satisfy the predicate are run.
  -/
  filter : Option (Path -> Bool)
  skip : Option (Path -> Bool)

  slimCheck : SlimCheckConfig
  smallCheckDepth : Option Nat
  colorMode : ColorMode
  unicodeMode : UnicodeMode
  diff : Bool
  diffContext : Option DiffContext
  externalDiff : Option (Option Int -> String -> String -> IO Unit)
  prettyPrint : Bool
  prettyPrintFunction : Bool -> String -> String -> String × String
  formatException : IO.Error -> String
  times : Bool
  expertMode : Bool
  availableFormatters : List Formatter
  format : Option (Format.Config -> IO Format)
  formatter : Option V1.Formatter
  htmlOutput : Bool
  concurrentJobs : Option Nat
  annotations : Annotations
deriving Inhabited

-- #check Annotations
-- #check Format
-- #check Format.Config
-- #check Config

def Config.mkDefault (formatters : List Formatter) : Config :=
  {
    ignoreConfigFile := false,
    dryRun := false,
    focusedOnly := false,
    failOnEmpty := false,
    failOnFocused := false,
    failOnPending := false,
    failOnEmptyDescription := false,
    printSlowItems := .none,
    printCpuTime := false,
    failFast := false,
    randomize := false,
    seed := .none,
    failureReport := .none,
    rerun := false,
    rerunAllOnSuccess := false,
    filter := .none,
    skip := .none,

    slimCheck := default,
    smallCheckDepth := .none,
    colorMode := default,
    unicodeMode := default,
    diff := true,
    diffContext := .some DiffContext.default
    externalDiff := .none,
    prettyPrint := true,
    prettyPrintFunction := Pretty.pretty2,
    formatException := IO.Error.formatExceptionWith toString,
    times := false,
    expertMode := false,
    availableFormatters := formatters,
    format := .none,
    formatter := .none,
    htmlOutput := false,
    concurrentJobs := .none,
    annotations := {},
  }

instance : Inhabited Config where
  default := Config.mkDefault $ []
    -- [
    --   ("checks", V2.checks),
    --   ("specdoc", V2.specdoc),
    --   ("progress", V2.progress),
    --   ("failed-examples", V2.failed_examples),
    --   ("silent", V2.silent),
    -- ]
    --   |>.map (Functor.map V2.formatterToFormat)

def Config.getSeed (config : Config) : Option Seed :=
  config.seed <|> config.slimCheck.seed

def Config.ensureSeed (config : Config) : IO (Seed × Config) := do
  let seed <-
    match config.seed with
    | .none => Seed.new
    | .some seed => pure seed
  return (seed, { config with seed := .some seed })

abbrev Filter := Option (Path -> Bool)

def Filter.or : Filter -> Filter -> Filter
| .some f, .some g => .some $ λ path => f path ∨ g path
| f, g => f <|> g

def addMatch (pattern : String) (config : Config) : Config :=
  { config with filter := Option.some (Path.filterPredicate pattern) |>.or config.filter }

def addSkip (pattern : String) (config : Config) : Config :=
  { config with skip := Option.some (Path.filterPredicate pattern) |>.or config.skip }

def argument {Config} (name : String) (parser : String -> Option a) (setter : a -> Config -> Config) : Declarative.Types.Setter Config :=
  .Arg name $ λ input config => flip setter config <$> parser input

def commandLineOnlyOptions : List (Declarative.Types.Option' Config) :=
  [
    .mk "ignore-dot-lspec" .none (.NoArg setIgnoreConfigFile) "do not read options from ~/.lspec and .lspec" true,
    .mk "match" (.some 'm') (argument "PATTERN" pure addMatch) "only run examples that match given PATTERN" true,
    .mk "skip" .none (argument "PATTERN" pure addSkip) "skip examples that match given PATTERN" true,
  ]
 where
  setIgnoreConfigFile (config : Config) := { config with ignoreConfigFile := true }

structure ExtensionOptions where
  mk ::
  unExtensionOptions : List (String × List (Declarative.Types.Option' Config))
deriving TypeName

def getConfigAnnotation [TypeName a] : Config -> Option a :=
  Annotations.getValue ∘ Config.annotations

def getExtensionOptions : Config -> List (String × List (Declarative.Types.Option' Config)) :=
  (·.elim [] ExtensionOptions.unExtensionOptions) ∘ getConfigAnnotation

