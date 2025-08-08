-- import LSpec.Prelude
import Polyfill.System

import LSpec.Core.Config

namespace LSpec.Core

structure FailureReport where
  mk ::
  seed : Seed
  maxSuccess : Nat
  maxSize : Nat
  maxDiscardRatio : Nat
  paths : List Path
deriving Repr, BEq, Inhabited

instance : ToString FailureReport where
  toString := reprStr

def read? [Inhabited a] (_ : String) : a := default

def FailureReport.write (config : Config) (report : FailureReport) : IO Unit :=
  match config.failureReport with
  | .some file => IO.FS.writeFile file $ toString report
  | .none => do
    try
      System.setEnv "LSPEC_FAILURES" $ toString report
    catch
    | error => IO.eprintln s!"WARNING: Could not write environment variable LSPEC_FAILURES ({error})"

def FailureReport.read (config : Config) : IO (Option FailureReport) :=
  match config.failureReport with
  | .some file => do
    unless (<- file.pathExists) do
      return .none
    let report <- read? <$> IO.FS.readFile file
    if report.isNone then
      IO.eprintln s!"WARNING: Could not read failure report from file {file}!"
    return report
  | .none => do
    let report <- (· >>= read?) <$> IO.getEnv "LSPEC_FAILURES"
    if report.isNone then
      IO.eprintln "WARNING: Could not read environment variable LSPEC_FAILURES; `--rerun' is ignored!"
    return report

def FailureReport.readOnRerun (config : Config) : IO (Option FailureReport) :=
  if config.rerun then
    FailureReport.read config
  else
    return .none

def FailureReport.apply (report? : Option FailureReport) (config : Config) : Config :=
  {
    config with
    filter? := matchFilter.or rerunFilter,
    seed? := config.getSeed <|> ((·.seed) <$> report?),
    slimCheck := {
      config.slimCheck with
      maxSuccess := config.slimCheck.maxSuccess <|> ((·.maxSuccess) <$> report?),
      maxDiscardRatio := config.slimCheck.maxDiscardRatio <|> ((·.maxDiscardRatio) <$> report?),
      maxSize := config.slimCheck.maxSize <|> ((·.maxSize) <$> report?),
    },
  }
 where
  matchFilter := config.filter?
  rerunFilter :=
    match (·.paths) <$> report? with
    | .some []
    | .none => .none
    | .some xs => .some xs.elem

