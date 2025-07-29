import GetOpt.Declarative

import LSpec.Core.Config.Definition
import LSpec.Core.Config.Options
import LSpec.Core.Config.Parser

namespace LSpec.Core

open GetOpt

structure ConfigFile where
  mk ::
  path : System.FilePath
  args : List String

def runnerOptions : List (Declarative.Types.Option' Config) := [] -- TODO

def formatterOptions (formatters : List Formatter) : List (Declarative.Types.Option' Config) := [] -- TODO

def otherOptions (config : Config) : List (String × List (Declarative.Types.Option' Config)) :=
  [
    ("RUNNER OPTIONS", runnerOptions),
    ("FORMATTER OPTIONS", formatterOptions formatters),
    -- ("OPTIONS FOR QUICKCHECK", quickCheckOptions),
    -- ("OPTIONS FOR SMALLCHECK", smallCheckOptions),
  ] ++ extensionOptions
 where
  formatters := config.availableFormatters
  extensionOptions := getExtensionOptions config

def commandLineOptions (config : Config) : List (String × List (Declarative.Types.Option' Config)) :=
  ("OPTIONS", commandLineOnlyOptions) :: otherOptions config

def ConfigFile.ignored (_config : Config) (_args : List String) : IO Bool := do
  match (<- IO.getEnv "IGNORE_DOT_LSPEC") with
  | .some _ => return true
  | .none =>
    -- TODO: parseCommandLineOptions
    return false

def readConfigFile (path : System.FilePath) : IO (Option ConfigFile) := do
  unless (<- path.pathExists) do
    return .none
  let content <- IO.FS.readFile path
  return (.some $ ConfigFile.mk path content.unescapeArgs)

def readGlobalConfigFile : IO (Option ConfigFile) := do
  let home <- System.getHomeDir
  readConfigFile $ home / ".lspec"

def readLocalConfigFile : IO (Option ConfigFile) := do
  let currentDir <- IO.currentDir
  readConfigFile $ currentDir / ".lspec"

def readConfigFiles : IO (List ConfigFile) := do
  let global <- readGlobalConfigFile
  let local_ <- readLocalConfigFile
  return [global, local_].reduceOption

def parseCommandLineOptions (progName : String) (args : List String) (config : Config) : Except (ExitCode × String) Config :=
  .ok default

--   match Declarative.parseCommandLineOptions (commandLineOptions config) progName args config with
--   | .Success c => .ok c
--   | .Help message => .error (.Success, message)
--   | .Failure message => .error (.Failure 1, message)

def parseEnvironmentOptions (environment : System.Environment) (config : Config) : Except (ExitCode × String) (List String × Config) :=
  match Declarative.parseEnvironmentOptions "LSPEC" environment config $ commandLineOptions config |>.flatMap (·.snd)  with
  -- FIXME
  | _ => .ok default

def parseOtherOptions (progName : String) (source : String) (args : List String) (config : Config) : Except (ExitCode × String) Config :=
  -- FIXME
  .ok default

/-
  match Declarative.parse (interpretOptions options) config args with
  | .ok config' => Except.ok config'
  | .error error =>
    let message := "\n".intercalate $
      match error.splitOn "\n" with
      | x :: [] => [s!"{x} {source}"]
      | xs => xs ++ [source]
    Except.error (.Failure 1, s!"{progName}: {message}")
-/

def parseFileOptions (progName : String) (config : Config) : ConfigFile -> Except (ExitCode × String) Config
| { path, args } => parseOtherOptions progName s!"in config file {path}" args config

def parseEnvVarOptions (progName : String) : EnvVar -> Config -> Except (ExitCode × String) Config :=
  parseOtherOptions progName s!"from environment variable {LSPEC_OPTIONS}"

def parseOptions (config : Config) (progName: String) (configFiles : List ConfigFile) (options : Option EnvVar) (environment : System.Environment) (args : List String) : Except (ExitCode × String) (List String × Config) := do
      MLList.ofList configFiles |>.foldM (parseFileOptions progName) config
  >>= options.elim pure (parseEnvVarOptions progName)
  >>= parseEnvironmentOptions environment
  >>= traverse (parseCommandLineOptions progName args)

def readConfig (config : Config) (args : List String) : IO Config := do
  let progName <- IO.getProgName
  let configFiles <- do
    match (<- ConfigFile.ignored config args) with
    | true => pure []
    | false => readConfigFiles
  let env <- System.getEnvironment
  let options := words <$> env.get? LSPEC_OPTIONS
  match parseOptions config progName configFiles options env args with
  | .error (status, msg) => status.exitWithMessage msg
  | .ok (warnings, config') => do
    for warning in warnings do
      IO.eprintln warning
    return config'

