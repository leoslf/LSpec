import Cli
-- import GetOpt.Declarative

import LSpec.Prelude
import LSpec.Core.Config.Definition
import LSpec.Core.Config.Options
import LSpec.Core.Config.Parser
import LSpec.Core.Runner.Cmd

namespace LSpec.Core

-- open GetOpt

structure ConfigFile where
  mk ::
  path : System.FilePath
  args : List String
deriving Repr, BEq, Inhabited, TypeName

-- def runnerOptions : List (Declarative.Types.Option' Config) := [] -- TODO
--
-- def formatterOptions (formatters : List Formatter) : List (Declarative.Types.Option' Config) := [] -- TODO
--
-- def otherOptions (config : Config) : List (String × List (Declarative.Types.Option' Config)) :=
--   [
--     ("RUNNER OPTIONS", runnerOptions),
--     ("FORMATTER OPTIONS", formatterOptions formatters),
--     -- ("OPTIONS FOR QUICKCHECK", quickCheckOptions),
--     -- ("OPTIONS FOR SMALLCHECK", smallCheckOptions),
--   ] ++ extensionOptions
--  where
--   formatters := config.availableFormatters
--   extensionOptions := getExtensionOptions config

-- def commandLineOptions (config : Config) : List (String × List (Declarative.Types.Option' Config)) :=
--   ("OPTIONS", commandLineOnlyOptions) :: otherOptions config

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

-- def parseCommandLineOptions (args : List String) (config : Config) : EIO (ExitCode × String) Config :=
--
-- --   match Declarative.parseCommandLineOptions (commandLineOptions config) progName args config with
-- --   | .Success c => .ok c
-- --   | .Help message => .error (.Success, message)
-- --   | .Failure message => .error (.Failure 1, message)
--
-- -- def parseEnvironmentOptions (environment : System.Environment) (config : Config) : Except (ExitCode × String) (List String × Config) :=
-- --   match Declarative.parseEnvironmentOptions "LSPEC" environment config $ commandLineOptions config |>.flatMap (·.snd)  with
-- --   -- FIXME
-- --   | _ => .ok default
--
-- def parseOtherOptions (progName : String) (source : String) (args : List String) (config : Config) : Except (ExitCode × String) Config :=
--   -- FIXME
--   .ok default
--
-- /-
--   match Declarative.parse (interpretOptions options) config args with
--   | .ok config' => Except.ok config'
--   | .error error =>
--     let message := "\n".intercalate $
--       match error.splitOn "\n" with
--       | x :: [] => [s!"{x} {source}"]
--       | xs => xs ++ [source]
--     Except.error (.Failure 1, s!"{progName}: {message}")
-- -/
--
-- -- def parseFileOptions (progName : String) (config : Config) : ConfigFile -> Except (ExitCode × String) Config
-- -- | { path, args } => parseOtherOptions progName s!"in config file {path}" args config
-- --
-- -- def parseEnvVarOptions (progName : String) : EnvVar -> Config -> Except (ExitCode × String) Config :=
-- --   parseOtherOptions progName s!"from environment variable {LSPEC_OPTIONS}"
--
-- def parseOptions (config : Config) (progName: String) (configFiles : List ConfigFile) (options : Option EnvVar) (environment : System.Environment) (args : List String) : EIO (ExitCode × String) (List String × Config) := do
--       MLList.ofList configFiles |>.foldM (parseFileOptions progName) config
--   >>= options.elim pure (parseEnvVarOptions progName)
--   >>= parseEnvironmentOptions environment
--   >>= traverse (parseCommandLineOptions args)

-- set_option pp.universes true

def parseOptions (cmd : Cli.Cmd) (args : List String) (config : Config) : EIO (ExitCode × String) (List String × Config) := do
  let mut warnings : List String := []
  let mut config := config
  match ((<- cmd.process' args |>.toBaseIO) : Except String Cli.Parsed) with
  | .ok parsed =>
    if parsed.hasFlag "help" then
      throw (.Success, cmd.help)
    if parsed.cmd.meta.hasVersion ∧ parsed.hasFlag "version" then
      throw (.Success, cmd.meta.version!)

    if parsed.hasFlag "ignore-dot-lspec" then
      config := { config with ignoreConfigFile := true }

    if let .some failOn := parsed.flag? "fail-on" then
      config := { config with failOn := Std.HashSet.ofArray $ failOn.as! (Array FailOn) }

    -- IO.println config.failOn.toArray
    if let .some format := parsed.flag? "format" then
      let format := format.as! String
      let formatter? := config.availableFormatters.lookup format
      if formatter?.isNone then
        warnings := warnings.concat s!"unknown format: {format}"
      config := { config with format? := (·.toFormat) <$> formatter? }

    pure (warnings, config)
  | .error error =>
    throw (.Failure 2, error)

def readConfig (cmd : Cli.Cmd) (config : Config := default) (args : List String) : IO Config := do
  let progName <- IO.getProgName
  let options? <- Functor.map String.unescapeArgs <$> IO.getEnv LSPEC_OPTIONS
  let mut args := args ++ options?.getD []
  match <- parseOptions cmd args config |>.toBaseIO with
  | .error (status, message) =>
    status.exitWithMessage message
  | .ok (warnings, config) =>
    -- TODO: support config files
    -- let configFiles <- do
    --   match <- config.ignoreConfigFile with
    --   | true => pure []
    --   | false => readConfigFiles
    for warning in warnings do
      IO.eprintln warning
    pure config

