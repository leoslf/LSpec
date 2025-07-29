import GetOpt.Declarative.Types

namespace GetOpt.Declarative

structure InvalidArgument where
  mk ::
  name : String
  value : String
deriving Repr, BEq

inductive ParseResult Config where
| Help (message : String) : ParseResult Config
| Failure (message : String) : ParseResult Config
| Success (config : Config) : ParseResult Config

-- def interpretOptions {Config} : List (Types.Option' Config) ->

-- def parseWithHelp := sorry

def parseCommandLineOptions {Config} (options : String × List (Types.Option' Config)) (progName : String) (args : List String) (config : Config) : ParseResult Config := sorry
--   match parseWithHelp (options'.flatMap (·.snd)) config args with
--   | .none => .Help usage
--   | .some (.err error) => .Failure $ s!"{progName}: {error}\nTry `{progName} --help' for more information.\n"
--   | .some (.ok config') => .Success config'
--  where
--   options' := sorry -- addHelpFlag $ options.map $ Functor.map interpretOptions
--

abbrev OptDescr (_ : Type u) := Unit

def interpretOptions {Config} : List (Types.Option' Config) -> List (OptDescr (Config -> Except InvalidArgument Config)) := sorry -- [] -- FIXME

def foldResult {Config} (config : Config) (options : List (Config -> Except InvalidArgument Config)) : Except String Config := sorry

def interpretResult {Config} (config : Config) : List (Config -> Except InvalidArgument Config) × List String × List String -> Except String Config :=
  sorry -- interpretGetOptResult >=> foldResult config

def parse {Config} (options : List (OptDescr (Config -> Except InvalidArgument Config))) (config : Config) (args : List String) : Except String Config := sorry

