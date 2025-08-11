import LSpec.GetOpt.Declarative.Types
import LSpec.Polyfill

namespace LSpec.GetOpt.Declarative

structure InvalidValue where
  mk ::
  name : String
  value : String
deriving Repr, BEq

def parseEnvironmentOption {Config} (prefix_ : String) (environment : System.Environment) (config : Config) (option : Types.Option' Config) : Except InvalidValue Config :=
  match environment.get? name with
  | .none => .ok config
  | .some value =>
    let invalid := .error $ InvalidValue.mk name value
    match option.setter with
    | .NoArg setter =>
      match value with
      | "yes" => .ok $ setter config
      | _ => invalid
    | .Flag setter =>
      match value with
      | "yes" => .ok $ setter true config
      | "no" => .ok $ setter false config
      | _ => invalid
    | .OptArg _ setter =>
      match setter (.some value) config with
      | .some config' => .ok config'
      | _ => invalid
    | .Arg _ setter =>
      match setter value config with
      | .some config' => .ok config'
      | _ => invalid
 where
  normalize : Char -> Char
  | '-' => '_'
  | c => c.toUpper
  name : String := s!"{prefix_}_{option.name.map normalize}"

def parseEnvironmentOptions {Config} (prefix_ : String) (environment : System.Environment) (config : Config) : List (Types.Option' Config) -> List InvalidValue × Config :=
  List.foldr f ([], config)
 where
  f (option : Types.Option' Config) : List InvalidValue × Config -> List InvalidValue × Config
  | (errors, config) =>
    match parseEnvironmentOption prefix_ environment config option with
    | .error error => (error :: errors, config)
    | .ok config' => (errors, config')

