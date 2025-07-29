namespace GetOpt.Declarative.Types

inductive Setter config where
| NoArg (setter : config -> config) : Setter config
| Flag (setter : Bool -> config -> config) : Setter config
| OptArg (name : String) (setter : Option String -> config -> Option config) : Setter config
| Arg (name : String) (setter : String -> config -> Option config) : Setter config

structure Option' config where
  mk ::
  name : String
  shortcut : Option Char
  setter : Setter config
  help : String
  documented : Bool
