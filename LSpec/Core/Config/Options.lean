import Cli

import LSpec.Parser
import LSpec.Core.Config.Macro

namespace LSpec.Core.Config

open Cli

abbrev Parser := LSpec.Parser String

def LSPEC_OPTIONS : String := "LSPEC_OPTIONS"

inductive FailOn where
| empty : FailOn
| focused : FailOn
| pending : FailOn
| emptyDescription : FailOn
deriving Repr, DecidableEq, BEq, Hashable, Inhabited, Nonempty, ParseableType

inductive ColorMode where
| auto : ColorMode
| never : ColorMode
| always : ColorMode
deriving Repr, DecidableEq, BEq, Hashable, Nonempty, ParseableType

#synth ParseableType ColorMode

instance : Inhabited ColorMode where
  default := .auto

inductive UnicodeMode where
| auto : UnicodeMode
| never : UnicodeMode
| always : UnicodeMode
deriving Repr, DecidableEq, BEq, Hashable, Nonempty, ParseableType

#synth ParseableType ColorMode

instance : Inhabited UnicodeMode where
  default := .auto

-- structure Pattern where
--   mk ::
--   path : List String
-- deriving Repr, DecidableEq, BEq, Inhabited, Nonempty
--
-- instance : ParseableType Pattern where
--   name := "Pattern"
--   parse? s :=
--     match s.unescape (ifs := "./") with
--     | [] => .none
--     | path => .some $ Pattern.mk path

-- #eval (ParseableType.parse? "test.test" : Option Pattern)
