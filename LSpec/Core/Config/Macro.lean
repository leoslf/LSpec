import Lean

import Cli
import LSpec.Casing

open Lean Elab Parser Term Command

namespace Cli

def deriveParseableType (declNames : Array Name) (transform : String -> String := String.kebab) : CommandElabM Bool := do
  let env <- getEnv
  for declName in declNames do
    if let .some (.inductInfo ind) := env.find? declName then
      let mut cases : Array (TSyntax ``matchAlt) := #[]

      for member in ind.ctors do
        let ctor : Ident := Lean.mkIdent member
        let .str _ name := member
          | unreachable!
        let literal := Syntax.mkStrLit (transform $ toString name)
        cases := cases.push (<- `(matchAltExpr| | $literal => .some $ctor))
      cases := cases.push (<- `(matchAltExpr| | _ => .none))

      let name := Syntax.mkStrLit (toString declName)
      let cmd <- `(
        instance : ParseableType $(mkIdent declName) where
          name := $name
          parse? $cases:matchAlt*
      )
      elabCommand cmd
    return true

  return false

initialize
  registerDerivingHandler ``ParseableType deriveParseableType

end Cli
