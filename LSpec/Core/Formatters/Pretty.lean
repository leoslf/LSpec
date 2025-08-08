import LSpec.Core.Formatters.Pretty.Unicode
import LSpec.Core.Formatters.Pretty.Parser

namespace LSpec.Core.Formatters

def pretty (unicode : Bool) : String -> Option String := .some -- FIXME: sorry
/-
  parseValue >=> render
 where
  render := sorry
-/

def pretty2 (unicode : Bool) (expected : String) (actual : String) : String × String := (expected, actual) -- FIXME: sorry

/-
  let recover := recoverMultiLineString unicode
  match (recover expected, recover actual) with
  | (.some expected', .some actual') => (expected', actual')
  | _ => sorry

-/
