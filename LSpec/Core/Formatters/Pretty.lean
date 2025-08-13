import LSpec.Prelude

import LSpec.Core.Formatters.Pretty.Unicode
import LSpec.Core.Formatters.Pretty.Parser

namespace LSpec.Core.Formatters

def recoverString (input : String) : Option String :=
  if input.startsWith "\"" && input.endsWith "\"" then
    match input.unescape with
    | [s] => .some s
    | _ => .none
  else
    .none

def recoverMultiLineString (unicode : Bool) (input : String) : Option String :=
  recoverString input |>.filter shouldParseBack
 where
  isSafe (c : Char) : Bool := (unicode || c.isAscii) && not c.isControl || c == '\n'
  isMultiline (input : String) : Bool := input.lines.length > 1
  shouldParseBack (input : String) :Bool := input.all isSafe && isMultiline input

def pretty (unicode : Bool) : String -> Option String := .some -- FIXME: sorry
/-
  parseValue >=> render
 where
  render := sorry
-/

def pretty2 (unicode : Bool) (expected : String) (actual : String) : String × String :=
  match recoverMultiLineString unicode expected, recoverMultiLineString unicode actual with
  | .some expected, .some actual => (expected, actual)
  | _, _ =>
    match pretty unicode expected, pretty unicode actual with
    | .some expected', .some actual' =>
      if expected' != actual' then
        (expected', actual')
      else
        (expected, actual)
    | _, _ =>
      (expected, actual)

/-
  let recover := recoverMultiLineString unicode
  match (recover expected, recover actual) with
  | (.some expected', .some actual') => (expected', actual')
  | _ => sorry

-/
