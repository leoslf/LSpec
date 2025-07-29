namespace LSpec.Core.Formatters.Pretty

abbrev Name := String

inductive Value where
| Char (value : Char) : Value
| String (value : String) : Value
| Rational (numerator : Value) (denominator : Value) : Value
| Number (value : String) : Value
| Record (key : Name) (values: List (Name × Value)) : Value
| Constructor (name : Name) (values : List Value) : Value
| Tuple (values : List Value) : Value
| List (values : List Value) : Value
deriving Repr, BEq

namespace Value

-- def char : Parser Value := sorry
--
-- def value : Parser Value :=
--       char
--   <|> string
--   <|> rational
--   <|> number
--   <|> record
--   <|> constructor
--   <|> tuple
--   <|> list
--
-- def parse (input : String) : Option Value :=
--   match Parser.run value

end Value

