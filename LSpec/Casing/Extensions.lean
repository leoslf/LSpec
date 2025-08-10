import LSpec.Casing.Identifier

namespace String

open LSpec.Casing.Identifier

def pascal : String -> String :=
  toPascal ∘ fromAny

def camel : String -> String :=
  toCamel ∘ fromAny

def snake : String -> String :=
  toSnake ∘ fromAny

def quietSnake : String -> String :=
  toQuietSnake ∘ fromAny

def screamingSnake : String -> String :=
  toScreamingSnake ∘ fromAny

def kebab : String -> String :=
  toKebab ∘ fromAny

def wordify : String -> String :=
  toWords ∘ fromAny

end String
