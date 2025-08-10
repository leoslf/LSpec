import Mathlib.Control.Traversable.Basic

import Polyfill.String

namespace LSpec.Casing

structure Identifier α where
  mk ::
  unIdentifier : List α
deriving Repr, BEq, DecidableEq, Inhabited, Nonempty

def Identifier.with (f : List α -> List β) : Identifier α -> Identifier β
| ⟨words⟩ => ⟨f words⟩

def Identifier.map (f : α -> β) : Identifier α -> Identifier β :=
  Identifier.with $ List.map f

instance : Functor Identifier where
  map := Identifier.map

def Identifier.pure (word : α) : Identifier α :=
  Identifier.mk [word]

def Identifier.seq (mf : Identifier (α -> β)) (mα : Unit -> Identifier α) : Identifier β :=
  match mf, mα () with
  | ⟨fs⟩, ⟨as⟩ => ⟨as.flatMap λa => fs.map λf => f a⟩

instance : Applicative Identifier where
  pure := Identifier.pure
  seq := Identifier.seq

def Identifier.bind : (m : Identifier α) -> (f : α -> Identifier β) -> Identifier β
| ⟨as⟩, f => ⟨as.flatMap (Identifier.unIdentifier ∘ f)⟩

instance : Monad Identifier where
  bind := Identifier.bind

def Identifier.foldl (f : b -> a -> b) (initial : b) : Identifier a -> b
| ⟨words⟩ => words.foldl f initial

def Identifier.foldr (f : a -> b -> b) (initial : b) : Identifier a -> b
| ⟨words⟩ => words.foldr f initial

def Identifier.traverse [Applicative m] (f : a -> m b) : Identifier a -> m (Identifier b)
| ⟨words⟩ => Identifier.mk <$> words.traverse f

instance : Traversable Identifier where
  traverse := Identifier.traverse

namespace Identifier

partial def fromHumps : String -> Identifier String :=
  .mk ∘ go
 where
  go : String -> List String
  | ⟨[]⟩ => [""]
  | ⟨[x]⟩ => [String.singleton x]
  | xxs@(⟨x :: xs⟩) =>
    if x.isUpper then
      match xxs.span Char.isUpper with
      | (lhs, "") => [lhs]
      | (lhs, rhs) =>
        let curLen := lhs.length - 1
        let cur := lhs.take curLen
        let rec_ := go rhs
        let nxt := lhs.drop curLen ++ "".intercalate (rec_.take 1)
        let rem := rec_.drop 1
        let curL := if cur.isEmpty then [] else [cur]
        let nxtL := if nxt.isEmpty then [] else [nxt]
        curL ++ nxtL ++ rem
    else
      match xxs.span (not ∘ Char.isUpper) with
      | (cur, "") => [cur]
      | (cur, rem) => cur :: go rem

def fromWords : String -> Identifier String :=
  .mk ∘ String.splitOn (sep := " ")

def fromKebab : String -> Identifier String :=
  .mk ∘ String.splitOn (sep := "-")

def fromSnake : String -> Identifier String :=
  .mk ∘ String.splitOn (sep := "_")

def fromAny : String -> Identifier String :=
  fromHumps >=> fromKebab >=> fromSnake >=> fromWords

def toPascal : Identifier String -> String
| ⟨words⟩ => "".intercalate $ words.map (·.toTitle)

def toCamel : Identifier String -> String
| ⟨[]⟩ => ""
| ⟨word :: words⟩ => word.toLower ++ (Identifier.mk words).toPascal

def toKebab : Identifier String -> String
| ⟨words⟩ => "-".intercalate $ words.map String.toLower

/-- To "snake_Case" -/
def toSnake : Identifier String -> String
| ⟨words⟩ => "_".intercalate $ words.map String.toLower

/-- To "quiet_snake_Case" -/
def toQuietSnake : Identifier String -> String :=
  String.toLower ∘ Identifier.toSnake

/-- To "SCREAMING_SNAKE_CASE" -/
def toScreamingSnake : Identifier String -> String :=
  String.toLower ∘ Identifier.toSnake

def toWords : Identifier String -> String
| ⟨words⟩ => words.unwords

def dropPrefix : Identifier a -> Identifier a :=
  Identifier.with $ List.drop 1

end Identifier

