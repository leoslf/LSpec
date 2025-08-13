import LSpec.Polyfill.Alternative

namespace LSpec

structure Parser (s : Type u) (a : Type u) where
  mk ::
  run : s -> Option (a × s)

def Parser.map (f : a -> b) (parser : Parser s a) : Parser s b :=
  Parser.mk λs => do
    let (a, s') <- parser.run s
    return (f a, s')

instance : Functor (Parser s) where
  map := Parser.map

instance : Applicative (Parser s) where
  pure a := Parser.mk λs => (a, s)
  seq pf pa := Parser.mk λs => do
    let (f, s) <- pf.run s
    let (a, s) <- pa () |>.run s
    return (f a, s)

instance : Alternative (Parser s) where
  failure := Parser.mk $ Function.const _ .none
  orElse a b := Parser.mk λs =>
    a.run s <|> (b ()).run s

instance : Stream String Char where
  next? s :=
    match Stream.next? s.toList with
    | .none => .none
    | .some (a, s) => .some (a, s.asString)

def satisfy [Stream stream token] (predicate : token -> Bool) : Parser stream token :=
  Parser.mk λs =>
    Stream.next? s |>.filter λ(a, _) => predicate a

def sepBy1 [Alternative m] [Monad m] : m a -> m sep -> m (List a)
| p, sep => (· :: ·) <$> p <*> many (sep *> p)

def sepBy [Alternative m] [Monad m] : m a -> m sep -> m (List a)
| p, sep => sepBy1 p sep <|> pure []
