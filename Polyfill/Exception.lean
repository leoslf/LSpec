structure Exception where
  mk ::
  message : String
deriving Repr, BEq

def Exception.of (e : IO.Error) : Exception :=
  .mk $ e.toString

instance : ToString Exception where
  toString := reprStr
