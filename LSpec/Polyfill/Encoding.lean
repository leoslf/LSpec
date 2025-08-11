structure TextEncoding where
  mk ::
  name : String
  -- decoder {dstate} : IO (TextDecoder dstate)
  -- encoder {estate} : IO (TextEncoder estate)

instance : Repr TextEncoding where
  reprPrec
  | { name, .. }, _ => name

instance : ToString TextEncoding where
  toString := reprStr

def TextEncoding.utf8 : TextEncoding :=
  {
    name := "UTF-8",
  }
