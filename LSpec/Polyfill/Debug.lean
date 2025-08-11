@[never_extract]
def dbgTraceM [Monad m] (s : String) : m Unit :=
  dbgTrace s $ λ() => pure ()
