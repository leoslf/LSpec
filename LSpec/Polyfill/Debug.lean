namespace LSpec

@[never_extract]
def dbgTraceM [Monad m] (s : String) : m Unit :=
  dbgTrace s $ λ() => pure ()

@[never_extract]
def dbgTraceWith [ToString β] (f : α -> β) (value : α) : α :=
  dbgTrace s!"{f value}" $ λ() => value

@[never_extract]
def dbgTraceM' [Monad m] [MonadLift BaseIO m] (s : String) : m Unit :=
  let tid <- IO.getTID
  dbgTraceM s!"[tid: {tid}] {s}"

