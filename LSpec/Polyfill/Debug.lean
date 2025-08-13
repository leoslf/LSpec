namespace LSpec

@[never_extract]
def dbgTraceM [Monad m] (s : String) : m Unit :=
  dbgTrace s $ λ() => pure ()

@[never_extract]
def dbgTraceWith [ToString β] (f : α -> β) (value : α) : α :=
  dbgTrace s!"{f value}" $ λ() => value

@[never_extract]
def dbgTraceM' (s : String) : BaseIO Unit := do
  let tid <- IO.getTID
  if debug then
    dbgTraceM s!"[tid: {tid}] {s}"
 where
  debug := false

