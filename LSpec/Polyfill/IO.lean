import LSpec.Polyfill.Encoding

namespace IO

open Function (const)

instance : MonadLift IO IO where
  monadLift := id

instance : MonadLift BaseIO BaseIO where
  monadLift := id

/--
The name of the program as it was invoked.

However, it is hard-to-impossible to implement on some non-Unix OSes, instead, for maximum portability, we just return the leafname of the program as invoked.
 -/
def getProgName : IO String := do
  let executable <- IO.appPath
  return executable.fileName.get!

-- initialize argsRef : IO.Ref (List String) <- IO.mkRef []

-- def withArgs (args : List String) (action : IO a) : IO a := do
--   let original <- argsRef.swap args
--   let result <- action
--   let _ <- argsRef.set original
--   return result
--
-- def useArgs (action : IO a) (args : List String) : IO a :=
--   withArgs args $ do
--     action
--
-- def getArgs : IO (List String) := do
--   argsRef.get

def Error.formatExceptionWith : (Error -> String) -> Error -> String
| showException, exception => showException exception
  -- TODO

namespace FS.Stream

def isWritable (_ : Stream) : IO Bool :=
  -- TODO
  pure true

def isNotDumb (_ : Stream) : IO Bool :=
  (.some "dumb" != ·) <$> getEnv "TERM"

def supportsANSI (stream : Stream) : IO Bool := do
  pure $ (<- stream.isTty) ∧ (<- stream.isWritable) ∧ (<- stream.isNotDumb)

def getEncoding (_ : Stream) : IO (Option TextEncoding) := do
  -- FIXME:
  pure $ .some TextEncoding.utf8

end FS.Stream

abbrev WithStream (m) (α) [Monad m] [MonadFinally m] [MonadLiftT BaseIO m] := IO.FS.Stream -> m α -> m α

def captureB [Monad m] [MonadLift IO m] [MonadFinally m] (withStream : WithStream m a) (action : m a) : m (ByteArray × a) := do
  let buffer : IO.Ref IO.FS.Stream.Buffer <- IO.mkRef {}
  let stream := IO.FS.Stream.ofBuffer buffer
  let result <- withStream stream do
    try
      action
    finally
      stream.flush
  let output <- (·.data) <$> buffer.get
  return (output, result)

def captureB' [Monad m] [MonadLift IO m] [MonadFinally m] (withStream : WithStream m a) (action : m a) : m ByteArray := do
  (·.fst) <$> captureB withStream action

def capture [Monad m] [MonadLift IO m] [MonadFinally m] (withStream : WithStream m a) (action : m a) : m (String × a) := do
  let (bytes, result) <- captureB withStream action
  return (String.fromUTF8! bytes, result)

def capture' [Monad m] [MonadLift IO m] [MonadFinally m] (withStream : WithStream m a) (action : m a) : m String := do
  (·.fst) <$> capture withStream action

def bracket [Monad m] [MonadFinally m] [MonadLift IO m] (before : m a) (after : a -> m b) (action : a -> m c) : m c := do
  let a <- before
  try
    action a
  finally
    after a

def bracket_ [Monad m] [MonadFinally m] [MonadLift IO m] (before : m a) (after : m b) (action: m c) : m c :=
  bracket before (const _ after) (const _ action)

def backtrace (_ : Unit) : BaseIO String := do
  pure ""
  -- let buffer : IO.Ref FS.Stream.Buffer <- IO.mkRef {}
  -- withStdout (FS.Stream.ofBuffer buffer) do
  --   pure $ dbgStackTrace $ Function.const _ ()
  -- String.fromUTF8! <$> (·.data) <$> buffer.get

def eprintln! [ToString a] (s : a) : IO Unit := do
  IO.eprintln s
  (<- IO.getStderr).flush

open System.Platform in
def nproc : IO Nat := do
  (·.trimRight.toNat!) <$> do
    if isWindows then
      Option.get! <$> IO.getEnv "NUMBER_OF_PROCESSORS"
    else if isEmscripten then
      pure "1"
    else if isOSX then
      IO.Process.run {
        cmd := "sysctl"
        args := #["-n", "hw.ncpu"]
      }
    else
      IO.Process.run {
        cmd := "nproc"
        args := #[]
      }
