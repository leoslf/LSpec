import Polyfill.Encoding

namespace IO

open Function (const)

instance : MonadLift IO IO where
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

def bracket (before : IO a) (after : a -> IO b) (action : a -> IO c) : IO c := do
  let a <- before
  try
    let r <- action a
    let _ <- after a
    return r
  catch
  | e =>
    let _ <- after a
    throw e

def bracket_ (before : IO a) (after : IO b) (action: IO c) : IO c :=
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
