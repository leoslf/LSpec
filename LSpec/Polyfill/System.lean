-- FIXME: obviously Std.Internal.Async.System is not production ready
import Std.Internal.Async.System

import LSpec.Polyfill.IO

namespace LSpec

namespace System

export Std.Internal.IO.Async.System (Environment)
export Std.Internal.IO.Async.System (getHomeDir)

abbrev getEnvironment := Std.Internal.IO.Async.System.getEnv
abbrev unsetEnv := Std.Internal.IO.Async.System.unsetEnvVar
abbrev setEnv := Std.Internal.IO.Async.System.setEnvVar

end System

inductive ExitCode where
| Success (code : UInt8 := 0) : ExitCode
| Failure (code : UInt8) : ExitCode
deriving Repr, Ord, BEq

def ExitCode.code : ExitCode -> UInt8
| .Success code => code
| .Failure code => code

def ExitCode.exitWithMessage [Inhabited a] (status : ExitCode) (message : String) : IO a := do
  let handle <- do
    match status with
    | .Success _ => IO.getStdout
    | .Failure _ => IO.getStderr
  handle.putStrLn message
  IO.Process.exit status.code

def System.FilePath.splitFileName (path : System.FilePath) : String × String :=
  let basename := path'.extract (path'.revFind (. == System.FilePath.pathSeparator) |>.getD path'.endPos) path'.endPos
  let dirname := path'.stripSuffix basename
  (dirname, basename)
 where
  path' := s!"{path.normalize}"

def die [Inhabited a] (message : String) (code : UInt8 := 1) : IO a := do
  let progName <- IO.getProgName
  ExitCode.Failure code |>.exitWithMessage s!"{progName}: {message}"
