import Polyfill.CallStack

namespace LSpec.Core

structure Location where
  mk ::
  file : System.FilePath
  line : Int
  column : Int
deriving Repr, DecidableEq

instance : ToString Location where
  toString := reprStr

def Location.of : CallStack.Location -> Location
| { file, beginning, .. } =>
  Location.mk file beginning.line beginning.column

def Location.format (location : Location) : String :=
  s!"{location.file}:{location.line}:{location.column}"
