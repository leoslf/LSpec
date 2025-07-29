import Polyfill.CallStack

namespace LSpec.Core

structure Location where
  mk ::
  file : System.FilePath
  line : Int
  column : Int
deriving Repr, DecidableEq

def Location.of : CallStack.Location -> Location
| { file, beginning, .. } =>
  Location.mk file beginning.line beginning.column
