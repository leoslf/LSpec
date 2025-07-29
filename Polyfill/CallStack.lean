import Polyfill.IO

namespace CallStack

structure Position where
  mk ::
  line : Int
  column : Int
deriving Repr, Ord, BEq

structure Location where
  mk ::
  package : Lean.Name
  module : Lean.Name
  file : System.FilePath
  beginning : Position
  ending : Position
deriving Repr

end CallStack

abbrev CallStack := List (String × CallStack.Location)

def callStack (_ : Unit) : CallStack :=
  let backtrace := unsafe unsafeBaseIO $ IO.backtrace ()
  dbgTrace backtrace (Function.const Unit [])

def callSite (_ : Unit) : Option (String × CallStack.Location) :=
  callStack () |>.getLast?
