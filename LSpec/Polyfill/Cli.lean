import Cli
-- import Polyfill.EState

namespace Cli

namespace Cmd

def process' (cmd : Cmd) (args : List String) : EIO String Parsed := do
  match <- cmd.process args |>.toBaseIO with
  | .ok (_, parsed) => return parsed
  | .error (_, msg) => throw msg

end Cmd
