import LSpec.Core.Tree
import LSpec.Core.Location
import LSpec.Core.Clock
import LSpec.Core.Runner.JobQueue

namespace LSpec.Core.Runner

structure Eval.Item where
  description : String
  location : Option Location
  concurrency : Concurrency
  action : Example.ProgressCallback -> IO (Clock.Seconds × Example.Result)

abbrev Eval.Tree := LSpec.Core.Tree (IO Unit) Eval.Item
abbrev Eval.Forest := List Eval.Tree
