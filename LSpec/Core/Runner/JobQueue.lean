namespace LSpec

inductive Concurrency where
| Sequential : Concurrency
| Concurrent : Concurrency
deriving Repr, DecidableEq

-- structure Semaphore where
--   mk ::
--   wait : IO Unit
--   signal : IO Unit
--
-- abbrev CancelQueue := IO.Ref (List
--
-- structure JobQueue where
--   mk ::
--   semaphore :
