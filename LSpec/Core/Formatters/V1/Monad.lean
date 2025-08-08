import Free

import LSpec.Exception
import LSpec.Core.Seed
import LSpec.Core.Clock
import LSpec.Core.Path
import LSpec.Core.Location

namespace LSpec.Core.Formatters.V1

universe u v

variable {m : Type -> Type} [Monad m]
variable {a : Type}

inductive Failure.Reason where
| NoReason : Failure.Reason
| Reason (reason : String) : Failure.Reason
| ExpectedButGot (preface? : Option String) (expected : String) (actual : String) : Failure.Reason
| Error (info? : Option String) (e : Exception) : Failure.Reason
deriving Repr, BEq

structure Failure where
  mk ::
  location? : Option Location
  path : Path
  message : Failure.Reason
deriving Repr, BEq

inductive FormatF Next where
| getSuccessCount (next : Nat -> Next) : FormatF Next
| getPendingCount (next : Nat -> Next) : FormatF Next
| getFailMessages (next : List Failure -> Next) : FormatF Next
| usedSeed (next : Seed -> Next) : FormatF Next
| printTimes (next : Bool -> Next) : FormatF Next
| getCPUTime? (next : Option Clock.Seconds -> Next) : FormatF Next
| getRealTime (next : Clock.Seconds -> Next) : FormatF Next
| write (s : String) (next : Next) : FormatF Next
| writeTransient (s : String) (next : Next) : FormatF Next
-- | withFailColor : ∀{m : Type -> Type} {a : Type}, (action : m a) -> (next : a -> Next) -> FormatF Next
-- | withSuccessColor : ∀{m : Type -> Type} {a : Type}, (action : m a) -> (next : a -> Next) -> FormatF Next
-- | withPendingColor : ∀{m : Type -> Type} {a : Type}, (action : m a) -> (next : a -> Next) -> FormatF Next
-- | withInfoColor : ∀{m : Type -> Type} {a : Type}, (action : m a) -> (next : a -> Next) -> FormatF Next
| useDiff (next : Bool -> Next) : FormatF Next
| extraChunk (s : String) (next : Next) : FormatF Next
| missingChunk (s : String) (next : Next) : FormatF Next
| liftIO : ∀{a : Type}, (action : IO a) -> (next : a -> Next) -> FormatF Next

abbrev FormatM := Free FormatF

instance : Functor FormatF where
  map f
  | .getSuccessCount next => .getSuccessCount $ f ∘ next
  | .getPendingCount next => .getPendingCount $ f ∘ next
  | .getFailMessages next => .getFailMessages $ f ∘ next
  | .usedSeed next => .usedSeed $ f ∘ next
  | .printTimes next => .printTimes $ f ∘ next
  | .getCPUTime? next => .getCPUTime? $ f ∘ next
  | .getRealTime next => .getRealTime $ f ∘ next
  | .write s next => .write s $ f next
  | .writeTransient s next => .writeTransient s $ f next
  -- | .withFailColor action next => .withFailColor action (f ∘ next)
  -- | .withSuccessColor action next => .withSuccessColor action (f ∘ next)
  -- | .withPendingColor action next => .withPendingColor action (f ∘ next)
  -- | .withInfoColor action next => .withInfoColor action (f ∘ next)
  | .useDiff next => .useDiff $ f ∘ next
  | .extraChunk s next => .extraChunk s $ f next
  | .missingChunk s next => .missingChunk s $ f next
  | .liftIO action next => .liftIO action $ f ∘ next

instance : MonadLift IO FormatM where
  monadLift action := Free.lift (f := FormatF) (FormatF.liftIO action id)

structure Environment (m : Type -> Type) where
  mk ::
  getSuccessCount : m Nat
  getPendingCount : m Nat
  getFailMessages : m (List Failure)
  usedSeed : m Seed
  printTimes : m Bool
  getCPUTime? : m (Option Clock.Seconds)
  getRealTime : m Clock.Seconds
  write : String -> m Unit
  writeTransient : String -> m Unit
  -- withFailColor {a : Type} : m a -> m a
  -- withSuccessColor {a : Type} : m a -> m a
  -- withPendingColor {a : Type} : m a -> m a
  -- withInfoColor {a : Type} : m a -> m a
  useDiff : m Bool
  extraChunk : String -> m Unit
  missingChunk : String -> m Unit
  liftIO : ∀{a : Type}, IO a -> m a

-- set_option diagnostics true

def FormatM.interpretWith [MonadLift IO m] (env : Environment m) : FormatM a -> m a :=
  Free.iterM λ
  | .getSuccessCount next => env.getSuccessCount >>= next
  | .getPendingCount next => env.getPendingCount >>= next
  | .getFailMessages next => env.getFailMessages >>= next
  | .usedSeed next => env.usedSeed >>= next
  | .printTimes next => env.printTimes >>= next
  | .getCPUTime? next => env.getCPUTime? >>= next
  | .getRealTime next => env.getRealTime >>= next
  | .write s next => env.write s *> next
  | .writeTransient s next => env.writeTransient s *> next
  -- | @FormatF.withFailColor _ m' _ action next => do
  --   match eq : m = m' with
  --   | rfl => env.withFailColor (cast eq action) >>= next
  -- | @FormatF.withSuccessColor _ m' _ action next => env.withSuccessColor (a := a) action >>= next
  -- | @FormatF.withPendingColor _ m' a action next => env.withPendingColor (a := a) action >>= next
  -- | @FormatF.withInfoColor _ m' _ action next => env.withInfoColor (a := a) action >>= next
  | .useDiff next => env.useDiff >>= next
  | .extraChunk s next => env.extraChunk s *> next
  | .missingChunk s next => env.missingChunk s *> next
  | .liftIO action next => liftM action >>= next

