import LSpec.Core.Tree
import LSpec.Core.Clock
import LSpec.Core.Config
import LSpec.Core.Expectations
import LSpec.Core.FailureReport
import LSpec.Core.Spec.Monad

namespace LSpec.Core

def describe (label : String) : SpecWith a -> SpecWith a :=
  withEnv pushLabel ∘ mapSpecForest (pure ∘ specGroup label)
 where
  pushLabel : Env -> Env
  | { specDescriptionPath } => Env.mk $ label :: specDescriptionPath

def context : String -> SpecWith a -> SpecWith a := describe

def it [Example a] (label : String) (action : a) : SpecWith (Example.Arg a) := do
  fromSpecList [specItem label action]

def setParallelizable (value : Bool) (item : SpecTree.Item a) : SpecTree.Item a :=
  { item with parallelizable? := item.parallelizable? <|> .some value }

def parallel : SpecWith a -> SpecWith a :=
  mapSpecItem (setParallelizable true)

def sequential : SpecWith a -> SpecWith a :=
  mapSpecItem (setParallelizable false)

-- FIXME:
def pending : Expectation := do
  sorry
  -- throw $ .Pending (location ()) .none

def pending_ : Expectation := do
  -- throw $ .Pending .none .none
  sorry

def pendingWith (reason : String) : Expectation := do
  -- throw $ .Pending (location ()) $ .some reason
  sorry

def getSpecDescriptionPath : SpecM a (List String) := do
  List.reverse <$> reads Env.specDescriptionPath
