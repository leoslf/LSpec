import LSpec.Prelude
import LSpec.Core.Tree
import LSpec.Core.Config

namespace LSpec.Core

open SpecTree (Item)
open Example (Params ActionWith ProgressCallback Result)

universe u v

variable [Monad m]

structure Env where
  mk ::
  specDescriptionPath : List String
deriving Repr

abbrev SpecM a := WriterT (Function.End Config × SpecForest a) (ReaderT Env IO)

#synth Monad IO
#synth ∀α, Monoid (Function.End Config × SpecForest α)
#synth Monad (ReaderT Env IO)
#synth ∀α, Monad (WriterT (Function.End Config × SpecForest α) (ReaderT Env IO))

#synth ∀a, Functor (SpecM a)
#synth ∀a, Applicative (SpecM a)
#synth ∀a, Monad (SpecM a)

-- NOTE: abbrev is NOT transparent
-- Defining SpecWith and Spec as notation with parentheses delays the type application, eventually allowing do-notation
notation:max "SpecWith " a:max => (SpecM a Unit)
notation:max "Spec" => (SpecM Unit Unit)

#check SpecWith Unit
#check Spec

def SpecM.run : SpecWith a -> IO (Function.End Config × SpecForest a) :=
  flip ReaderT.run (Env.mk []) ∘ WriterT.exec

def SpecM.evaluate (config : Config) (spec : SpecWith a) : IO (Config × SpecForest a) := do
  let (f, forest) <- spec.run
  return (f config, forest)

def withEnv {r : Type} (f : Env -> Env) : SpecM a r -> SpecM a r :=
  WriterT.map (withReader f)

def modifyConfig (f : Config -> Config) : SpecWith a :=
  MonadWriter.tell (f, [])

def fromSpecForest : Function.End Config × SpecForest a -> SpecWith a :=
  MonadWriter.tell

def fromSpecList (forest : SpecForest a) : SpecWith a :=
  fromSpecForest (mempty, forest)

-- FIXME: remove this
def runIO : IO r -> SpecM a r :=
  liftM

def mapSpecForest (f : SpecForest a -> List (SpecTree b)) : SpecM a r -> SpecM b r :=
  WriterT.map (Functor.map $ Functor.map $ second f)

def mapSpecItem : (Item a -> Item b) -> SpecWith a -> SpecWith b :=
  mapSpecForest ∘ Forest.bimap id

def modifyParams (f : Params -> Params) : SpecWith a -> SpecWith a :=
  mapSpecItem λitem => { item with example_ := item.example_ ∘ f }
