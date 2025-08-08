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

abbrev SpecM a r :=
  WriterT (Function.End Config × SpecForest a) (ReaderT Env IO) r

#synth Monad IO
#synth ∀α, Monoid (Function.End Config × SpecForest α)
#synth Monad (ReaderT Env IO)
#synth ∀α, Monad (WriterT (Function.End Config × SpecForest α) (ReaderT Env IO))

#synth ∀a, Functor (SpecM a)
#synth ∀a, Applicative (SpecM a)
#synth ∀a, Monad (SpecM a)
-- #synth ∀a, MonadIO (SpecM a)

abbrev SpecWith a := SpecM a Unit
abbrev Spec := SpecWith Unit

def SpecWith.run : SpecWith a -> IO (Function.End Config × SpecForest a) :=
  flip ReaderT.run (Env.mk []) ∘ WriterT.exec

def SpecWith.evaluate (config : Config) (spec : SpecWith a) : IO (Config × SpecForest a) := do
  let (f, forest) <- spec.run
  return (f config, forest)

def withEnv {r : Type} (f : Env -> Env) : SpecM a r -> SpecM a r :=
  WriterT.map (withReader f)

def modifyConfig (f : Config -> Config) : SpecWith a :=
  MonadWriter.tell (f, [])

def fromSpecForest : Function.End Config × SpecForest a -> SpecWith a :=
  MonadWriter.tell

def fromSpecList (forest : SpecForest a) : SpecWith a :=
  fromSpecForest (One.one, forest)

-- FIXME: remove this
def runIO : IO r -> SpecM a r :=
  liftM

def mapSpecForest (f : SpecForest a -> List (SpecTree b)) : SpecM a r -> SpecM b r :=
  WriterT.map (Functor.map $ Functor.map $ second f)

def mapSpecItem : (Item a -> Item b) -> SpecWith a -> SpecWith b :=
  mapSpecForest ∘ Forest.bimap id

def modifyParams (f : Params -> Params) : SpecWith a -> SpecWith a :=
  mapSpecItem λitem => { item with example_ := item.example_ ∘ f }
