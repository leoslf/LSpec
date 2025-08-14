namespace LSpec.Core

abbrev Args := List String
abbrev ArgsT := ReaderT Args

@[inline]
def ArgsT.run (self : ArgsT m a) (args : List String) : m a :=
  ReaderT.run self args

variable [Monad m] [MonadReaderOf Args m] [MonadWithReaderOf Args m]

@[inline]
def ArgsT.getArgs : m Args :=
  read

@[inline]
def ArgsT.withArgs (args : Args) : m a -> m a :=
  withReader (Function.const _ args)

export ArgsT (getArgs withArgs)

instance : MonadLift BaseIO (ArgsT IO) where
  monadLift := liftM (m := IO) ∘ liftM (n := IO)
