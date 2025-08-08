-- import Polyfill.MonadIO
--
-- instance [MonadIO m] : MonadIO (ReaderT r m) where
--   monadLift := liftM ∘ liftIO (m := m)

def MonadReader.reads [Monad m] [MonadReader r m] (f : r -> a) : m a :=
  f <$> MonadReader.read

export MonadReader (reads)
