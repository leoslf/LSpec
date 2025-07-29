import Polyfill.MonadIO

instance [MonadIO m] : MonadIO (ReaderT r m) where
  monadLift := liftM ∘ liftIO (m := m)
