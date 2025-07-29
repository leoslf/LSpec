class MonadIO m extends Monad m, MonadLift IO m where
  liftIO : IO a -> m a := liftM

export MonadIO (liftIO)

instance : MonadIO IO where
  monadLift := id
