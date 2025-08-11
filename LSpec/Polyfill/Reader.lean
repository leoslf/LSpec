def MonadReader.reads [Monad m] [MonadReader r m] (f : r -> a) : m a :=
  f <$> MonadReader.read

export MonadReader (reads)
