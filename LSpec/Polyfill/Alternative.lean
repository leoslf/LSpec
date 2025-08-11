def Alternative.guarded [Alternative m] (predicate : a -> Bool) (x : a) : m a :=
  if predicate x then
    pure x
  else
    failure

mutual
  partial def many [Alternative m] [Monad m] (v : m a) : m (List a) :=
    some v <|> pure []

  partial def some [Alternative m] [Monad m] (v : m a) : m (List a) := do
    return (<- v) :: (<- many v)
end

export Alternative (guarded)
