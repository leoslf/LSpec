import Mathlib.Algebra.Group.Defs
import Mathlib.Control.Monad.Writer

import LSpec.Polyfill.Monad

def WriterT.map [Monoid w] [Monoid w'] [Monad n] (f : m (a × w) -> n (b × w')) : WriterT w m a -> WriterT w' n b :=
  WriterT.mk ∘ f ∘ WriterT.run

def WriterT.exec [Monad m] : WriterT w m a -> m w
| m => liftM1 Prod.snd $ WriterT.run m
