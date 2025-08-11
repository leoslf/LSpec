def Except.elim {ε a b} (f : ε -> b) (g : a -> b) : Except ε a -> b
| .error e => f e
| .ok a => g a
