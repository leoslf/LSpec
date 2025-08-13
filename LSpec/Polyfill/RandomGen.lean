namespace LSpec

def Array.shuffle [RandomGen G] (ref : ST.Ref s G) (input : Array a) : ST s (Array a) := do
  let mut xs := input
  let n := xs.size
  for i in Array.range n do
    xs <- xs.swapIfInBounds i <$> randomIndex i n
  return xs
 where
  randomIndex (lo : Nat) (hi : Nat) : ST s Nat :=
    ref.modifyGet $ λ generator => randNat generator lo hi

def List.shuffle [RandomGen G] (ref : ST.Ref s G) (xs : List a) : ST s (List a) :=
  Array.toList <$> Array.shuffle ref xs.toArray

