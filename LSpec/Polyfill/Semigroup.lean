namespace LSpec

universe u

variable {α β : Type u}

class Semigroup (α) where
  op : α -> α -> α
  sconcat (as : List α) (h : as ≠ []) : α
  -- :=
  --   let rec go (b : α) : List α -> α
  --     | [] => b
  --     | c :: cs => op b $ go c cs
  --   match as with
  --   | a :: as => go a as

  -- FIXME: naive implementation
  stimes (n : Nat) (x : α) (h : n > 0) : α
  --  :=
  --   let rec go' : Nat -> α -> α
  --     | 0 => id
  --     | n + 1 => go' n ∘ op x
  --   go' n x

infixr:67 " <> " => Semigroup.op

export Semigroup (sconcat stimes)

class LawfulSemigroup (α : Type u) extends Semigroup α where
  op_assoc : ∀{a b c : α}, a <> b <> c = a <> (b <> c)

instance : Semigroup Unit where
  op _ _ := ()
  sconcat _ _:= ()
  stimes _ _ _ := ()

instance : Semigroup (List α) where
  op := (· ++ ·)
  sconcat as _ := List.flatten as
  stimes n as _ := List.replicate n as |>.flatten

instance [Semigroup α] [Semigroup β] : Semigroup (α × β) where
  op := op
  sconcat
  | [], h => nomatch h
  | x :: xs, _ => xs.foldl op x
  stimes n
  | (a, b), h => (stimes n a h, stimes n b h)
 where
  op : α × β -> α × β -> α × β
  | (a, b), (a', b') => (a <> a', b <> b')

instance : Semigroup Ordering where
  op := op

  sconcat
  | [], h => nomatch h
  | x :: xs, _ => xs.foldl op x

  stimes n x h :=
    match compare n 0 with
    | .lt => unreachable!
    | .eq => .eq
    | .gt => x
 where
  op : Ordering -> Ordering -> Ordering
  | .lt, _ => .lt
  | .eq, y => y
  | .gt, _ => .gt

instance [Semigroup α] : Semigroup (Option α) where
  op := op

  sconcat
  | [], h => nomatch h
  | x :: xs, _ => xs.foldl op x

  stimes n
  | .none, _ => .none
  | .some x, h =>
    match compare n 0 with
    | .lt => unreachable!
    | .eq => .none
    | .gt => .some $ stimes n x h
 where
  op : Option α -> Option α -> Option α
  | .none, y => y
  | x, .none => x
  | .some x, .some y => .some $ x <> y

instance {α : Type} [Semigroup α] : Semigroup (IO α) where
  op := op

  sconcat
  | [], h => nomatch h
  | x :: xs, _ => xs.foldl op x

  -- FIXME: native
  stimes n action _ := Id.run do
    let mut result := action
    for i in [0:n] do
      result := op result action
    return result
 where
  op (a : IO α) (b : IO α) : IO α := (· <> ·) <$> a <*> b


