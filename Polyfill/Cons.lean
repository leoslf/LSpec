-- class Cons t where
--   Token : Type
--   nil : t
--   cons : Token -> t -> t

-- @[default_instance]
-- instance (priority := high) : Cons (List a) where
--   Token := a
--   nil := []
--   cons := List.cons

instance : Coe (List Char) String where
  coe := List.asString

instance : Coe String (List Char) where
  coe := String.toList

-- inductive ConsableString where
-- | nil : ConsableString
-- | cons (head : Char) (tail : ConsableString) : ConsableString
--
-- instance : Coe String ConsableString where
--   coe
--   | ⟨[]⟩ => .nil
--   | ⟨c :: cs⟩ => .cons c cs
--
-- instance : Coe ConsableString String where
--   coe
--   | .nil => ""
--   | .cons c cs  => String.mk $ c :: cs.toList
--
-- instance (priority := low) : Cons ConsableString where
--   Token := Char
--   nil := ConsableString.nil
--   cons := ConsableString.cons
--
-- infixr:67 " ::: " => ConsableString.cons
