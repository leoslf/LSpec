universe u v

variable {m : Type u -> Type v} [Monad m]

set_option diagnostics true

deriving instance TypeName for Nat, Int, UInt8, UInt16, UInt32, UInt64, Int8, Int16, Int32, Int64, Bool, Float32, Float, String, IO.Error

def parenthesize (type : String) : String :=
  if type.contains ' ' then
    s!"({type})"
  else
    type

section PUnit
unsafe def instTypeNamePUnitImpl : TypeName PUnit := .mk _ $ Lean.Name.mkSimple s!"PUnit"
@[implemented_by instTypeNamePUnitImpl] opaque instTypeNamePUnit : TypeName PUnit
instance : TypeName PUnit  := instTypeNamePUnit
end PUnit

#synth TypeName PUnit
#synth TypeName Unit

section IO
unsafe def instTypeNameIOImpl [TypeName a] : TypeName (IO a) := .mk _ $ Lean.Name.mkSimple s!"IO {parenthesize $ reprStr $ TypeName.typeName a}"
@[implemented_by instTypeNameIOImpl] opaque instTypeNameIO [TypeName a] : TypeName (IO a)
instance [TypeName a] : TypeName (IO a)  := instTypeNameIO
end IO

section BaseIO
unsafe def instTypeNameBaseIOImpl [TypeName a] : TypeName (BaseIO a) := .mk _ $ Lean.Name.mkSimple s!"BaseIO {parenthesize $ reprStr $ TypeName.typeName a}"
@[implemented_by instTypeNameBaseIOImpl] opaque instTypeNameBaseIO [TypeName a] : TypeName (BaseIO a)
instance [TypeName a] : TypeName (BaseIO a)  := instTypeNameBaseIO
end BaseIO

section Option
unsafe def instTypeNameOptionImpl [TypeName a] : TypeName (Option a) := .mk _ $ Lean.Name.mkSimple s!"Option {parenthesize $ reprStr $ TypeName.typeName a}"
@[implemented_by instTypeNameOptionImpl] opaque instTypeNameOption [TypeName a] : TypeName (Option a)
instance [TypeName a] : TypeName (Option a)  := instTypeNameOption
end Option

section Prod
unsafe def instTypeNameProdImpl [TypeName a] [TypeName b] : TypeName (Prod a b) := .mk _ $ Lean.Name.mkSimple s!"{parenthesize $ reprStr $ TypeName.typeName a} × {parenthesize $ reprStr $ TypeName.typeName b}"
@[implemented_by instTypeNameProdImpl] opaque instTypeNameProd [TypeName a] [TypeName b] : TypeName (Prod a b)
instance [TypeName a] [TypeName b] : TypeName (Prod a b)  := instTypeNameProd
end Prod

section List
unsafe def instTypeNameListImpl [TypeName a] : TypeName (List a) := .mk _ $ Lean.Name.mkSimple s!"List {parenthesize $ reprStr $ TypeName.typeName a}"
@[implemented_by instTypeNameListImpl] opaque instTypeNameList [TypeName a] : TypeName (List a)
instance [TypeName a] : TypeName (List a)  := instTypeNameList
end List

section Function
unsafe def instTypeNameFunctionImpl [TypeName a] [TypeName b] : TypeName (a -> b) := .mk _ $ Lean.Name.mkSimple s!"{parenthesize $ reprStr $ TypeName.typeName a} → {parenthesize $ reprStr $ TypeName.typeName b}"
@[implemented_by instTypeNameFunctionImpl] opaque instTypeNameFunction [TypeName a] [TypeName b] : TypeName (a -> b)
instance [TypeName a] [TypeName b] : TypeName (a -> b)  := instTypeNameFunction
end Function

instance [TypeName a] : TypeName (Option a) := inferInstance

instance [TypeName a] [TypeName b] : TypeName (a × b) := inferInstance

instance [TypeName a] [TypeName b] : TypeName (a -> b) := inferInstance

instance (priority := low) {a : Type u} {b : Type v} [TypeName a] [TypeName b] : Repr (a -> b) where
  reprPrec _ _ := reprStr (TypeName.typeName a) ++ " -> " ++ reprStr (TypeName.typeName b)

-- instance (priority := low) [Repr a] : ToString a where
--   toString := reprStr

instance [Repr a] [Repr b] : Repr (Prod a b) where
  reprPrec
  | (a, b), prec =>
    "(" ++ reprPrec a prec ++ ", " ++ reprPrec b prec ++ ")"

instance [Repr a] [Repr b] : ToString (Prod a b) where
  toString := reprStr
