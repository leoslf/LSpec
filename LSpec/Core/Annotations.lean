import Std.Data.HashMap

namespace LSpec.Core

universe u

abbrev Annotations := Std.HashMap Lean.Name Dynamic

def Annotations.setValue {α : Type u} [TypeName α] (value : α) (annotations : Annotations) : Annotations :=
  let dynamic := Dynamic.mk value
  let type : Lean.Name := dynamic.typeName
  annotations.insert type dynamic

def Annotations.getValue {α : Type u} [TypeName α] (annotations : Annotations) : Option α := do
  let type : Lean.Name := TypeName.typeName α
  match annotations.get? type with
  | Option.none => Option.none
  | Option.some (dynamic : Dynamic) => Dynamic.get? α dynamic

instance : Repr Annotations where
  reprPrec annotations prec :=
    let annotations' := annotations.toList
      -- TODO: value
      |>.map λ(type, _) => type
    reprPrec annotations' prec
