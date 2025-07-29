import Std.Data.HashMap

namespace LSpec.Core

abbrev Annotations := Std.HashMap Lean.Name Dynamic

namespace Annotations

def setValue [TypeName α] (value : α) (annotations : Annotations) : Annotations :=
  let dynamic := Dynamic.mk value
  let type : Lean.Name := dynamic.typeName
  annotations.insert type dynamic

def getValue [TypeName α] (annotations : Annotations) : Option α :=
  let type : Lean.Name := TypeName.typeName α
  do
    let dynamic <- annotations.get? type
    dynamic.get? α

end Annotations
