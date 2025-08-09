import LSpec.Prelude
import LSpec.Core.Path
import LSpec.Core.Format

namespace LSpec.Core.Runner

open LSpec.Core

structure SlowItem where
  mk ::
  location? : Option Location
  path : Path
  duration : Float
deriving Repr

def toSlowItem : Path × Format.Item -> SlowItem
| (path, item) => SlowItem.mk item.location? path $ item.duration.toMilliseconds

def slowItems (n : Nat) : List SlowItem -> List SlowItem :=
  -- TODO
  id -- [] -- List.take n . List.reverse . sortOn SlowItem.duration . List.filter ((· != 0) ∘ SlowItem.duration)

def printSlowSpecItem (item : SlowItem) : IO Unit :=
  IO.eprintln $ s!"  " ++ item.location?.elim "" Location.format ++ item.path.join ++ s!" ({item.duration}ms)"

def printSlowSpecItems : Nat -> Format -> Format
| n, format, event => do
  format event
  match event with
  | .Done items => do
    let xs := slowItems n $ items.map toSlowItem
    unless xs.isEmpty do
      IO.eprintln "\nSlow spec items:"
      xs.forM printSlowSpecItem
  | _ => pure ()
