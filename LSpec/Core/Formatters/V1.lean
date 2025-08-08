import Console.ANSI

import LSpec.Core.Example
import LSpec.Core.Format
import LSpec.Core.Formatters.V1.Monad
import LSpec.Core.Formatters.V2

open LSpec.Core.Format

namespace LSpec.Core.Formatters

def V1 := Unit

namespace V1

variable {a : Type}

def Failure.Reason.unlift : V2.Failure.Reason -> Failure.Reason
| .NoReason => .NoReason
| .Reason reason => .Reason reason
| .ColorizedReason reason => .Reason reason.stripAnsi
| .ExpectedButGot preface expected actual => .ExpectedButGot preface expected actual
| .Error info e => .Error info e

def Failure.unlift (v2 : V2.Failure) : Failure := {
  location? := v2.location?
  path := v2.path
  message := Failure.Reason.unlift v2.message
}

structure Formatter where
  mk ::
  header : FormatM Unit
  exampleGroupStarted : List String -> String -> FormatM Unit
  exampleGroupDone : FormatM Unit
  exampleStarted : Path -> FormatM Unit
  exampleProgress : Path -> Example.Progress -> FormatM Unit
  exampleSucceeded : Path -> String -> FormatM Unit
  exampleFailed : Path -> String -> Failure.Reason -> FormatM Unit
  examplePending : Path -> String -> Option String -> FormatM Unit
  failed : FormatM Unit
  footer : FormatM Unit
deriving Inhabited, Repr

#check Formatter

def silent : Formatter := {
  header := pass
  exampleGroupStarted := λ_ _ => pass
  exampleGroupDone := pass
  exampleStarted := λ_ => pass
  exampleProgress := λ_ _ => pass
  exampleSucceeded := λ_ _ => pass
  exampleFailed := λ_ _ _ => pass
  examplePending := λ_ _ _ => pass
  failed := pass
  footer := pass
}

def Formatter.interpret (action : FormatM a) : V2.FormatM a :=
  action.interpretWith {
    getSuccessCount := V2.getSuccessCount
    getPendingCount := V2.getPendingCount
    getFailMessages := List.map Failure.unlift <$> V2.getFailMessages
    usedSeed := V2.usedSeed
    printTimes := V2.printTimes
    getCPUTime? := V2.getCPUTime?
    getRealTime := V2.getRealTime
    write := V2.write
    writeTransient := V2.writeTransient
    -- withFailColor := V2.withFailColor
    -- withSuccessColor := V2.withSuccessColor
    -- withPendingColor := V2.withPendingColor
    -- withInfoColor := V2.withInfoColor
    useDiff := V2.useDiff
    extraChunk := V2.extraChunk
    missingChunk := V2.missingChunk
    liftIO := liftM
  }

def Formatter.toV2 (self : Formatter) : V2.Formatter :=
  {
    started := interpret self.header
    groupStarted := interpret ∘ self.exampleGroupStarted.uncurry
    groupDone := interpret ∘ Function.const _ self.exampleGroupDone
    progress := λpath => interpret ∘ self.exampleProgress path
    itemStarted := interpret ∘ self.exampleStarted
    itemDone := λpath item => interpret $ do
      match item.result with
      | .Success => self.exampleSucceeded path item.info
      | .Pending _ reason => self.examplePending path item.info reason
      | .Failure _ reason => self.exampleFailed path item.info $ Failure.Reason.unlift reason
    done := interpret $ self.failed *> self.footer
  }

def Formatter.toFormat (self : Formatter) : Format.Config -> IO Format :=
  self.toV2.toFormat
