import Printf

import LSpec.Core.Path
import LSpec.Core.Clock
import LSpec.Core.Format
import LSpec.Core.Formatters.V2.Monad

namespace LSpec.Core.Formatters

open LSpec.Core.Clock (Seconds)

def V2 := Unit

namespace V2

structure Formatter where
  mk ::
  /-- evaluated before a test run -/
  started : FormatM Unit
  /-- evaluated before each spec group -/
  groupStarted : Path -> FormatM Unit
  /-- evaluated after each spec group -/
  groupDone : Path -> FormatM Unit
  /-- used to notify the progress of the currently evaluated example -/
  progress : Path -> Example.Progress -> FormatM Unit
  /-- evaluated before each spec item -/
  itemStarted : Path -> FormatM Unit
  itemDone : Path -> Format.Item -> FormatM Unit
  done : FormatM Unit
deriving Repr

def Formatter.toFormat (formatter : Formatter) (config : Format.Config) : IO Format :=
  -- TODO:
  monadic (FormatM.run config) λ
    | .Started => formatter.started
    | .GroupStarted path => formatter.groupStarted path
    | .GroupDone path => formatter.groupDone path
    | .Progress path progress => formatter.progress path progress
    | .ItemStarted path => formatter.itemStarted path
    | .ItemDone path item => do
      match item.result with
      | .Success => increaseSuccessCount
      | .Pending _ _ => increasePendingCount
      | .Failure location? err => addFailure $ Failure.mk (location? <|> item.location?) path err
    | .Done _ => formatter.done
 where
  addFailure (failure : Failure) :=
    modify λstate => ({ state with failMessages := state.failMessages.concat failure } : FormatterState)

def silent : Formatter := {
  started := pass
  groupStarted := λ_ => pass
  groupDone := λ_ => pass
  progress := λ_ _ => pass
  itemStarted := λ_ => pass
  itemDone := λ_ _ => pass
  done := pass
}

def checks : Formatter :=
  -- let formatProgress
  -- | (current, total) =>
  --   if total == 0 then
  --     s!"{current}"
  --   else
  --     s!"{current}/{total}"
  -- let indentationFor nesting := "".pushn ' ' $ nesting.length * 2
  silent
  -- {
  --   silent with
  --   -- progress := λ(nesting, requirement) progress => do
  --   --   .writeTransient s!"{indentationFor nesting}{requirement} [{formatProgress progress}]"
  -- }

set_option linter.unusedVariables false

def defaultFailedFormatter : FormatM Unit := do
  writeLine ""

  let failures : List Failure <- getFailMessages
  if not failures.isEmpty then
    writeLine "Failures:"
    writeLine ""

    failures.zipIdx (n := 1) |>.forM λ(failure, i) => do
      formatFailure i failure
      writeLine ""

    writeLine s!"Randomized with seed {<- usedSeed}"
    writeLine ""
 where
  indentation := "       "
  indent := indentBy indentation

  formatFailure : Nat -> Failure -> FormatM Unit
  | n, { location?, path, message := reason } => do
    let unicode <- outputUnicode
    location?.forM λlocation => do
      withInfoColor $ writeLine $ s!" {location.format}"
    write $ s!" {n}) "
    writeLine $ path.formatRequirement
    match reason with
    | .NoReason => pure ()
    | .Reason err => withFailColor $ indent err
    | .ColorizedReason err => indent err
    | .ExpectedButGot preface expected_ actual_ => do
      let pretty <- prettyPrintFunction
      let (expected, actual) :=
        match pretty with
        | .none => (expected_, actual_)
        | .some f => f expected_ actual_
      preface.forM indent

      -- let b <- useDiff
      -- TODO
    | .Error info e => do
      info.forM indent
      let formatException <- getConfigValue Format.Config.formatException
      withFailColor ∘ indent $ s!"uncaught exception: {e}"

    -- unlessExpert $ do
    --   -- TODO

def defaultFooter : FormatM Unit := do
  writeLine =<< (· ++ ·)
    <$> (printf "Finished in %1.4f seconds" <$> getRealTime)
    <*> pure ((<- getCPUTime?).elim "" (printf ", used %1.4f seconds of CPU time" ·))

  let fails <- getFailCount
  let pending <- getPendingCount
  let total <- getTotalCount

  -- TODO
  pure ()

set_option linter.unusedVariables true

def specdoc : Formatter :=
  {
    silent with
    started := do
      writeLine ""
    groupStarted := λ(nesting, name) => do
      writeLine $ indentationFor nesting ++ name
    progress := λ_ progress => do
      writeTransient $ formatProgress progress
    itemDone := λ(nesting, requirement) item => do
      let duration := item.duration
      let info := item.info
      match item.result with
      | .Success =>
        withSuccessColor $ do
          writeResult nesting requirement duration info
      | .Pending _ reason? =>
        withPendingColor $ do
          writeResult nesting requirement duration info
          indentBy (indentationFor ("" :: nesting)) $ "# PENDING: " ++ reason?.getD "No reason given"
      | .Failure _ _ =>
        withFailColor $ do
          let n <- getFailCount
          writeResult nesting (requirement ++ " FAILED [{n}]") duration info
    done := defaultFailedFormatter *> defaultFooter
  }
 where
  indentationFor (nesting : List String) := "".intercalate $ List.replicate (nesting.length * 2) " "

  writeResult (nesting : List String) (requirement : String) (duration : Seconds) (info : String) : FormatM Unit := do
    let shouldPrintTimes <- printTimes
    let dt := duration.toMilliseconds.floor
    let times :=
      if not shouldPrintTimes || dt == 0 then
        ""
      else
        s!" ({dt}ms)"
    writeLine $ indentationFor nesting ++ requirement ++ times
    indentBy (indentationFor ("" :: nesting)) info


  formatProgress
  | (current, total) =>
    if total == 0 then
      s!"{current}"
    else
      s!"{current}/{total}"


def failed_examples : Formatter := {
  silent with
  done := defaultFailedFormatter *> defaultFooter
}

def progress : Formatter := {
  failed_examples with
  itemDone := λ_ item => do
    match item.result with
    | .Success => withSuccessColor $ write "."
    | .Pending _ _ => withPendingColor $ write "."
    | .Failure _ _ => withFailColor $ write "F"
    (<- IO.getStdout).flush
}
