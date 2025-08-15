import Printf

import LSpec.Prelude
import LSpec.Core.Path
import LSpec.Core.Clock
import LSpec.Core.Format
import LSpec.Core.Formatters.V2.Monad
import LSpec.Core.Formatters.Diff

namespace LSpec.Core.Formatters

open LSpec.Core.Clock (Seconds timeout)

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

instance : Repr Formatter where
  reprPrec _ _ := "V2.Formatter"

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
      formatter.itemDone path item
    | .Done _ => formatter.done
 where
  addFailure (failure : Failure) :=
    modify λstate => ({ state with failMessages := state.failMessages.concat failure } : FormatterState)

def silent : Formatter := {
  started := pass'
  groupStarted := λ_ => pass'
  groupDone := λ_ => pass'
  progress := λ_ _ => pass'
  itemStarted := λ_ => pass'
  itemDone := λ_ _ => pass'
  done := pass'
}

def checks : Formatter :=
  {
    silent with
    progress := λ(nesting, requirement) progress => do
      writeTransient s!"{indentationFor nesting}{requirement} [{formatProgress progress}]"
    itemStarted := λ(nesting, requirement) => do
      writeTransient $ s!"{indentationFor nesting}{requirement} [ ]"
    itemDone := λ(nesting, requirement) item => do
      let unicode <- outputUnicode
      let fallback := λa b => if unicode then a else b
      writeResult nesting requirement item.duration item.info |>.uncurry $
        match item.result with
        | .Success .. => (withSuccessColor, fallback "✔" "v")
        | .Pending .. => (withPendingColor, fallback "‐" "-")
        | .Failure .. => (withFailColor, fallback "‐" "-")
      match item.result with
      | .Success .. => pass'
      | .Failure .. => pass'
      | .Pending _ reason? =>
        withPendingColor $ indentBy (indentationFor ("" :: nesting)) $ "# PENDING: " ++ reason?.getD "No reason given"
  }
 where
  indentationFor (nesting : List String) := "".intercalate $ List.replicate (nesting.length * 2) " "

  writeResult (nesting : List String) (requirement : String) (duration : Seconds) (info : String) (withColor : FormatM Unit -> FormatM Unit) (symbol : String) : FormatM Unit := do
    let shouldPrintTimes <- printTimes
    let dt := duration.toMilliseconds.floor
    let times :=
      if not shouldPrintTimes || dt == 0 then
        ""
      else
        s!" ({dt}ms)"
    write $ indentationFor nesting ++ requirement ++ " ["
    withColor $ write symbol
    writeLine $ "]" ++ if shouldPrintTimes then times else ""
    indentBy (indentationFor ("" :: nesting)) info

  formatProgress
  | (current, total) =>
    if total == 0 then
      s!"{current}"
    else
      s!"{current}/{total}"

set_option linter.unusedVariables false

def indentation : String := "       "

def formatOmittedLines (n : Nat) : String :=
  s!"@@ {n} lines omitted @@"

inductive ColorChunk where
| plain : String -> ColorChunk
| color : String -> ColorChunk
deriving Repr, BEq, Inhabited, TypeName

inductive Chunk where
| original : String -> Chunk
| modified : String -> Chunk
| info : String -> Chunk
| modifiedChunks : List ColorChunk -> Chunk
deriving Repr, BEq, Inhabited, TypeName

def expectedChunks : List LineDiff -> List Chunk :=
  List.flatMap λ
  | .both lines => lines.map .original
  | .first lines => lines.map .modified
  | .second _ => []
  | .omitted n => [.info $ formatOmittedLines n]
  | .singleLineDiff diffs =>
    pure $ .modifiedChunks $ diffs.filterMap λ
      | .first _ => .none
      | .second chunk => .some $ .color chunk
      | .both chunk => .some $ .plain chunk

def actualChunks : List LineDiff -> List Chunk :=
  List.flatMap λ
  | .both lines => lines.map .original
  | .first _ => []
  | .second lines => lines.map .modified
  | .omitted n => [.info $ formatOmittedLines n]
  | .singleLineDiff diffs =>
    pure $ .modifiedChunks $ diffs.filterMap λ
      | .first chunk => .some $ .color chunk
      | .second _ => .none
      | .both chunk => .some $ .plain chunk

def writeChunks (pre : String) (chunks : List Chunk) (colorize : String -> FormatM Unit) : FormatM Unit := do
  withFailColor $ write (indentation ++ pre)
  go pass' chunks
 where
  replicate (n : Nat) : Char -> String := List.asString ∘ List.replicate n
  indentation_ : String := indentation ++ replicate pre.length ' '

  go (indent_ : FormatM Unit) : (chunks : List Chunk) -> FormatM Unit
  | [] => pass'
  | chunk :: chunks => do
    indent_
    match chunk with
    | .original a => write a
    | .modified a => colorize a
    | .info text => withInfoColor $ write text
    | .modifiedChunks chunks' =>
      chunks'.forM λ
      | .plain a => write a
      | .color a => colorize a
    write "\n"
    go (write indentation_) chunks

def writeDiff (chunks : List LineDiff) (extra : String -> FormatM Unit) (missing : String -> FormatM Unit) : FormatM Unit := do
  writeChunks "expected: " (expectedChunks chunks) extra
  writeChunks " but got: " (actualChunks chunks) extra

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

  evaluate := pure

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
    | .ExpectedButGot preface? expected_ actual_ => do
      let pretty <- prettyPrintFunction
      let (expected, actual) :=
        match pretty with
        | .none => (expected_, actual_)
        | .some f => f expected_ actual_
      preface?.forM indent

      let threshold : Clock.Seconds := 2
      match <- externalDiff? with
      | .some externalDiff => do
        externalDiff expected actual
      | .none => do
        let chunks? <-
          if <- useDiff then
            timeout threshold $ evaluate $ lineDiff (<- diffContext?) expected actual
          else
            pure .none
        match chunks? with
        | .some chunks => do
          writeDiff chunks extraChunk missingChunk
        | .none => do
          writeDiff [.first (splitLines expected), .second (splitLines actual)] write write

    | .Canceled => withFailColor $ indent "canceled"
    | .Error info e => do
      info.forM indent
      let formatException <- getConfigValue Format.Config.formatException
      withFailColor ∘ indent $ s!"uncaught exception: {e}"

    -- FIXME:
    -- unlessExpert $ do

def pluralize : (n : Nat) -> (s : String) -> String
| 1, s => s!"1 {s}"
| n, s => s!"{n} {s}s"

def defaultFooter : FormatM Unit := do
  writeLine =<< (· ++ ·)
    <$> ((λseconds => printf "Finished in %1.4f seconds" (seconds : Float)) <$> getRealTime)
    <*> pure ((<- getCPUTime?).elim "" (printf ", used %1.4f seconds of CPU time" ·))

  let fails <- getFailCount
  let pending <- getPendingCount
  let total <- getTotalCount

  let output := ", ".intercalate $ [
    pluralize total "example",
    pluralize fails "failure",
  ] ++ [
    Option.some pending
      |>.filter (· > 0)
      |>.map (s!"{·} pending")
  ].reduceOption

  let color :=
    if fails > 0 then
      withFailColor
    else if pending > 0 then
      withPendingColor
    else
      withSuccessColor

  color $ writeLine output

set_option linter.unusedVariables true

def specdoc : Formatter :=
  {
    silent with
    started := do
      -- withDebugColor $ writeLine "V2.specdoc.started"
      writeLine ""
    groupStarted := λ(nesting, name) => do
      -- withDebugColor $ writeLine "V2.specdoc.groupStarted"
      writeLine $ indentationFor nesting ++ name
    progress := λ_ progress => do
      -- withDebugColor $ writeLine "V2.specdoc.progress"
      writeTransient $ formatProgress progress
    itemDone := λ(nesting, requirement) item => do
      -- withDebugColor $ writeLine "V2.specdoc.itemDone"
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
          writeResult nesting (requirement ++ s!" FAILED [{n}]") duration info
    done := do
      -- withDebugColor $ writeLine "specdoc.done"
      defaultFailedFormatter *> defaultFooter
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
  done := do
    -- withDebugColor $ writeLine "V2.failed_examples.done"
    defaultFailedFormatter *> defaultFooter
}

def progress : Formatter := {
  failed_examples with
  itemDone := λ_ item => do
    -- withDebugColor $ writeLine "V2.progress.itemDone"
    match item.result with
    | .Success => withSuccessColor $ write "."
    | .Pending _ _ => withPendingColor $ write "."
    | .Failure _ _ => withFailColor $ write "F"
}
