import Std.Internal.Async.Basic
import LSpec.Console.ANSI

import LSpec.Core.Clock
import LSpec.Core.Format

open Std.Internal.IO.Async

namespace LSpec.Core.Formatters.V2

open LSpec.Core.Clock
open Console.ANSI

abbrev Failure.Reason := Example.Result.FailureReason

structure Failure where
  mk ::
  location? : Option Location
  path : Path
  message : Failure.Reason
deriving Repr

structure FormatterState where
  mk ::
  successCount : Nat := 0
  pendingCount : Nat := 0
  failMessages : List Failure := []
  cpuStartTime? : Option Nat := .none
  startTime : Clock.Seconds
  config : Format.Config
  color? : Option SGR := .none
deriving Repr, Inhabited

abbrev FormatM (a : Type) := ReaderT (IO.Ref FormatterState) IO a

instance : MonadLift BaseIO FormatM where
  monadLift := liftM (m := IO) (n := FormatM) ∘ liftM (m := BaseIO) (n := IO)

def withLineBuffering (action : IO a) : IO a := do
  -- FIXME: lean 4 currently doesn't support changing buffering mode
  action

def FormatM.run (config : Format.Config) (action : FormatM a) : IO a := withLineBuffering $ do
  let time <- Seconds.getMonotonicTime
  let cpuTime? <- do
    if config.reportProgress then
      -- FIXME: find out how to get CPU time
      -- .some <$> CPUTime.getCPUTime
      pure .none
    else
      pure .none
  let progress := config.reportProgress && not config.htmlOutput
  let state : FormatterState := {
    cpuStartTime? := cpuTime?
    startTime := time
    config := { config with reportProgress := progress }
    color? := .none
  }
  ReaderT.run action =<< IO.mkRef state

def gets (f : FormatterState -> a) : FormatM a := do
  f <$> ((·.get) =<< read)

def getConfig : FormatM Format.Config :=
  gets FormatterState.config

def getConfigValue (f : Format.Config -> a) : FormatM a :=
  f <$> getConfig

def getSuccessCount : FormatM Nat :=
  gets FormatterState.successCount

def getPendingCount : FormatM Nat :=
  gets FormatterState.pendingCount

def getFailMessages : FormatM (List Failure) :=
  List.reverse <$> gets FormatterState.failMessages

def getFailCount : FormatM Nat :=
  List.length <$> getFailMessages

def getTotalCount : FormatM Nat :=
  List.sum <$> [getSuccessCount, getPendingCount, getFailCount].traverse id

def useDiff : FormatM Bool :=
  getConfigValue Format.Config.useDiff

def getStream : FormatM (IO.FS.Stream) := do
  let ref? <- getConfigValue Format.Config.stream?
  ref?.elim IO.getStdout (·.get)

def unlessExpert (action : FormatM Unit) : FormatM Unit := do
  unless <- getConfigValue Format.Config.expertMode do
    action

def diffContext? : FormatM (Option DiffContext) := do
  getConfigValue Format.Config.diffContext?

def externalDiff? : FormatM (Option (String -> String -> IO Unit)) := do
  getConfigValue Format.Config.externalDiff?

-- | The random seed that is used for QuickCheck.
def usedSeed : FormatM Seed :=
  getConfigValue Format.Config.usedSeed

def prettyPrintFunction : FormatM (Option (String -> String -> String × String)) :=
  getConfigValue Format.Config.prettyPrintFunction

def outputUnicode : FormatM Bool :=
  getConfigValue Format.Config.outputUnicode

def modify (f : FormatterState -> FormatterState) : FormatM Unit := do
  (<- read).modify f

def increaseSuccessCount : FormatM Unit := do
  modify λstate => { state with successCount := state.successCount.succ }

def increasePendingCount : FormatM Unit := do
  modify λstate => { state with pendingCount := state.pendingCount.succ }

def splitLines : String -> List String :=
  String.groupBy (on (· == ·) (t := isNewline))
 where
  isNewline : Char -> Bool := (· == '\n')

def writeChunk (s : String) : FormatM Unit := do
  let stream <- getStream
  let plainOutput := stream.putStr s
  let colorOutput color := IO.bracket_ (stream.setSGR [color]) (stream.setSGR [SGR.Reset]) plainOutput
  match <- gets FormatterState.color? with
  | .some color =>
    let usePlain :=
      match color with
      | .SetColor .Foreground _ _ => s.all Char.isSpace
      | _ => false
    if usePlain then
      plainOutput
    else
      colorOutput color
  | .none => plainOutput

def write (s : String) : FormatM Unit :=
  splitLines s |>.forM writeChunk

def writeLine (s : String) : FormatM Unit :=
  write s *> write "\n"

def writeTransient (new : String) : FormatM Unit := do
  let stream <- getStream
  let withoutLineWrapping {a} : IO a -> IO a :=
    IO.bracket_
      (stream.putStr disableLineWrappingCode)
      (stream.putStr enableLineWrappingCode)
  let clearLine := stream.putStr $ "\r" ++ csi [] "K"

  if <- getConfigValue Format.Config.reportProgress then
    withoutLineWrapping $ stream.putStr new
    stream.flush
    clearLine

def getCPUTime? : FormatM (Option Seconds) := do
  -- let t1 <-
  -- FIXME: seems there's no non-internal way to get CPUTime
  pure .none

def getRealTime : FormatM Seconds := do
  let t1 <- Seconds.getMonotonicTime
  let t0 <- gets FormatterState.startTime
  -- dbgTraceM s!"t0: {t0}, t1: {t1}"
  -- dbgTraceM s!"real time: {reprPrec (t1 - t0) 10}"
  return t1 - t0

def printTimes : FormatM Bool :=
  getConfigValue Format.Config.printTimes

def htmlSpan (cls : String) (action : FormatM a) : FormatM a :=
  write s!"<span class=\"{cls}\">" *> action <* write "</span>"

def setColor (color? : Option SGR) : FormatM Unit := do
  if <- getConfigValue Format.Config.useColor then
    modify λstate => { state with color? }

def withColor_ (color : SGR) (action : FormatM a) : FormatM a := do
  let old? <- gets FormatterState.color?
  setColor (.some color) *> action <* setColor old?

def withColor (color : SGR) (cls : String) (action : FormatM a) : FormatM a := do
  let produceHTML <- getConfigValue Format.Config.htmlOutput
  (if produceHTML then htmlSpan cls else withColor_ color) action

def withSuccessColor : FormatM a -> FormatM a :=
  withColor (.SetColor .Foreground .Dull .Green) "lspec-success"

def withPendingColor : FormatM a -> FormatM a :=
  withColor (.SetColor .Foreground .Dull .Yellow) "lspec-pending"

def withInfoColor : FormatM a -> FormatM a :=
  withColor (.SetColor .Foreground .Dull .Cyan) "lspec-info"

def withFailColor : FormatM a -> FormatM a :=
  withColor (.SetColor .Foreground .Dull .Red) "lspec-failure"

def withDebugColor : FormatM a -> FormatM a :=
  withColor (.SetColor .Background .Vivid .Blue) "lspec.debug"

def indentBy (indentation : String) (message : String) : FormatM Unit := do
  message.lines.forM λline => do
    writeLine $ indentation ++ line

def diffColorize (color : Color) (cls : String) (s : String) : FormatM Unit :=
  withColor (.SetColor layer .Dull color) cls $
    match s with
    | "" => write eraseInLine
    | _ => write s
 where
  eraseInLine := csi [] "K"

  layer : Layer :=
    if s.all Char.isSpace then
      .Background
    else
      .Foreground

/-- Diff: missing chunk colored in red -/
def extraChunk (s : String) : FormatM Unit := do
  match <- getConfigValue Format.Config.useDiff with
  | true => extra s
  | false => write s
 where
  extra : String -> FormatM Unit := diffColorize .Red "lspec-failure"

/-- Diff: extra chunk colored in green -/
def missingChunk (s : String) : FormatM Unit := do
  match <- getConfigValue Format.Config.useDiff with
  | true => missing s
  | false => write s
 where
  missing : String -> FormatM Unit := diffColorize .Green "lspec-success"
