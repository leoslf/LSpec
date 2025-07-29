import Lean

import LSpec.Prelude
import LSpec.Core.Spec

open Lean Parser Elab Command Term Meta

def parse (input : String) (fileName : String := "<generated>"): CommandElabM (Array Syntax) := do
  -- Create input context from raw code
  let inputCtx := Parser.mkInputContext input fileName

  -- Setup parser context
  let options <- getOptions
  let env <- getEnv
  let mut (_, state, messages) <- parseHeader inputCtx
  let pmctx : ParserModuleContext := { env, options }

  -- let mut messages := MessageLog.empty

  let mut cmds := #[]

  while state.pos != input.endPos do
    let (cmd, state', messages') := parseCommand inputCtx pmctx state messages
    -- if messages'.hasErrors then
    --   messages'.forM λmessage => do
    --     throwError message.data
    cmds := cmds.push cmd
    state := state'
  return cmds

elab "#lspec" "discover" : command => do
  let context <- read
  let generated <- IO.Process.run {
    cmd := "lake",
    args := #["exe", "lspec-discover", context.fileName]
  }
  logInfo s!"generated: {generated}"
  let cmds <- parse generated
  for cmd in cmds do
    logInfo s!"{cmd}"
    elabCommandTopLevel cmd
