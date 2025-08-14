import Lean

open Lean Elab PrettyPrinter

namespace LSpec.Discover

-- def parse (input : String) (fileName : String := "<generated>"): CommandElabM (Array Syntax) := do
--   -- Create input context from raw code
--   let inputCtx := Parser.mkInputContext input fileName
--
--   -- Setup parser context
--   let options <- getOptions
--   let env <- getEnv
--   let mut (_, state, messages) <- parseHeader inputCtx
--   let pmctx : ParserModuleContext := { env, options }
--
--   -- let mut messages := MessageLog.empty
--
--   let mut cmds := #[]
--
--   while state.pos != input.endPos do
--     let (cmd, state', messages') := parseCommand inputCtx pmctx state messages
--     -- if messages'.hasErrors then
--     --   messages'.forM λmessage => do
--     --     throwError message.data
--     cmds := cmds.push cmd
--     state := state'
--   return cmds
--
-- -- elab "#lspec" "discover" : command => do
-- --   let context <- read
-- --   let generated <- IO.Process.run {
-- --     cmd := "lake",
-- --     args := #["exe", "lspec-discover", "--source", context.fileName]
-- --   }
-- --   logInfo s!"generated: {generated}"
-- --   let cmds <- parse generated
-- --   for cmd in cmds do
-- --     logInfo s!"{cmd}"
-- --     elabCommandTopLevel cmd
--
-- def parse (input : String) : CommandM Syntax := do
--   testParseModule
--

structure CommandSyntax where
  env : Environment
  currNamespace : Name := Name.anonymous
  openDecls : List OpenDecl := []
  stx : Syntax

structure Module' where
  env : Environment
  header : TSyntax `Lean.Parser.Module.header
  commands : Array CommandSyntax

def parseModule (input : String) (fileName : String) (opts : Options := {}) (trustLevel : UInt32 := 1024) : IO Module' := do
  let mainModuleName := Name.anonymous -- FIXME
  let inputCtx := Parser.mkInputContext input fileName
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← processHeader header opts messages inputCtx trustLevel
  let env := env.setMainModule mainModuleName
  let s ← IO.processCommands inputCtx parserState
    { Command.mkState env messages opts with infoState := { enabled := true } }

  let commands : Array CommandSyntax <- s.commandState.infoState.trees.toArray.mapM λ
    | InfoTree.context (.commandCtx { env, currNamespace, openDecls, .. }) (.node (Info.ofCommandInfo {stx, ..}) _) =>
      pure {env, currNamespace, openDecls, stx}
    | _ =>
      throw $ IO.userError "unknown InfoTree"

  return {
    env
    header
    commands
  }

structure Declaration where
  env : Environment
  currNamespace : Name
  stx : TSyntax `Lean.Parser.Command.declaration

partial def getDeclarations (currNamespace : Name) (stx : Syntax) : List Name := Id.run do
  if let `(command| $decl:declaration) := stx then
    if let `(command| $modifiers:declModifiers $defn:definition) := decl then
      return [currNamespace.mkStr defn.raw.getArgs[1]!.getArgs[0]!.getId.toString]
  return stx.getArgs.toList.flatMap (getDeclarations currNamespace)

def extractDeclarations (input : String) (fileName : String) : IO (List Name) := do
  let module <- parseModule input fileName
  return module.commands.toList.flatMap (λcommand => getDeclarations command.currNamespace command.stx)
