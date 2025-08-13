import Lake

open Lake DSL

package LSpec where
  version := v!"2.1.0"
  testDriver := "lspec_tests"

lean_lib LSpec

lean_exe "lspec-discover" where
  root := `LSpec.Discover.Main
  needs := #[LSpec]

require "leanprover-community" / "batteries" @ git "v4.22.0-rc4"

require "Cli" from git
  "https://github.com/leoslf/lean4-cli" @ "main"

require "Printf" from git
  "https://github.com/leoslf/printf.lean" @ "master"

require "Free" from git
  "https://github.com/leoslf/free.lean" @ "master"

require "Concurrency" from git
  "https://github.com/leoslf/concurrency.lean" @ "master"

target TestMain pkg : System.FilePath := do
  -- NOTE: make sure to .gitignore the file
  let output := pkg.dir / "Test" / "Main.lean"
  liftM do
    IO.FS.writeFile output =<< IO.Process.run {
      cmd := "lake"
      args := #["exe", "lspec-discover"]
      inheritEnv := true
    }
  return pure output

lean_lib Test

@[default_target]
lean_exe lspec_tests where
  root := `Test.Main
  needs := #[TestMain]
