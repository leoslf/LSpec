import LSpec
import LSpec.Core.Runner
import LSpec.Core.Format
import LSpec.Core.Formatters.V2

open LSpec.Core Formatters

namespace Test.LSpec.Core.Formatters.V2Spec

/- Fixture -/
def testSpec : Spec := do
  describe "Example" $ do
    it "success"      $ Example.Result.mk "" .Success
    it "fail 1"       $ Example.Result.mk "" $ .Failure .none $ .Reason "fail message"
    it "pending"      $ pendingWith "pending message"
    it "fail 2"       $ Example.Result.mk "" $ .Failure .none $ .NoReason
    it "exceptions"   $ (default /- FIXME: undefined -/ : Example.Result)
    it "fail 3"       $ Example.Result.mk "" $ .Failure .none $ .NoReason

def defaultConfig : Config := {
  (default : Config) with
  -- NOTE: we are testing the streams
  concurrentJobs? := .some 1
}

def formatConfig : Format.Config :=
  {
    (default : Format.Config) with
    outputUnicode := outputUnicode
    useDiff := true
    diffContext? := .some 3
    externalDiff? := .none
    prettyPrint := true
    prettyPrintFunction := .some $ defaultConfig.prettyPrintFunction outputUnicode
  }
 where
  outputUnicode := true

def captureLines [Monad m] [MonadLift IO m] [MonadLift BaseIO m] [MonadFinally m] (action : m a) : m (List String) :=
  String.lines <$> IO.capture' IO.withStdout action

def runSpecWith (formatter : V2.Formatter) (spec : Spec) : ArgsT IO (List String) := do
  captureLines $ do
    lspecWithResult config spec
 where
  config := {
    defaultConfig with
    format? := .some formatter.toFormat
  }

def spec : Spec := do
  -- FIXME: https://github.com/leanprover/lean4/issues/426
  -- describe "progress" do
  --   let path : Path := ([], "")
  --   let item : Example.Result.Status -> Format.Event := .ItemDone path ∘ Format.Item.mk .none 0 ""
  --   context "itemDone" do
  --     it "marks succeeding examples with '.'" do
  --       let output <- IO.capture' IO.withStdout $ do
  --         let stdout <- IO.getStdout
  --         let ref <- IO.mkRef stdout
  --         let formatter <- V2.progress.toFormat { formatConfig with stream? := .some ref }
  --         formatter $ item .Success
  --       output <shouldBe> "."

  pure ()
