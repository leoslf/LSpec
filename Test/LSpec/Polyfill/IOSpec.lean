import LSpec

open LSpec.Core

namespace Test.LSpec.Polyfill.IOSpec

def spec : Spec := do
  describe "capture'" do
    it "should capture whatever goes to the stream" do
      let message := "Hello World"
      IO.capture' IO.withStdout (IO.print message) <shouldReturn> message

