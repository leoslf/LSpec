import Polyfill.String
import Console.ANSI.Common

namespace Console.ANSI

def csi (args : List Nat) (code : String) : String :=
  "\x1b[" ++ (";".intercalate $ args.map toString) ++ code

def Color.toCode : Color -> Nat
| Black   => 0
| Red     => 1
| Green   => 2
| Yellow  => 3
| Blue    => 4
| Magenta => 5
| Cyan    => 6
| White   => 7

def ConsoleIntensity.toCode : ConsoleIntensity -> Nat
| .Bold => 1
| .Faint => 2
| .Normal => 22

def Underlining.toCode : Underlining -> Nat
| .Single => 4
| .Double => 21
| .None => 24

def BlinkSpeed.toCode : BlinkSpeed -> Nat
| .Slow => 5
| .Rapid => 6
| .None => 25

def SGR.toCode : SGR -> Nat
| .Reset => 0
| .SetConsoleIntensity intensity => intensity.toCode
| .SetItalicized true => 3
| .SetItalicized false => 23
| .SetUnderlining underlining => underlining.toCode
| .SetBlinkSpeed speed => speed.toCode
| .SetVisible false => 8
| .SetVisible true => 28
| .SetSwapForegroundBackground true => 7
| .SetSwapForegroundBackground false => 27
| .SetColor .Foreground .Dull color => 30 + color.toCode
| .SetColor .Foreground .Vivid color => 90 + color.toCode
| .SetColor .Background .Dull color => 40 + color.toCode
| .SetColor .Background .Vivid color => 100 + color.toCode

def cursorUpCode (n : Nat) := csi [n] "A"
def cursorDownCode (n : Nat) := csi [n] "B"
def cursorForwardCode (n : Nat) := csi [n] "C"
def cursorBackwardCode (n : Nat) := csi [n] "D"
def cursorDownLineCode (n : Nat) := csi [n] "E"
def cursorUpLineCode (n : Nat) := csi [n] "F"
def setCursorColumnCode (n : Nat) := csi [n + 1] "G"
def setCursorPositionCode (n : Nat) (m : Nat) := csi [n + 1, m + 1] "H"

def clearFromCursorToScreenEndCode := csi [0] "J"
def clearFromCursorToScreenBeginningCode := csi [1] "J"
def clearScreenCode := csi [2] "J"

def clearFromCursorToLineEndCode := csi [0] "K"
def clearFromCursorToLineBeginningCode := csi [1] "K"
def clearLineCode := csi [2] "K"

def scrollPageUpCode (n : Nat) := csi [n] "S"
def scrollPageDownCode (n : Nat) := csi [n] "T"

def setSGRCode (sgrs : List SGR) := csi (sgrs.map SGR.toCode) "m"

def hideCursorCode := csi [] "?25l"
def showCursorCode := csi [] "?25h"

def setTitleCode (title : String) :=
  s!"\x1b]0;" ++ title.filter (· != '\x07') ++ "\x07"

def disableLineWrappingCode := csi [] "?7l"
def enableLineWrappingCode := csi [] "?7h"

end Console.ANSI

namespace IO.FS.Stream

open Console.ANSI

def setSGR (sgrs : List SGR) (stream : Stream) : IO Unit :=
  stream.putStr $ setSGRCode sgrs

def hideCursor (stream : Stream) : IO Unit :=
  stream.putStr hideCursorCode

def showCursor (stream : Stream) : IO Unit :=
  stream.putStr showCursorCode

end IO.FS.Stream

def String.stripAnsi : String -> String :=
  List.asString ∘ go ∘ String.toList
 where
  -- TODO
  -- FIXME
  go := id

