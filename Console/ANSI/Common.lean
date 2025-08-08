namespace Console.ANSI

inductive Color where
| Black : Color
| Red : Color
| Green : Color
| Yellow : Color
| Blue : Color
| Magenta : Color
| Cyan : Color
| White : Color
deriving Repr

inductive Color.Intensity where
| Dull : Color.Intensity
| Vivid : Color.Intensity
deriving Repr

inductive Layer where
| Foreground : Layer
| Background : Layer
deriving Repr

inductive BlinkSpeed where
/-- Less than 150 blinks per minute -/
| Slow : BlinkSpeed
/-- More than 150 blinks per minute -/
| Rapid : BlinkSpeed
| None : BlinkSpeed
deriving Repr

inductive Underlining where
| Single : Underlining
/-- Not widely supported -/
| Double : Underlining
| None : Underlining
deriving Repr

inductive ConsoleIntensity where
| Bold : ConsoleIntensity
| Faint : ConsoleIntensity
| Normal : ConsoleIntensity
deriving Repr

inductive SGR where
| Reset : SGR
| SetConsoleIntensity : ConsoleIntensity -> SGR
/-- Not widely supported: sometimes treated as swapping foreground and background -/
| SetItalicized : Bool -> SGR
| SetUnderlining : Underlining -> SGR
| SetBlinkSpeed : BlinkSpeed -> SGR
/-- Not widely supported -/
| SetVisible : Bool -> SGR
| SetSwapForegroundBackground : Bool -> SGR
| SetColor : Layer -> Color.Intensity -> Color -> SGR
deriving Repr
