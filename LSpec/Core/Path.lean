import Batteries.Data.String.Matcher

namespace LSpec.Core

/-- At Path describes the location of a spec item within a spec tree. -/
abbrev Path := List String × String

namespace Path

def join : Path -> String
| (groups, requirement) => "/" ++ "/".intercalate (groups.concat requirement) ++ "/"

def formatRequirement : Path -> String
| (groups, requirement) =>
  let groups' :=
    match groups.span (¬·.any (· == ' ')) with
    | ([], ys) => join ys
    | (xs, ys) => join $ ".".intercalate xs :: ys
  groups' ++ requirement
 where
  join (xs : List String) :=
    match xs with
    | x :: [] => x ++ " "
    | ys => "".intercalate $ ys.map (· ++ ", ")

def filterPredicate (pattern : String) (path : Path) : Bool :=
  (path.join.findSubstr? pattern <|> path.formatRequirement.findSubstr? pattern) |>.isSome
