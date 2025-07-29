def String.words : String -> List String :=
  String.splitOn (sep := " ")

def String.reverse : String -> String :=
  List.asString ∘ List.reverse ∘ String.toList

inductive Quoting where
| None : Quoting
| Single : Quoting
| Double : Quoting

def defaultIFS := " \t\n"

def String.unescape (args : String) (ifs : String := defaultIFS) : List String :=
  go args.toList .None false [] [] |>.map (String.reverse) |>.reverse
 where
  go : List Char -> Quoting -> Bool -> List Char -> List String -> List String
  -- case 1: end of input
  | [], _, _, a, as => a.asString :: as
  -- case 2: backslash escape in progress
  | c :: cs, q, true, a, as => go cs q false (c :: a) as
  | c :: cs, q, false, a, as =>
    match (q, c) with
    -- case 3: no backslash escape in progress, but got a backslash
    | (_,       '\\') => go cs q true a as
    -- case 4: single-quote escaping in progress
    | (.Single, '\'') => go cs .None false a as
    | (.Single, c)    => go cs .Single false (c :: a) as
    -- case 5: double-quote escaping in progress
    | (.Double, '"')  => go cs .None false a as
    | (.Double, c)    => go cs .Double false (c :: a) as
    -- case 6: no escaping is in progress
    | (.None,   '\'') => go cs .Single false [] (a.asString :: as)
    | (.None,   '"')  => go cs .Double false [] (a.asString :: as)
    | (.None,   c)    =>
      if ifs.contains c then
        go cs .None false [] (a.asString :: as)
      else
        go cs .None false (c :: a) as

/-- Given a string of concatenated strings, separated each by removing a layer of quoting and/or escaping of certain characters -/
def String.unescapeArgs (args : String) (ifs : String := defaultIFS) : List String :=
  args.unescape (ifs := ifs) |>.filter (!·.isEmpty)

export String (words unescape unescapeArgs)
