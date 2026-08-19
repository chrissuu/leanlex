import Leanlex.CharSet

/-- The core, minimal, regex type -/
inductive Regex (α : Type) where
  | empty
  | epsilon
  | atom  : α → Regex α
  | alt   : Regex α → Regex α → Regex α
  | inter : Regex α → Regex α → Regex α
  | compl : Regex α → Regex α
  | seq   : Regex α → Regex α → Regex α
  | star  : Regex α → Regex α
deriving BEq, Ord

abbrev CharRegex := Regex CharSet
