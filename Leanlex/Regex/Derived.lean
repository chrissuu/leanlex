import Leanlex.Regex.Core
import Leanlex.CharSet

namespace Regex
abbrev CharRegex := Regex CharSet

def wildcard : CharRegex := .atom CharSet.univ

def plus (r : CharRegex) : CharRegex :=
  .seq r (.star r)

def optional (r : CharRegex) : CharRegex :=
  .alt r .epsilon

def exactly (r : CharRegex) : Nat → CharRegex
  | 0 => .epsilon
  | n + 1 => .seq r (exactly r n)

def between (r : CharRegex) : Nat → Nat → CharRegex
  | 0, 0 => .epsilon
  | 0, m + 1 => .alt (exactly r (m + 1)) (between r 0 m)
  | _ + 1, 0 => .empty
  | n+1, m + 1 => .seq r (between r n m)

def atleastN (r : CharRegex) (n : Nat) : CharRegex :=
  .seq (exactly r n) (plus r)

end Regex
