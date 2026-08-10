import Leanlex.Regex.Core

namespace Derivative

def nullable : CharRegex → Bool
  | .empty => false
  | .epsilon => true
  | .atom _ => false
  | .alt r s => nullable r || nullable s
  | .inter r s => nullable r && nullable s
  | .compl r => not (nullable r)
  | .seq r s => nullable r && nullable s
  | .star _ => true

def derivative (c : Char) : CharRegex → CharRegex
  | .empty => .empty
  | .epsilon => .empty
  | .atom s => if s.contains c then .epsilon else .empty
  | .alt r s => .alt (derivative c r) (derivative c s)
  | .inter r s => .inter (derivative c r) (derivative c s)
  | .compl r => .compl (derivative c r)
  | .seq r s =>
    if nullable r
    then .alt (derivative c r) (derivative c s)
    else derivative c r
  | .star r => .seq (derivative c r) (.star r)

end Derivative
