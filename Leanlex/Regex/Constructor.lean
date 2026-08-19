import Leanlex.Regex.Core
import Leanlex.Regex.Canonicalization
/-
Smart Constructors

See: https://www.cambridge.org/core/services/aop-cambridge-core/content/view/E5734B86DEB96C61C69E5CF3C4FB0AFA/S0956796808007090a.pdf/regular-expression-derivatives-re-examined.pdf
-/

def Regex.mkInter [Ord α] (u v : Regex α) : Regex α :=
  let terms := flattenInter u ++ flattenInter v
  if terms.any isEmpty then .empty
  else
    terms
    |>.filter (λ r => !isSigmaStar r)
    |> sortDedup
    |> buildInterFromList


def Regex.mkAlt [Ord α] (u v : Regex α) : Regex α :=
  let terms := flattenAlt u ++ flattenAlt v
  if terms.any isSigmaStar then .compl .empty
  else
    terms
    |>.filter (λ r => !isEmpty r)
    |> sortDedup
    |> buildAltFromList

def Regex.mkSeq  (u v : Regex α) : Regex α :=
  match u, v with
  | .seq r s, t => mkSeq r (mkSeq s t)
  | .empty, _ => .empty
  | _, .empty => .empty
  | .epsilon, _ => v
  | _, .epsilon => u
  | _, _ => .seq u v

def Regex.mkStar : Regex α → Regex α
  | .star r => mkStar r
  | .epsilon => .epsilon
  | .empty => .epsilon
  | r => .star r

def Regex.mkCompl : Regex α → Regex α
  | .compl r => r
  | r => .compl r
