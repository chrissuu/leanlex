import Leanlex.Regex.Core
def isSigmaStar : Regex α → Bool
  | .compl .empty => true
  | _ => false

def isEmpty : Regex α → Bool
  | .empty => true
  | _ => false

def flattenAlt : Regex α → List (Regex α)
  | .alt r s => flattenAlt r ++ flattenAlt s
  | r => [r]

def flattenInter : Regex α → List (Regex α)
  | .inter r s => flattenInter r ++ flattenInter s
  | r => [r]

def insertSortedDedup [Ord α] (r : Regex α)
  : List (Regex α) → List (Regex α)
  | [] => [r]
  | s::ss =>
    match compare r s with
    | .eq => s::ss
    | .lt => r::s::ss
    | .gt => s::(insertSortedDedup r ss)

def sortDedup [Ord α]
  : List (Regex α) → List (Regex α)
  | [] => []
  | r::rs => insertSortedDedup r (sortDedup rs)

def buildCommFromList (constr : Regex α → Regex α → Regex α)
  : List (Regex α) → Regex α
  | [] => .empty
  | [r] => r
  | r::rs => constr r (buildCommFromList constr rs)

def buildAltFromList : List (Regex α) → Regex α := buildCommFromList .alt
def buildInterFromList : List (Regex α) → Regex α := buildCommFromList .inter
