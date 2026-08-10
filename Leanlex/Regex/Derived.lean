import Leanlex.Regex.Core

namespace Regex

def wildcard : CharRegex := .atom CharSet.univ

def plus (r : Regex α) : Regex α :=
  .seq r (.star r)

def optional (r : Regex α) : Regex α :=
  .alt r .epsilon

def exactly (r : Regex α) : Nat → Regex α
  | 0 => .epsilon
  | n + 1 => .seq r (exactly r n)

def between (r : Regex α) : Nat → Nat → Regex α
  | 0, 0 => .epsilon
  | 0, m + 1 => .alt (exactly r (m + 1)) (between r 0 m)
  | _ + 1, 0 => .empty
  | n+1, m + 1 => .seq r (between r n m)

def atLeast (r : Regex α) (n : Nat) : Regex α :=
  .seq (exactly r n) (.star r)

def atMost (r : Regex α) (n : Nat) : Regex α :=
  between r 0 n

def diff (r s : Regex α) : Regex α :=
  .inter r (.compl s)

def seqMany (rs : List (Regex α)) : Regex α :=
  rs.foldr .seq .epsilon

def altMany (rs : List (Regex α)) : Regex α :=
  rs.foldr .alt .empty

def zeroOrMore (r : Regex α) : Regex α := .star r
def oneOrMore  (r : Regex α) : Regex α := plus r
def zeroOrOne  (r : Regex α) : Regex α := optional r

def char (c : Char) : CharRegex := .atom (CharSet.singleton c)
def range (lo hi : Char) : CharRegex := .atom (CharSet.range lo hi)
def string (s : String) : CharRegex := seqMany (s.toList.map char)

end Regex
