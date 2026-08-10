import Leanlex.Unicode
open Unicode

structure Range (α : Type) where
  lo : α
  hi : α

abbrev CharRange := Range Char

namespace CharRange

def point (c : Char) : CharRange := ⟨c, c⟩

end CharRange

structure CharSet where
  ranges : List CharRange

namespace CharSet

def singleton (c : Char) : CharSet :=
  { ranges := [.point c] }

def range (lo hi : Char) : CharSet :=
  if hi < lo
  then { ranges := [] }
  else { ranges := [⟨lo, hi⟩] }

def univ : CharSet :=
  { ranges := [
    ⟨minChar, beforeSurrogate⟩,
    ⟨afterSurrogate, maxChar⟩
  ]}

end CharSet
