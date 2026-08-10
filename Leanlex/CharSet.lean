import Leanlex.Unicode
open Unicode

structure Range (α : Type) where
  lo : α
  hi : α
deriving Repr, BEq

abbrev CharRange := Range Char

namespace CharRange

def point (c : Char) : CharRange := ⟨c, c⟩

end CharRange

structure CharSet where
  ranges : List CharRange
deriving Repr, BEq

namespace CharSet

def empty : CharSet :=
  { ranges := [] }

private def leChar (a b : Char) : Bool :=
  !(b < a)

private def maxChar' (a b : Char) : Char :=
  if a < b then b else a

private def minChar' (a b : Char) : Char :=
  if a < b then a else b

private def next (c : Char) : Option Char :=
  if c == maxChar then
    none
  else if c == beforeSurrogate then
    some afterSurrogate
  else
    some (Char.ofNat (c.toNat + 1))

private def prev (c : Char) : Option Char :=
  if c == minChar then
    none
  else if c == afterSurrogate then
    some beforeSurrogate
  else
    some (Char.ofNat (c.toNat - 1))

private def validRange (r : CharRange) : Bool :=
  leChar r.lo r.hi

private def rangeLt (a b : CharRange) : Bool :=
  a.lo < b.lo

private def insertRange (r : CharRange) : List CharRange → List CharRange
  | [] => [r]
  | x :: xs =>
      if rangeLt r x then
        r :: x :: xs
      else
        x :: insertRange r xs

private def sortRanges (rs : List CharRange) : List CharRange :=
  rs.foldr insertRange []

private def canMerge (a b : CharRange) : Bool :=
  match next a.hi with
  | none => true
  | some c => leChar b.lo c

private def mergeRange (a b : CharRange) : CharRange :=
  { lo := a.lo, hi := maxChar' a.hi b.hi }

private def normalizeSorted : List CharRange → List CharRange
  | [] => []
  | r :: rs =>
      if !validRange r then
        normalizeSorted rs
      else
        go r rs
where
  go (current : CharRange) : List CharRange → List CharRange
    | [] => [current]
    | r :: rs =>
        if !validRange r then
          go current rs
        else if canMerge current r then
          go (mergeRange current r) rs
        else
          current :: go r rs

def normalize (s : CharSet) : CharSet :=
  { ranges := normalizeSorted (sortRanges s.ranges) }

def singleton (c : Char) : CharSet :=
  { ranges := [.point c] }

def range (lo hi : Char) : CharSet :=
  if hi < lo
  then empty
  else { ranges := [⟨lo, hi⟩] }

def univ : CharSet :=
  { ranges := [
    ⟨minChar, beforeSurrogate⟩,
    ⟨afterSurrogate, maxChar⟩
  ]}

def ofList (cs : List Char) : CharSet :=
  normalize { ranges := cs.map CharRange.point }

def ofRanges (ranges : List CharRange) : CharSet :=
  normalize { ranges := ranges }

def union (a b : CharSet) : CharSet :=
  normalize { ranges := a.ranges ++ b.ranges }

private def interRange (a b : CharRange) : Option CharRange :=
  let lo := maxChar' a.lo b.lo
  let hi := minChar' a.hi b.hi
  if hi < lo then none else some ⟨lo, hi⟩

private def interRanges (a b : List CharRange) : List CharRange :=
  List.flatMap (fun ra =>
    b.filterMap fun rb => interRange ra rb
  ) a

def inter (a b : CharSet) : CharSet :=
  let a := normalize a
  let b := normalize b
  normalize { ranges := interRanges a.ranges b.ranges }

private def diffOne (a b : CharRange) : List CharRange :=
  match interRange a b with
  | none => [a]
  | some i =>
      let left :=
        match prev i.lo with
        | none => []
        | some hi =>
            if hi < a.lo then [] else [⟨a.lo, hi⟩]
      let right :=
        match next i.hi with
        | none => []
        | some lo =>
            if a.hi < lo then [] else [⟨lo, a.hi⟩]
      left ++ right

private def diffRange (parts : List CharRange) (b : CharRange) : List CharRange :=
  List.flatMap (fun a => diffOne a b) parts

def diff (a b : CharSet) : CharSet :=
  let a := normalize a
  let b := normalize b
  normalize { ranges := b.ranges.foldl diffRange a.ranges }

def compl (s : CharSet) : CharSet :=
  diff univ s

private def rangeContains (r : CharRange) (c : Char) : Bool :=
  leChar r.lo c && leChar c r.hi

def contains (s : CharSet) (c : Char) : Bool :=
  s.ranges.any fun r => rangeContains r c

def isEmpty (s : CharSet) : Bool :=
  (normalize s).ranges.isEmpty

end CharSet
