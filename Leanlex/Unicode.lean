namespace Unicode

def minChar : Char :=
  Char.ofNat 0x0000

def beforeSurrogate : Char :=
  Char.ofNat 0xD7FF

def afterSurrogate : Char :=
  Char.ofNat 0xE000

def maxChar : Char :=
  Char.ofNat 0x10FFFF

end Unicode
