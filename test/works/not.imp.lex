Just
  [ Kfun
  , KId "factorial"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "total"
  , KBraceBra
  , KId "total"
  , KEquals
  , KNum 1
  , KSemi
  , Kwhile
  , KRoundBra
  , KId "n"
  , KOp ">"
  , KNum 1
  , KRoundKet
  , KBraceBra
  , KId "total"
  , KRoundBra
  , KOp "*"
  , KRoundKet
  , KEquals
  , KId "n"
  , KSemi
  , KId "n"
  , KRoundBra
  , KOp "-"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KBraceKet
  , KId "total"
  , KBraceKet
  , Kfun
  , KId "fibonacci"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "tmp"
  , KComma
  , KId "pred"
  , KComma
  , KId "prepred"
  , KComma
  , KId "sign"
  , KBraceBra
  , Kif
  , KRoundBra
  , KId "n"
  , KOp "<"
  , KNum 0
  , KRoundKet
  , Kthen
  , KBraceBra
  , KId "n"
  , KEquals
  , KOp "-"
  , KId "n"
  , KSemi
  , KId "sign"
  , KEquals
  , KRoundBra
  , KRoundBra
  , KRoundBra
  , KId "n"
  , KOp "%"
  , KNum 2
  , KRoundKet
  , KOp "*"
  , KNum 2
  , KRoundKet
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , KId "sign"
  , KEquals
  , KNum 1
  , KSemi
  , KBraceKet
  , KId "pred"
  , KEquals
  , KNum 1
  , KSemi
  , KId "prepred"
  , KEquals
  , KNum 0
  , KSemi
  , Kwhile
  , KRoundBra
  , KId "n"
  , KOp "!="
  , KNum 0
  , KRoundKet
  , KBraceBra
  , KId "tmp"
  , KEquals
  , KId "pred"
  , KSemi
  , KId "pred"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KId "prepred"
  , KSemi
  , KId "prepred"
  , KEquals
  , KId "tmp"
  , KSemi
  , KId "n"
  , KRoundBra
  , KOp "-"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KBraceKet
  , KRoundBra
  , KId "prepred"
  , KOp "*"
  , KId "sign"
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "perm"
  , KRoundBra
  , KId "n"
  , KComma
  , KId "r"
  , KRoundKet
  , KBraceBra
  , KRoundBra
  , KId "factorial"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KOp "/"
  , KId "factorial"
  , KRoundBra
  , KRoundBra
  , KId "n"
  , KOp "-"
  , KId "r"
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "binom"
  , KRoundBra
  , KId "n"
  , KComma
  , KId "r"
  , KRoundKet
  , KBraceBra
  , KRoundBra
  , KId "perm"
  , KRoundBra
  , KId "n"
  , KComma
  , KId "r"
  , KRoundKet
  , KOp "/"
  , KId "factorial"
  , KRoundBra
  , KId "r"
  , KRoundKet
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "catalan"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KBraceBra
  , KRoundBra
  , KId "binom"
  , KRoundBra
  , KRoundBra
  , KNum 2
  , KOp "*"
  , KId "n"
  , KRoundKet
  , KComma
  , KId "n"
  , KRoundKet
  , KOp "/"
  , KRoundBra
  , KId "n"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "sqrt"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "val"
  , KBraceBra
  , Kwhile
  , KRoundBra
  , KRoundBra
  , KId "val"
  , KOp "^"
  , KNum 2
  , KRoundKet
  , KOp "<="
  , KId "n"
  , KRoundKet
  , KBraceBra
  , KId "val"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KBraceKet
  , KRoundBra
  , KId "val"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "isPrime"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "max"
  , KComma
  , KId "factor"
  , KBraceBra
  , KId "max"
  , KEquals
  , KId "sqrt"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KSemi
  , KId "factor"
  , KEquals
  , KNum 2
  , KSemi
  , Kwhile
  , KRoundBra
  , KId "factor"
  , KOp "<="
  , KId "max"
  , KRoundKet
  , KBraceBra
  , Kif
  , KOp "!"
  , KRoundBra
  , KId "n"
  , KOp "%"
  , KId "factor"
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , Kfalse
  , KSemi
  , KBraceKet
  , KId "factor"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KBraceKet
  , KRoundBra
  , KId "n"
  , KOp ">"
  , KNum 1
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "prime"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "pcount"
  , KComma
  , KId "cand"
  , KBraceBra
  , Kif
  , KRoundBra
  , KId "n"
  , KOp "<"
  , KNum 1
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KNum 0
  , KSemi
  , KBraceKet
  , KId "cand"
  , KEquals
  , KNum 1
  , KSemi
  , Kwhile
  , KRoundBra
  , KId "pcount"
  , KOp "<"
  , KId "n"
  , KRoundKet
  , KBraceBra
  , KId "cand"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KId "pcount"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KId "isPrime"
  , KRoundBra
  , KId "cand"
  , KRoundKet
  , KSemi
  , KBraceKet
  , KId "cand"
  , KBraceKet
  , Kfun
  , KId "numPrimeDivisors"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "i"
  , KComma
  , KId "p"
  , KComma
  , KId "k"
  , KBraceBra
  , KId "i"
  , KEquals
  , KNum 1
  , KSemi
  , KId "p"
  , KEquals
  , KId "prime"
  , KRoundBra
  , KId "i"
  , KRoundKet
  , KSemi
  , Kwhile
  , KRoundBra
  , KId "n"
  , KOp ">"
  , KNum 1
  , KRoundKet
  , KBraceBra
  , Kif
  , KOp "!"
  , KRoundBra
  , KId "n"
  , KOp "%"
  , KId "p"
  , KRoundKet
  , Kthen
  , KBraceBra
  , KId "k"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KId "n"
  , KRoundBra
  , KOp "/"
  , KRoundKet
  , KEquals
  , KId "p"
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , KId "i"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KId "p"
  , KEquals
  , KId "prime"
  , KRoundBra
  , KId "i"
  , KRoundKet
  , KSemi
  , KBraceKet
  , KBraceKet
  , KId "k"
  , KBraceKet
  , Kfun
  , KId "phi"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "i"
  , KComma
  , KId "p"
  , KComma
  , KId "k"
  , KComma
  , KId "pPhi"
  , KComma
  , KId "result"
  , KBraceBra
  , KId "i"
  , KEquals
  , KNum 1
  , KSemi
  , KId "p"
  , KEquals
  , KId "prime"
  , KRoundBra
  , KId "i"
  , KRoundKet
  , KSemi
  , KId "result"
  , KEquals
  , KNum 1
  , KSemi
  , Kwhile
  , KRoundBra
  , KId "n"
  , KOp ">"
  , KNum 1
  , KRoundKet
  , KBraceBra
  , Kif
  , KOp "!"
  , KRoundBra
  , KId "n"
  , KOp "%"
  , KId "p"
  , KRoundKet
  , Kthen
  , KBraceBra
  , KId "k"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KId "n"
  , KRoundBra
  , KOp "/"
  , KRoundKet
  , KEquals
  , KId "p"
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , KId "result"
  , KRoundBra
  , KOp "*"
  , KRoundKet
  , KEquals
  , KRoundBra
  , KRoundBra
  , KId "k"
  , KOp ">"
  , KNum 0
  , KRoundKet
  , KOp "*"
  , KRoundBra
  , KRoundBra
  , KId "p"
  , KOp "^"
  , KRoundBra
  , KId "k"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KOp "*"
  , KRoundBra
  , KId "p"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KSemi
  , KId "k"
  , KEquals
  , KOp "!"
  , KRoundBra
  , KId "k"
  , KOp ">"
  , KNum 0
  , KRoundKet
  , KSemi
  , KId "i"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KId "p"
  , KEquals
  , KId "prime"
  , KRoundBra
  , KId "i"
  , KRoundKet
  , KSemi
  , KBraceKet
  , KBraceKet
  , KRoundBra
  , KId "result"
  , KOp "*"
  , KRoundBra
  , KRoundBra
  , KId "k"
  , KOp ">"
  , KNum 0
  , KRoundKet
  , KOp "*"
  , KRoundBra
  , KRoundBra
  , KId "p"
  , KOp "^"
  , KRoundBra
  , KId "k"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KOp "*"
  , KRoundBra
  , KId "p"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "gcd"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "b"
  , KRoundKet
  , KBraceBra
  , KTernary
  , KRoundBra
  , KOp "!"
  , KRoundBra
  , KId "a"
  , KOp "%"
  , KId "b"
  , KRoundKet
  , KComma
  , KId "b"
  , KComma
  , KOuroboros
  , KRoundBra
  , KId "b"
  , KComma
  , KRoundBra
  , KId "a"
  , KOp "%"
  , KId "b"
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "triangular"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KBraceBra
  , KRoundBra
  , KRoundBra
  , KRoundBra
  , KId "n"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KOp "*"
  , KId "n"
  , KRoundKet
  , KOp "/"
  , KNum 2
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "ackermann"
  , KRoundBra
  , KId "m"
  , KComma
  , KId "n"
  , KRoundKet
  , KBraceBra
  , KTernary
  , KRoundBra
  , KOp "!"
  , KId "m"
  , KComma
  , KRoundBra
  , KId "n"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KComma
  , KTernary
  , KRoundBra
  , KOp "!"
  , KId "n"
  , KComma
  , KOuroboros
  , KRoundBra
  , KRoundBra
  , KId "m"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KComma
  , KNum 1
  , KRoundKet
  , KComma
  , KOuroboros
  , KRoundBra
  , KRoundBra
  , KId "m"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KComma
  , KOuroboros
  , KRoundBra
  , KId "m"
  , KComma
  , KRoundBra
  , KId "n"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "knuthUpArrow"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "b"
  , KComma
  , KId "p"
  , KRoundKet
  , KBraceBra
  , KTernary
  , KRoundBra
  , KRoundBra
  , KId "p"
  , KOp "=="
  , KNum 1
  , KRoundKet
  , KComma
  , KRoundBra
  , KId "a"
  , KOp "^"
  , KId "b"
  , KRoundKet
  , KComma
  , KTernary
  , KRoundBra
  , KRoundBra
  , KId "b"
  , KOp "=="
  , KNum 1
  , KRoundKet
  , KComma
  , KId "a"
  , KComma
  , KOuroboros
  , KRoundBra
  , KId "a"
  , KComma
  , KOuroboros
  , KRoundBra
  , KId "a"
  , KComma
  , KRoundBra
  , KId "b"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KComma
  , KId "p"
  , KRoundKet
  , KComma
  , KRoundBra
  , KId "p"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "main"
  , KRoundBra
  , KRoundKet
  , Kvars
  , KId "a"
  , KComma
  , KId "b"
  , KBraceBra
  , KId "a"
  , KEquals
  , KNum 1
  , KSemi
  , KId "b"
  , KEquals
  , KNum 0
  , KSemi
  , Kreturn
  , KOp "!"
  , KRoundBra
  , KOp "!"
  , KId "a"
  , KOp "&"
  , KId "b"
  , KRoundKet
  , KSemi
  , KBraceKet
  ]
