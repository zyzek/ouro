Just
  [ Kfun
  , KId "factorial"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "count"
  , KComma
  , KId "total"
  , KBraceBra
  , KId "total"
  , KEquals
  , KNum 1
  , KSemi
  , KId "count"
  , KEquals
  , KNum 0
  , KSemi
  , Kwhile
  , KRoundBra
  , KId "count"
  , KOp "<"
  , KId "n"
  , KRoundKet
  , KBraceBra
  , KId "count"
  , KEquals
  , KRoundBra
  , KId "count"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "total"
  , KEquals
  , KRoundBra
  , KId "total"
  , KOp "*"
  , KId "count"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kreturn
  , KId "total"
  , KSemi
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
  , KEquals
  , KRoundBra
  , KId "prepred"
  , KOp "+"
  , KId "pred"
  , KRoundKet
  , KSemi
  , KId "prepred"
  , KEquals
  , KId "tmp"
  , KSemi
  , KId "n"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kreturn
  , KRoundBra
  , KId "prepred"
  , KOp "*"
  , KId "sign"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "perm"
  , KRoundBra
  , KId "n"
  , KComma
  , KId "r"
  , KRoundKet
  , Kvars
  , KId "d"
  , KBraceBra
  , KId "d"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "-"
  , KId "r"
  , KRoundKet
  , KSemi
  , Kreturn
  , KRoundBra
  , KId "factorial"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KOp "/"
  , KId "factorial"
  , KRoundBra
  , KId "d"
  , KRoundKet
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "binom"
  , KRoundBra
  , KId "n"
  , KComma
  , KId "r"
  , KRoundKet
  , KBraceBra
  , Kreturn
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
  , KSemi
  , KBraceKet
  , Kfun
  , KId "catalan"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "m"
  , KBraceBra
  , KId "m"
  , KEquals
  , KRoundBra
  , KNum 2
  , KOp "*"
  , KId "n"
  , KRoundKet
  , KSemi
  , Kreturn
  , KRoundBra
  , KId "binom"
  , KRoundBra
  , KId "m"
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
  , KSemi
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
  , KEquals
  , KRoundBra
  , KId "val"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kreturn
  , KRoundBra
  , KId "val"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
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
  , KEquals
  , KRoundBra
  , KId "factor"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kif
  , KRoundBra
  , KId "n"
  , KOp "<="
  , KNum 1
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , Kfalse
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , Kreturn
  , Ktrue
  , KSemi
  , KBraceKet
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
  , KEquals
  , KRoundBra
  , KId "cand"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , Kif
  , KId "isPrime"
  , KRoundBra
  , KId "cand"
  , KRoundKet
  , Kthen
  , KBraceBra
  , KId "pcount"
  , KEquals
  , KRoundBra
  , KId "pcount"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KBraceKet
  , KBraceKet
  , Kreturn
  , KId "cand"
  , KSemi
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
  , KEquals
  , KRoundBra
  , KId "k"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "n"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "/"
  , KId "p"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , KId "i"
  , KEquals
  , KRoundBra
  , KId "i"
  , KOp "+"
  , KNum 1
  , KRoundKet
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
  , Kreturn
  , KId "k"
  , KSemi
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
  , KEquals
  , KRoundBra
  , KId "k"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "n"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "/"
  , KId "p"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , Kif
  , KRoundBra
  , KId "k"
  , KOp ">"
  , KNum 0
  , KRoundKet
  , Kthen
  , KBraceBra
  , KId "pPhi"
  , KEquals
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
  , KSemi
  , KId "result"
  , KEquals
  , KRoundBra
  , KId "result"
  , KOp "*"
  , KId "pPhi"
  , KRoundKet
  , KSemi
  , KId "k"
  , KEquals
  , KNum 0
  , KSemi
  , KBraceKet
  , KId "i"
  , KEquals
  , KRoundBra
  , KId "i"
  , KOp "+"
  , KNum 1
  , KRoundKet
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
  , Kif
  , KRoundBra
  , KId "k"
  , KOp ">"
  , KNum 0
  , KRoundKet
  , Kthen
  , KBraceBra
  , KId "pPhi"
  , KEquals
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
  , KSemi
  , KId "result"
  , KEquals
  , KRoundBra
  , KId "result"
  , KOp "*"
  , KId "pPhi"
  , KRoundKet
  , KSemi
  , KId "k"
  , KEquals
  , KNum 0
  , KSemi
  , KBraceKet
  , Kreturn
  , KId "result"
  , KSemi
  , KBraceKet
  , Kfun
  , KId "gcd"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "b"
  , KRoundKet
  , Kvars
  , KId "r"
  , KBraceBra
  , Kif
  , KOp "!"
  , KRoundBra
  , KId "a"
  , KOp "%"
  , KId "b"
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KId "b"
  , KSemi
  , KBraceKet
  , KId "r"
  , KEquals
  , KRoundBra
  , KId "a"
  , KOp "%"
  , KId "b"
  , KRoundKet
  , KSemi
  , Kreturn
  , KId "gcd"
  , KRoundBra
  , KId "b"
  , KComma
  , KId "r"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "triangular"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KBraceBra
  , Kreturn
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
  , KSemi
  , KBraceKet
  , Kfun
  , KId "ackermann"
  , KRoundBra
  , KId "m"
  , KComma
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "mdec"
  , KComma
  , KId "ndec"
  , KBraceBra
  , Kif
  , KOp "!"
  , KId "m"
  , Kthen
  , KBraceBra
  , Kreturn
  , KRoundBra
  , KId "n"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , KId "mdec"
  , KEquals
  , KRoundBra
  , KId "m"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
  , Kif
  , KOp "!"
  , KId "n"
  , Kthen
  , KBraceBra
  , KId "ndec"
  , KEquals
  , KNum 1
  , KSemi
  , Kreturn
  , KId "ackermann"
  , KRoundBra
  , KId "mdec"
  , KComma
  , KId "ndec"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , KId "ndec"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "ndec"
  , KEquals
  , KId "ackermann"
  , KRoundBra
  , KId "m"
  , KComma
  , KId "ndec"
  , KRoundKet
  , KSemi
  , Kreturn
  , KId "ackermann"
  , KRoundBra
  , KId "mdec"
  , KComma
  , KId "ndec"
  , KRoundKet
  , KSemi
  , KBraceKet
  , KBraceKet
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
  , Kvars
  , KId "r"
  , KComma
  , KId "pdec"
  , KComma
  , KId "bdec"
  , KBraceBra
  , Kif
  , KRoundBra
  , KId "p"
  , KOp "=="
  , KNum 1
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KRoundBra
  , KId "a"
  , KOp "^"
  , KId "b"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kif
  , KRoundBra
  , KId "b"
  , KOp "=="
  , KNum 1
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KId "a"
  , KSemi
  , KBraceKet
  , KId "pdec"
  , KEquals
  , KRoundBra
  , KId "p"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "bdec"
  , KEquals
  , KRoundBra
  , KId "b"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "r"
  , KEquals
  , KId "knuthUpArrow"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "bdec"
  , KComma
  , KId "p"
  , KRoundKet
  , KSemi
  , Kreturn
  , KId "knuthUpArrow"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "r"
  , KComma
  , KId "pdec"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "main"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "counter"
  , KBraceBra
  , Kwhile
  , KRoundBra
  , KId "counter"
  , KOp "<"
  , KId "n"
  , KRoundKet
  , KBraceBra
  , KId "counter"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , Kprint
  , KId "prime"
  , KRoundBra
  , KId "counter"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kreturn
  , KNum 0
  , KSemi
  , KBraceKet
  ]
