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
  , KBraceBra
  , Kreturn
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
  , KBraceBra
  , Kreturn
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
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
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
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KBraceKet
  , Kreturn
  , KRoundBra
  , KId "n"
  , KOp ">"
  , KNum 1
  , KRoundKet
  , KSemi
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
  , Kif
  , KId "isPrime"
  , KRoundBra
  , KId "cand"
  , KRoundKet
  , Kthen
  , KBraceBra
  , KId "pcount"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
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
  , Kif
  , KRoundBra
  , KId "k"
  , KOp ">"
  , KNum 0
  , KRoundKet
  , Kthen
  , KBraceBra
  , KId "result"
  , KRoundBra
  , KOp "*"
  , KRoundKet
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
  , KId "k"
  , KEquals
  , KNum 0
  , KSemi
  , KBraceKet
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
  , Kif
  , KRoundBra
  , KId "k"
  , KOp ">"
  , KNum 0
  , KRoundKet
  , Kthen
  , KBraceBra
  , KId "result"
  , KRoundBra
  , KOp "*"
  , KRoundKet
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
  , Kreturn
  , Kouro
  , KRoundBra
  , KId "b"
  , KComma
  , KRoundBra
  , KId "a"
  , KOp "%"
  , KId "b"
  , KRoundKet
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
  , Kif
  , KOp "!"
  , KId "n"
  , Kthen
  , KBraceBra
  , Kreturn
  , Kouro
  , KRoundBra
  , KRoundBra
  , KId "m"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KComma
  , KNum 1
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , Kreturn
  , Kouro
  , KRoundBra
  , KRoundBra
  , KId "m"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KComma
  , Kouro
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
  , Kreturn
  , Kouro
  , KRoundBra
  , KId "a"
  , KComma
  , Kouro
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
  , KSemi
  , KBraceKet
  , Kfun
  , KId "main"
  , KRoundBra
  , KRoundKet
  , Kvars
  , KId "i"
  , KBraceBra
  , Kprint
  , KId "ackermann"
  , KRoundBra
  , KNum 2
  , KComma
  , KNum 2
  , KRoundKet
  , KSemi
  , Kprint
  , KId "fibonacci"
  , KRoundBra
  , KOp "-"
  , KNum 4
  , KRoundKet
  , KSemi
  , Kprint
  , KId "catalan"
  , KRoundBra
  , KNum 5
  , KRoundKet
  , KSemi
  , Kprint
  , KId "sqrt"
  , KRoundBra
  , KNum 256
  , KRoundKet
  , KSemi
  , Kprint
  , KId "isPrime"
  , KRoundBra
  , KNum 47
  , KRoundKet
  , KSemi
  , Kprint
  , KId "isPrime"
  , KRoundBra
  , KNum 20
  , KRoundKet
  , KSemi
  , Kprint
  , KId "prime"
  , KRoundBra
  , KNum 10
  , KRoundKet
  , KSemi
  , Kprint
  , KId "numPrimeDivisors"
  , KRoundBra
  , KNum 1024
  , KRoundKet
  , KSemi
  , Kprint
  , KId "phi"
  , KRoundBra
  , KNum 36
  , KRoundKet
  , KSemi
  , Kprint
  , KId "gcd"
  , KRoundBra
  , KNum 13
  , KComma
  , KNum 299
  , KRoundKet
  , KSemi
  , Kprint
  , KId "gcd"
  , KRoundBra
  , KNum 20
  , KComma
  , KNum 45
  , KRoundKet
  , KSemi
  , Kwhile
  , KRoundBra
  , KId "i"
  , KOp "<"
  , KNum 10
  , KRoundKet
  , KBraceBra
  , Kprint
  , KId "triangular"
  , KRoundBra
  , KId "p"
  , KRoundKet
  , KSemi
  , KId "i"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KBraceKet
  , Kprint
  , KId "knuthUpArrow"
  , KRoundBra
  , KNum 3
  , KComma
  , KNum 2
  , KComma
  , KNum 2
  , KRoundKet
  , KSemi
  , Kreturn
  , KOp "-"
  , KNum 123
  , KSemi
  , KBraceKet
  ]
