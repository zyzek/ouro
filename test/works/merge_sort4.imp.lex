Just
  [ Kfun
  , KId "min"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , Kif
  , KRoundBra
  , KId "x"
  , KOp "<"
  , KId "y"
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KId "x"
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , Kreturn
  , KId "y"
  , KSemi
  , KBraceKet
  , KBraceKet
  , Kfun
  , KId "max"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , Kif
  , KRoundBra
  , KId "x"
  , KOp ">"
  , KId "y"
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KId "x"
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , Kreturn
  , KId "y"
  , KSemi
  , KBraceKet
  , KBraceKet
  , Kfun
  , KId "not"
  , KRoundBra
  , KId "x"
  , KRoundKet
  , KBraceBra
  , Kreturn
  , KRoundBra
  , KId "x"
  , KOp "=="
  , KNum 0
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "xor"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "b"
  , KRoundKet
  , Kvars
  , KId "result"
  , KBraceBra
  , KId "result"
  , KEquals
  , KRoundBra
  , KRoundBra
  , KId "not"
  , KRoundBra
  , KId "a"
  , KRoundKet
  , KOp "&"
  , KId "b"
  , KRoundKet
  , KOp "|"
  , KRoundBra
  , KId "a"
  , KOp "&"
  , KId "not"
  , KRoundBra
  , KId "b"
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KSemi
  , Kreturn
  , KId "result"
  , KSemi
  , KBraceKet
  , Kfun
  , KId "fromBase"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , Kvars
  , KId "next"
  , KComma
  , KId "ctr"
  , KComma
  , KId "ret"
  , KComma
  , KId "ten"
  , KBraceBra
  , Kif
  , KRoundBra
  , KRoundBra
  , KId "y"
  , KOp ">"
  , KNum 10
  , KRoundKet
  , KOp "|"
  , KRoundBra
  , KId "y"
  , KOp "<"
  , KNum 2
  , KRoundKet
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KNum 0
  , KSemi
  , KBraceKet
  , KId "ten"
  , KEquals
  , KNum 10
  , KSemi
  , KId "ret"
  , KEquals
  , KNum 0
  , KSemi
  , KId "next"
  , KEquals
  , KId "x"
  , KSemi
  , KId "ctr"
  , KEquals
  , KNum 1
  , KSemi
  , Kwhile
  , KId "next"
  , KBraceBra
  , KId "ret"
  , KEquals
  , KRoundBra
  , KId "ret"
  , KOp "+"
  , KRoundBra
  , KRoundBra
  , KRoundBra
  , KId "y"
  , KOp "^"
  , KId "ctr"
  , KRoundKet
  , KOp "*"
  , KRoundBra
  , KId "next"
  , KOp "%"
  , KId "ten"
  , KRoundKet
  , KRoundKet
  , KOp "/"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KSemi
  , KId "ctr"
  , KEquals
  , KRoundBra
  , KId "ctr"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "next"
  , KEquals
  , KRoundBra
  , KId "next"
  , KOp "/"
  , KNum 10
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kreturn
  , KId "ret"
  , KSemi
  , KBraceKet
  , Kfun
  , KId "toBase"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , Kvars
  , KId "next"
  , KComma
  , KId "ctr"
  , KComma
  , KId "ret"
  , KComma
  , KId "ten"
  , KBraceBra
  , Kif
  , KRoundBra
  , KRoundBra
  , KId "y"
  , KOp ">"
  , KNum 10
  , KRoundKet
  , KOp "|"
  , KRoundBra
  , KId "y"
  , KOp "<"
  , KNum 2
  , KRoundKet
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KNum 0
  , KSemi
  , KBraceKet
  , KId "ten"
  , KEquals
  , KNum 10
  , KSemi
  , KId "ret"
  , KEquals
  , KNum 0
  , KSemi
  , KId "next"
  , KEquals
  , KId "x"
  , KSemi
  , KId "ctr"
  , KEquals
  , KNum 1
  , KSemi
  , Kwhile
  , KId "next"
  , KBraceBra
  , KId "ret"
  , KEquals
  , KRoundBra
  , KId "ret"
  , KOp "+"
  , KRoundBra
  , KRoundBra
  , KRoundBra
  , KId "ten"
  , KOp "^"
  , KId "ctr"
  , KRoundKet
  , KOp "*"
  , KRoundBra
  , KId "next"
  , KOp "%"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KOp "/"
  , KId "ten"
  , KRoundKet
  , KRoundKet
  , KSemi
  , KId "ctr"
  , KEquals
  , KRoundBra
  , KId "ctr"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "next"
  , KEquals
  , KRoundBra
  , KId "next"
  , KOp "/"
  , KNum 10
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kreturn
  , KId "ret"
  , KSemi
  , KBraceKet
  , Kfun
  , KId "floor"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , Kreturn
  , KRoundBra
  , KId "x"
  , KOp "-"
  , KRoundBra
  , KId "x"
  , KOp "%"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "ceil"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , Kreturn
  , KRoundBra
  , KRoundBra
  , KId "x"
  , KOp "-"
  , KRoundBra
  , KId "x"
  , KOp "%"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KOp "+"
  , KId "y"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "round"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , KId "x"
  , KEquals
  , KRoundBra
  , KId "x"
  , KOp "+"
  , KRoundBra
  , KId "y"
  , KOp "/"
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KSemi
  , KId "y"
  , KEquals
  , KRoundBra
  , KId "x"
  , KOp "%"
  , KId "y"
  , KRoundKet
  , KSemi
  , Kreturn
  , KRoundBra
  , KId "x"
  , KOp "-"
  , KId "y"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "trunc"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , Kreturn
  , KRoundBra
  , KRoundBra
  , KId "x"
  , KOp "/"
  , KRoundBra
  , KNum 10
  , KOp "^"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KOp "*"
  , KRoundBra
  , KNum 10
  , KOp "^"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "sigfigs"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , Kvars
  , KId "ctr"
  , KComma
  , KId "cond"
  , KBraceBra
  , KId "ctr"
  , KEquals
  , KNum 0
  , KSemi
  , KId "cond"
  , KEquals
  , KId "x"
  , KSemi
  , Kwhile
  , KId "cond"
  , KBraceBra
  , KId "cond"
  , KEquals
  , KRoundBra
  , KId "cond"
  , KOp "/"
  , KNum 10
  , KRoundKet
  , KSemi
  , KId "ctr"
  , KEquals
  , KRoundBra
  , KId "ctr"
  , KOp "+"
  , KNum 1
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kreturn
  , KRoundBra
  , KRoundBra
  , KId "x"
  , KOp "/"
  , KRoundBra
  , KNum 10
  , KOp "^"
  , KRoundBra
  , KId "ctr"
  , KOp "-"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KOp "*"
  , KRoundBra
  , KNum 10
  , KOp "^"
  , KRoundBra
  , KId "ctr"
  , KOp "-"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "abs"
  , KRoundBra
  , KId "x"
  , KRoundKet
  , KBraceBra
  , Kif
  , KRoundBra
  , KId "x"
  , KOp ">="
  , KNum 0
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KId "x"
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , Kreturn
  , KOp "-"
  , KId "x"
  , KSemi
  , KBraceKet
  , KBraceKet
  , Kfun
  , KId "clamp"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "low"
  , KComma
  , KId "high"
  , KRoundKet
  , Kvars
  , KId "lower"
  , KBraceBra
  , KId "lower"
  , KEquals
  , KId "max"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "low"
  , KRoundKet
  , KSemi
  , Kreturn
  , KId "min"
  , KRoundBra
  , KId "lower"
  , KComma
  , KId "high"
  , KRoundKet
  , KSemi
  , KBraceKet
  , Kfun
  , KId "main"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "b"
  , KComma
  , KId "c"
  , KComma
  , KId "d"
  , KRoundKet
  , Kvars
  , KId "a2"
  , KComma
  , KId "b2"
  , KComma
  , KId "c2"
  , KComma
  , KId "d2"
  , KBraceBra
  , KId "tmp"
  , KEquals
  , KId "max"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "b"
  , KRoundKet
  , KSemi
  , KId "a"
  , KEquals
  , KId "min"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "b"
  , KRoundKet
  , KSemi
  , KId "b"
  , KEquals
  , KId "tmp"
  , KSemi
  , KId "tmp"
  , KEquals
  , KId "max"
  , KRoundBra
  , KId "c"
  , KComma
  , KId "d"
  , KRoundKet
  , KSemi
  , KId "c"
  , KEquals
  , KId "min"
  , KRoundBra
  , KId "c"
  , KComma
  , KId "d"
  , KRoundKet
  , KSemi
  , KId "d"
  , KEquals
  , KId "tmp"
  , KSemi
  , KId "a2"
  , KEquals
  , KId "min"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "c"
  , KRoundKet
  , KSemi
  , KId "b2"
  , KEquals
  , KId "min"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "d"
  , KRoundKet
  , KSemi
  , KId "b2"
  , KEquals
  , KId "min"
  , KRoundBra
  , KId "b2"
  , KComma
  , KId "b"
  , KRoundKet
  , KSemi
  , KId "c2"
  , KEquals
  , KId "min"
  , KRoundBra
  , KId "d"
  , KComma
  , KId "b"
  , KRoundKet
  , KSemi
  , KId "d2"
  , KEquals
  , KId "max"
  , KRoundBra
  , KId "d"
  , KComma
  , KId "b"
  , KRoundKet
  , KSemi
  , KId "a"
  , KEquals
  , KId "a2"
  , KSemi
  , KId "b"
  , KEquals
  , KId "b2"
  , KSemi
  , KId "c"
  , KEquals
  , KId "c2"
  , KSemi
  , KId "d"
  , KEquals
  , KId "d2"
  , KSemi
  , Kprint
  , KId "a"
  , KSemi
  , Kprint
  , KId "b"
  , KSemi
  , Kprint
  , KId "c"
  , KSemi
  , Kreturn
  , KId "d"
  , KSemi
  , KBraceKet
  ]
