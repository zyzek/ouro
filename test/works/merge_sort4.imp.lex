Just
  [ Kfun
  , KId "min"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , KTernary
  , KRoundBra
  , KRoundBra
  , KId "x"
  , KOp "<"
  , KId "y"
  , KRoundKet
  , KComma
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "max"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , KTernary
  , KRoundBra
  , KRoundBra
  , KId "x"
  , KOp ">"
  , KId "y"
  , KRoundKet
  , KComma
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "not"
  , KRoundBra
  , KId "x"
  , KRoundKet
  , KBraceBra
  , KRoundBra
  , KId "x"
  , KOp "=="
  , KNum 0
  , KRoundKet
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
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
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
  , KNum 10
  , KRoundKet
  , KRoundKet
  , KOp "/"
  , KId "y"
  , KRoundKet
  , KSemi
  , KId "ctr"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KId "next"
  , KRoundBra
  , KOp "/"
  , KRoundKet
  , KEquals
  , KNum 10
  , KSemi
  , KBraceKet
  , KId "ret"
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
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KRoundBra
  , KRoundBra
  , KRoundBra
  , KNum 10
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
  , KNum 10
  , KRoundKet
  , KSemi
  , KId "ctr"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KId "next"
  , KRoundBra
  , KOp "/"
  , KRoundKet
  , KEquals
  , KNum 10
  , KSemi
  , KBraceKet
  , KId "ret"
  , KBraceKet
  , Kfun
  , KId "floor"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , KRoundBra
  , KId "x"
  , KOp "-"
  , KRoundBra
  , KId "x"
  , KOp "%"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "ceil"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
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
  , KBraceKet
  , Kfun
  , KId "round"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , KId "floor"
  , KRoundBra
  , KRoundBra
  , KId "x"
  , KOp "+"
  , KRoundBra
  , KId "y"
  , KOp "/"
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "trunc"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
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
  , KRoundBra
  , KOp "/"
  , KRoundKet
  , KEquals
  , KNum 10
  , KSemi
  , KId "ctr"
  , KRoundBra
  , KOp "+"
  , KRoundKet
  , KEquals
  , KNum 1
  , KSemi
  , KBraceKet
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
  , KBraceKet
  , Kfun
  , KId "abs"
  , KRoundBra
  , KId "x"
  , KRoundKet
  , KBraceBra
  , KTernary
  , KRoundBra
  , KRoundBra
  , KId "x"
  , KOp ">="
  , KNum 0
  , KRoundKet
  , KComma
  , KId "x"
  , KComma
  , KOp "-"
  , KId "x"
  , KRoundKet
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
  , KBraceBra
  , KId "min"
  , KRoundBra
  , KId "max"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "low"
  , KRoundKet
  , KComma
  , KId "high"
  , KRoundKet
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
  , KRoundBra
  , KId "min"
  , KRoundKet
  , KEquals
  , KId "b"
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
  , KRoundBra
  , KId "min"
  , KRoundKet
  , KEquals
  , KId "d"
  , KSemi
  , KId "d"
  , KEquals
  , KId "tmp"
  , KSemi
  , KId "aR"
  , KEquals
  , KId "min"
  , KRoundBra
  , KId "a"
  , KComma
  , KId "c"
  , KRoundKet
  , KSemi
  , KId "aO"
  , KEquals
  , KId "max"
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
  , KId "aO"
  , KComma
  , KId "d"
  , KRoundKet
  , KSemi
  , KId "b22"
  , KEquals
  , KId "min"
  , KRoundBra
  , KId "b"
  , KComma
  , KId "aO"
  , KRoundKet
  , KSemi
  , KId "bR"
  , KEquals
  , KId "min"
  , KRoundBra
  , KId "b2"
  , KComma
  , KId "b22"
  , KRoundKet
  , KSemi
  , KId "cR"
  , KEquals
  , KId "max"
  , KRoundBra
  , KId "b2"
  , KComma
  , KId "b22"
  , KRoundKet
  , KSemi
  , KId "dR"
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
  , KId "aR"
  , KSemi
  , KId "b"
  , KEquals
  , KId "bR"
  , KSemi
  , KId "c"
  , KEquals
  , KId "cR"
  , KSemi
  , KId "d"
  , KEquals
  , KId "dR"
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
  , KId "d"
  , KBraceKet
  ]
