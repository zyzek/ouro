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
  , Kreturn
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
  , KBraceBra
  , Kreturn
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
  , KSemi
  , KBraceKet
  , Kfun
  , KId "main"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , Kreturn
  , KId "max"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KSemi
  , KBraceKet
  ]
