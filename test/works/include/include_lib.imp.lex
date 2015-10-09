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
  , KId "x"
  , KOp "<"
  , KId "y"
  , KBraceBra
  , KId "x"
  , KBraceKet
  , KBraceBra
  , KId "y"
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
  , KId "x"
  , KOp ">"
  , KId "y"
  , KBraceBra
  , KId "x"
  , KBraceKet
  , KBraceBra
  , KId "y"
  , KBraceKet
  , KBraceKet
  , Kfun
  , KId "not"
  , KRoundBra
  , KId "x"
  , KRoundKet
  , KBraceBra
  , KId "x"
  , KOp "=="
  , KNum 0
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
  , KId "y"
  , KOp ">"
  , KNum 10
  , KOp "|"
  , KId "y"
  , KOp "<"
  , KNum 2
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
  , Kwhile
  , KId "next"
  , KBraceBra
  , KId "ret"
  , KOp "+"
  , KEquals
  , KId "y"
  , KOp "^"
  , KId "ctr"
  , KOp "*"
  , KRoundBra
  , KId "next"
  , KOp "%"
  , KNum 10
  , KRoundKet
  , KSemi
  , KId "ctr"
  , KOp "+"
  , KEquals
  , KNum 1
  , KSemi
  , KId "next"
  , KOp "/"
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
  , KId "y"
  , KOp ">"
  , KNum 10
  , KOp "|"
  , KId "y"
  , KOp "<"
  , KNum 2
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
  , Kwhile
  , KId "next"
  , KBraceBra
  , KId "ret"
  , KOp "+"
  , KEquals
  , KNum 10
  , KOp "^"
  , KId "ctr"
  , KOp "*"
  , KRoundBra
  , KId "next"
  , KOp "%"
  , KId "y"
  , KRoundKet
  , KSemi
  , KId "ctr"
  , KOp "+"
  , KEquals
  , KNum 1
  , KSemi
  , KId "next"
  , KOp "/"
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
  , KId "x"
  , KOp "-"
  , KId "x"
  , KOp "%"
  , KId "y"
  , KBraceKet
  , Kfun
  , KId "ceil"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , KId "x"
  , KOp "-"
  , KId "x"
  , KOp "%"
  , KId "y"
  , KOp "+"
  , KId "y"
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
  , KId "x"
  , KOp "+"
  , KId "y"
  , KOp "/"
  , KNum 2
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
  , KId "x"
  , KOp "/"
  , KNum 10
  , KOp "^"
  , KId "y"
  , KRoundKet
  , KOp "*"
  , KNum 10
  , KOp "^"
  , KId "y"
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
  , KOp "/"
  , KEquals
  , KNum 10
  , KSemi
  , KId "ctr"
  , KOp "+"
  , KEquals
  , KNum 1
  , KSemi
  , KBraceKet
  , KRoundBra
  , KId "x"
  , KOp "/"
  , KNum 10
  , KOp "^"
  , KRoundBra
  , KId "ctr"
  , KOp "-"
  , KId "y"
  , KRoundKet
  , KRoundKet
  , KOp "*"
  , KNum 10
  , KOp "^"
  , KRoundBra
  , KId "ctr"
  , KOp "-"
  , KId "y"
  , KRoundKet
  , KBraceKet
  , Kfun
  , KId "abs"
  , KRoundBra
  , KId "x"
  , KRoundKet
  , KBraceBra
  , Kif
  , KId "x"
  , KOp ">="
  , KNum 0
  , KBraceBra
  , KId "x"
  , KBraceKet
  , KBraceBra
  , KOp "-"
  , KId "x"
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
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceBra
  , KId "max"
  , KRoundBra
  , KId "x"
  , KComma
  , KId "y"
  , KRoundKet
  , KBraceKet
  ]
