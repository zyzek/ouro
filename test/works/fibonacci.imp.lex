Just
  [ Kfun
  , KId "fib"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "pred"
  , KComma
  , KId "prepred"
  , KBraceBra
  , Kif
  , KRoundBra
  , KId "n"
  , KOp "=="
  , KNum 0
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KNum 0
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , Kif
  , KRoundBra
  , KId "n"
  , KOp "=="
  , KNum 1
  , KRoundKet
  , Kthen
  , KBraceBra
  , Kreturn
  , KNum 1
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , KId "pred"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "prepred"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "-"
  , KNum 2
  , KRoundKet
  , KSemi
  , Kreturn
  , KRoundBra
  , KId "fib"
  , KRoundBra
  , KId "pred"
  , KRoundKet
  , KOp "+"
  , KId "fib"
  , KRoundBra
  , KId "prepred"
  , KRoundKet
  , KRoundKet
  , KSemi
  , KBraceKet
  , KBraceKet
  , KBraceKet
  , Kfun
  , KId "main"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KBraceBra
  , Kreturn
  , KId "fib"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KSemi
  , KBraceKet
  ]
