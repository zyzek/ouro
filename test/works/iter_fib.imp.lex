Just
  [ Kfun
  , KId "fib"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "tmp"
  , KComma
  , KId "pred"
  , KComma
  , KId "prepred"
  , KBraceBra
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
  , KId "prepred"
  , KSemi
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
