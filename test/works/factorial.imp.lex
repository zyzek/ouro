Just
  [ Kfun
  , KId "factorial"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "tmp"
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
  , KNum 1
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , KId "tmp"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
  , Kreturn
  , KRoundBra
  , KId "factorial"
  , KRoundBra
  , KId "tmp"
  , KRoundKet
  , KOp "*"
  , KId "n"
  , KRoundKet
  , KSemi
  , KBraceKet
  , KBraceKet
  , Kfun
  , KId "main"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KBraceBra
  , Kreturn
  , KId "factorial"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KSemi
  , KBraceKet
  ]
