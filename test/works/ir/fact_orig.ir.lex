Just
  [ KRoundBra
  , KRoundBra
  , KId "factorial"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KNum 0
  , KRoundBra
  , KLoad
  , KReg 1
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 2
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KId "cmp"
  , KReg 3
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "cond"
  , KReg 3
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 4
  , KId "cond"
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 4
  , KNum 1
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 1
  , KRoundBra
  , KConst
  , KReg 5
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 5
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 6
  , KId "tmp"
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 7
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 8
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 9
  , KReg 7
  , KReg 8
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 9
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 10
  , KId "tmp"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 11
  , KId "factorial"
  , KReg 10
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 12
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 13
  , KReg 11
  , KReg 12
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 13
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 14
  , KId "tmp"
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 14
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "main"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KNum 0
  , KRoundBra
  , KLoad
  , KReg 1
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 2
  , KId "factorial"
  , KReg 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 3
  , KId "tmp"
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 3
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundKet
  ]
