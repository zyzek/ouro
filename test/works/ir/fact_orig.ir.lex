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
  , KConst
  , KReg 0
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "cond"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 0
  , KNum 1
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 1
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
  , KInstr "eq"
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
  , KNum 2
  , KNum 3
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KConst
  , KReg 6
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 7
  , KId "tmp"
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 7
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KLoad
  , KReg 9
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 10
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 11
  , KReg 9
  , KReg 10
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 11
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 12
  , KId "tmp"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 13
  , KId "factorial"
  , KReg 12
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 14
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 15
  , KReg 13
  , KReg 14
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 15
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 16
  , KId "tmp"
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 16
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
  , KConst
  , KReg 0
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 0
  , KNum 1
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 1
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
