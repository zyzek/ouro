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
  , KId "total"
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
  , KConst
  , KReg 1
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "total"
  , KReg 1
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 2
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 2
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 3
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 4
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "gt"
  , KReg 5
  , KReg 3
  , KReg 4
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 5
  , KNum 3
  , KNum 4
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KLoad
  , KReg 7
  , KId "total"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 8
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 9
  , KReg 7
  , KReg 8
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "total"
  , KReg 9
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 10
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 11
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 12
  , KReg 10
  , KReg 11
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "n"
  , KReg 12
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 13
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 13
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KLoad
  , KReg 14
  , KId "total"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 15
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 16
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KInstr "geq"
  , KReg 17
  , KReg 15
  , KReg 16
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 18
  , KReg 14
  , KReg 17
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 18
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "fibonacci"
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
  , KStore
  , KId "pred"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "prepred"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "sign"
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
  , KInstr "lt"
  , KReg 3
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 3
  , KNum 2
  , KNum 3
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 5
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KInstr "neg"
  , KReg 6
  , KReg 5
  , KReg 5
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "n"
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 7
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 8
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KInstr "mod"
  , KReg 9
  , KReg 7
  , KReg 8
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 10
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 11
  , KReg 9
  , KReg 10
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 12
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 13
  , KReg 11
  , KReg 12
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "sign"
  , KReg 13
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 15
  , KReg 12
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 14
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 14
  , KNum 4
  , KNum 4
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KConst
  , KReg 15
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "sign"
  , KReg 15
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 15
  , KReg 14
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 16
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 16
  , KNum 4
  , KNum 4
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KConst
  , KReg 16
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "pred"
  , KReg 16
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 17
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "prepred"
  , KReg 17
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 18
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 18
  , KNum 5
  , KNum 5
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 5
  , KRoundBra
  , KLoad
  , KReg 19
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 20
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KInstr "neq"
  , KReg 21
  , KReg 19
  , KReg 20
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 21
  , KNum 6
  , KNum 7
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 6
  , KRoundBra
  , KLoad
  , KReg 23
  , KId "pred"
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "tmp"
  , KReg 23
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 24
  , KId "pred"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 25
  , KId "prepred"
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 26
  , KReg 24
  , KReg 25
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "pred"
  , KReg 26
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 27
  , KId "tmp"
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "prepred"
  , KReg 27
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 28
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 29
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 30
  , KReg 28
  , KReg 29
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "n"
  , KReg 30
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 31
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 31
  , KNum 5
  , KNum 5
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 7
  , KRoundBra
  , KLoad
  , KReg 32
  , KId "prepred"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 33
  , KId "sign"
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 34
  , KReg 32
  , KReg 33
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 34
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "perm"
  , KRoundBra
  , KId "n"
  , KId "r"
  , KRoundKet
  , KRoundBra
  , KNum 0
  , KRoundBra
  , KConst
  , KReg 0
  , KNum 0
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
  , KLoad
  , KReg 3
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 4
  , KId "r"
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 5
  , KReg 3
  , KReg 4
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 6
  , KId "factorial"
  , KReg 5
  , KRoundKet
  , KRoundBra
  , KInstr "div"
  , KReg 7
  , KReg 2
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 7
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "binom"
  , KRoundBra
  , KId "n"
  , KId "r"
  , KRoundKet
  , KRoundBra
  , KNum 0
  , KRoundBra
  , KConst
  , KReg 0
  , KNum 0
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
  , KLoad
  , KReg 2
  , KId "r"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 3
  , KId "perm"
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 4
  , KId "r"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 5
  , KId "factorial"
  , KReg 4
  , KRoundKet
  , KRoundBra
  , KInstr "div"
  , KReg 6
  , KReg 3
  , KReg 5
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 6
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "choose"
  , KRoundBra
  , KId "n"
  , KId "r"
  , KRoundKet
  , KRoundBra
  , KNum 0
  , KRoundBra
  , KConst
  , KReg 0
  , KNum 0
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
  , KLoad
  , KReg 2
  , KId "r"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 3
  , KId "binom"
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 3
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "catalan"
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
  , KBranch
  , KReg 0
  , KNum 1
  , KNum 1
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 1
  , KRoundBra
  , KConst
  , KReg 1
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 2
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 3
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 4
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 5
  , KId "binom"
  , KReg 3
  , KReg 4
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 6
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 7
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 8
  , KReg 6
  , KReg 7
  , KRoundKet
  , KRoundBra
  , KInstr "div"
  , KReg 9
  , KReg 5
  , KReg 8
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 9
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "sqrt"
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
  , KId "val"
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
  , KConst
  , KReg 1
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 1
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 2
  , KId "val"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 3
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KInstr "pow"
  , KReg 4
  , KReg 2
  , KReg 3
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 5
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KInstr "leq"
  , KReg 6
  , KReg 4
  , KReg 5
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 6
  , KNum 3
  , KNum 4
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KLoad
  , KReg 8
  , KId "val"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 9
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 10
  , KReg 8
  , KReg 9
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "val"
  , KReg 10
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 11
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 11
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KLoad
  , KReg 12
  , KId "val"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 13
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 14
  , KReg 12
  , KReg 13
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 14
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "isPrime"
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
  , KId "max"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "factor"
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
  , KId "sqrt"
  , KReg 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "max"
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 3
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "factor"
  , KReg 3
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 4
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 4
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 5
  , KId "factor"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 6
  , KId "max"
  , KRoundKet
  , KRoundBra
  , KInstr "leq"
  , KReg 7
  , KReg 5
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 7
  , KNum 3
  , KNum 6
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
  , KLoad
  , KReg 10
  , KId "factor"
  , KRoundKet
  , KRoundBra
  , KInstr "mod"
  , KReg 11
  , KReg 9
  , KReg 10
  , KRoundKet
  , KRoundBra
  , KInstr "not"
  , KReg 12
  , KReg 11
  , KReg 11
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 12
  , KNum 4
  , KNum 5
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KConst
  , KReg 14
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 14
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 5
  , KRoundBra
  , KLoad
  , KReg 15
  , KId "factor"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 16
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 17
  , KReg 15
  , KReg 16
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "factor"
  , KReg 17
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 18
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 18
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 6
  , KRoundBra
  , KLoad
  , KReg 19
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 20
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "gt"
  , KReg 21
  , KReg 19
  , KReg 20
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 21
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "prime"
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
  , KId "pcount"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "cand"
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
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "lt"
  , KReg 3
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 3
  , KNum 2
  , KNum 3
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KConst
  , KReg 5
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 5
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KConst
  , KReg 6
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "cand"
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 7
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 7
  , KNum 4
  , KNum 4
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KLoad
  , KReg 8
  , KId "pcount"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 9
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KInstr "lt"
  , KReg 10
  , KReg 8
  , KReg 9
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 10
  , KNum 5
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 5
  , KRoundBra
  , KLoad
  , KReg 12
  , KId "cand"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 13
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 14
  , KReg 12
  , KReg 13
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "cand"
  , KReg 14
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 15
  , KId "pcount"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 16
  , KId "cand"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 17
  , KId "isPrime"
  , KReg 16
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 18
  , KReg 15
  , KReg 17
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "pcount"
  , KReg 18
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 19
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 19
  , KNum 4
  , KNum 4
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 6
  , KRoundBra
  , KLoad
  , KReg 20
  , KId "cand"
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 20
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "coprime"
  , KRoundBra
  , KId "m"
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
  , KId "f"
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
  , KId "m"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 2
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 3
  , KId "gcd"
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "f"
  , KReg 3
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 4
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 4
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 5
  , KId "f"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 6
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "neq"
  , KReg 7
  , KReg 5
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 7
  , KNum 3
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KLoad
  , KReg 9
  , KId "f"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 10
  , KId "m"
  , KRoundKet
  , KRoundBra
  , KInstr "eq"
  , KReg 11
  , KReg 9
  , KReg 10
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 12
  , KId "f"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 13
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KInstr "eq"
  , KReg 14
  , KReg 12
  , KReg 13
  , KRoundKet
  , KRoundBra
  , KInstr "or"
  , KReg 15
  , KReg 11
  , KReg 14
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 15
  , KNum 4
  , KNum 5
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KConst
  , KReg 17
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 17
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 5
  , KRoundBra
  , KLoad
  , KReg 18
  , KId "m"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 19
  , KId "f"
  , KRoundKet
  , KRoundBra
  , KInstr "div"
  , KReg 20
  , KReg 18
  , KReg 19
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 21
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 22
  , KId "f"
  , KRoundKet
  , KRoundBra
  , KInstr "div"
  , KReg 23
  , KReg 21
  , KReg 22
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "m"
  , KReg 20
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "n"
  , KReg 23
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 24
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 24
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 6
  , KRoundBra
  , KConst
  , KReg 25
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 25
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "numPrimeDivisors"
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
  , KId "i"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "p"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "k"
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
  , KConst
  , KReg 1
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "i"
  , KReg 1
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 2
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 3
  , KId "prime"
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "p"
  , KReg 3
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 4
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 4
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 5
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 6
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "gt"
  , KReg 7
  , KReg 5
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 7
  , KNum 3
  , KNum 7
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
  , KLoad
  , KReg 10
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KInstr "mod"
  , KReg 11
  , KReg 9
  , KReg 10
  , KRoundKet
  , KRoundBra
  , KInstr "not"
  , KReg 12
  , KReg 11
  , KReg 11
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 12
  , KNum 4
  , KNum 5
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KLoad
  , KReg 14
  , KId "k"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 15
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 16
  , KReg 14
  , KReg 15
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "k"
  , KReg 16
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 17
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 18
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KInstr "div"
  , KReg 19
  , KReg 17
  , KReg 18
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "n"
  , KReg 19
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 25
  , KReg 18
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 20
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 20
  , KNum 6
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 5
  , KRoundBra
  , KLoad
  , KReg 21
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 22
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 23
  , KReg 21
  , KReg 22
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "i"
  , KReg 23
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 24
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 25
  , KId "prime"
  , KReg 24
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "p"
  , KReg 25
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 25
  , KReg 24
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 26
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 26
  , KNum 6
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 6
  , KRoundBra
  , KConst
  , KReg 26
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 26
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 7
  , KRoundBra
  , KLoad
  , KReg 27
  , KId "k"
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 27
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "phi"
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
  , KId "i"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "p"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "k"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "pPhi"
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "result"
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
  , KConst
  , KReg 1
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "i"
  , KReg 1
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 2
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 3
  , KId "prime"
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "p"
  , KReg 3
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 4
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "result"
  , KReg 4
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 5
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 5
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 6
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 7
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "gt"
  , KReg 8
  , KReg 6
  , KReg 7
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 8
  , KNum 3
  , KNum 7
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KLoad
  , KReg 10
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 11
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KInstr "mod"
  , KReg 12
  , KReg 10
  , KReg 11
  , KRoundKet
  , KRoundBra
  , KInstr "not"
  , KReg 13
  , KReg 12
  , KReg 12
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 13
  , KNum 4
  , KNum 5
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KLoad
  , KReg 15
  , KId "k"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 16
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 17
  , KReg 15
  , KReg 16
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "k"
  , KReg 17
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 18
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 19
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KInstr "div"
  , KReg 20
  , KReg 18
  , KReg 19
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "n"
  , KReg 20
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 45
  , KReg 19
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 21
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 21
  , KNum 6
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 5
  , KRoundBra
  , KLoad
  , KReg 22
  , KId "result"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 23
  , KId "k"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 24
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KInstr "gt"
  , KReg 25
  , KReg 23
  , KReg 24
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 26
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 27
  , KId "k"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 28
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 29
  , KReg 27
  , KReg 28
  , KRoundKet
  , KRoundBra
  , KInstr "pow"
  , KReg 30
  , KReg 26
  , KReg 29
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 31
  , KReg 25
  , KReg 30
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 32
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 33
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 34
  , KReg 32
  , KReg 33
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 35
  , KReg 31
  , KReg 34
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 36
  , KReg 22
  , KReg 35
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "result"
  , KReg 36
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 37
  , KId "k"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 38
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KInstr "gt"
  , KReg 39
  , KReg 37
  , KReg 38
  , KRoundKet
  , KRoundBra
  , KInstr "not"
  , KReg 40
  , KReg 39
  , KReg 39
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "k"
  , KReg 40
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 41
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 42
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 43
  , KReg 41
  , KReg 42
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "i"
  , KReg 43
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 44
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 45
  , KId "prime"
  , KReg 44
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "p"
  , KReg 45
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 45
  , KReg 44
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 46
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 46
  , KNum 6
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 6
  , KRoundBra
  , KConst
  , KReg 46
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 46
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 7
  , KRoundBra
  , KLoad
  , KReg 47
  , KId "result"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 48
  , KId "k"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 49
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KInstr "gt"
  , KReg 50
  , KReg 48
  , KReg 49
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 51
  , KReg 47
  , KReg 50
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 52
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 53
  , KId "k"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 54
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 55
  , KReg 53
  , KReg 54
  , KRoundKet
  , KRoundBra
  , KInstr "pow"
  , KReg 56
  , KReg 52
  , KReg 55
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 57
  , KReg 51
  , KReg 56
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 58
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 59
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 60
  , KReg 58
  , KReg 59
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 61
  , KReg 57
  , KReg 60
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 61
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "gcd"
  , KRoundBra
  , KId "a"
  , KId "b"
  , KRoundKet
  , KRoundBra
  , KNum 0
  , KRoundBra
  , KConst
  , KReg 0
  , KNum 0
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
  , KId "a"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 2
  , KId "b"
  , KRoundKet
  , KRoundBra
  , KInstr "mod"
  , KReg 3
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KInstr "not"
  , KReg 4
  , KReg 3
  , KReg 3
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
  , KLoad
  , KReg 6
  , KId "b"
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 14
  , KReg 6
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 8
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 8
  , KNum 4
  , KNum 4
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KLoad
  , KReg 9
  , KId "b"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 10
  , KId "a"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 11
  , KId "b"
  , KRoundKet
  , KRoundBra
  , KInstr "mod"
  , KReg 12
  , KReg 10
  , KReg 11
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 13
  , KId "gcd"
  , KReg 9
  , KReg 12
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 14
  , KReg 13
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 15
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 15
  , KNum 4
  , KNum 4
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KReturn
  , KReg 14
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "triangular"
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
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 3
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 4
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 5
  , KReg 3
  , KReg 4
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 6
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KInstr "div"
  , KReg 7
  , KReg 5
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 7
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "ackermann"
  , KRoundBra
  , KId "m"
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
  , KId "m"
  , KRoundKet
  , KRoundBra
  , KInstr "not"
  , KReg 2
  , KReg 1
  , KReg 1
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 2
  , KNum 2
  , KNum 3
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 4
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 5
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 6
  , KReg 4
  , KReg 5
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 29
  , KReg 6
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 8
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 8
  , KNum 7
  , KNum 7
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
  , KInstr "not"
  , KReg 10
  , KReg 9
  , KReg 9
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 10
  , KNum 4
  , KNum 5
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KLoad
  , KReg 12
  , KId "m"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 13
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 14
  , KReg 12
  , KReg 13
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 15
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 16
  , KId "ackermann"
  , KReg 14
  , KReg 15
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 28
  , KReg 16
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 18
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 18
  , KNum 6
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 5
  , KRoundBra
  , KLoad
  , KReg 19
  , KId "m"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 20
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 21
  , KReg 19
  , KReg 20
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 22
  , KId "m"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 23
  , KId "n"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 24
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 25
  , KReg 23
  , KReg 24
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 26
  , KId "ackermann"
  , KReg 22
  , KReg 25
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 27
  , KId "ackermann"
  , KReg 21
  , KReg 26
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 28
  , KReg 27
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 29
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 29
  , KNum 6
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 6
  , KRoundBra
  , KInstr "add"
  , KReg 29
  , KReg 28
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 30
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 30
  , KNum 7
  , KNum 7
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 7
  , KRoundBra
  , KReturn
  , KReg 29
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "uparrow"
  , KRoundBra
  , KId "a"
  , KId "b"
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KNum 0
  , KRoundBra
  , KConst
  , KReg 0
  , KNum 0
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
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 2
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "eq"
  , KReg 3
  , KReg 1
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 3
  , KNum 2
  , KNum 3
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 5
  , KId "a"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 6
  , KId "b"
  , KRoundKet
  , KRoundBra
  , KInstr "pow"
  , KReg 7
  , KReg 5
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 29
  , KReg 7
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 9
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 9
  , KNum 7
  , KNum 7
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KLoad
  , KReg 10
  , KId "b"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 11
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "eq"
  , KReg 12
  , KReg 10
  , KReg 11
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 12
  , KNum 4
  , KNum 5
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KLoad
  , KReg 14
  , KId "a"
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 28
  , KReg 14
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 16
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 16
  , KNum 6
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 5
  , KRoundBra
  , KLoad
  , KReg 17
  , KId "a"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 18
  , KId "a"
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 19
  , KId "b"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 20
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 21
  , KReg 19
  , KReg 20
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 22
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 23
  , KId "uparrow"
  , KReg 18
  , KReg 21
  , KReg 22
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 24
  , KId "p"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 25
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "sub"
  , KReg 26
  , KReg 24
  , KReg 25
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 27
  , KId "uparrow"
  , KReg 17
  , KReg 23
  , KReg 26
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 28
  , KReg 27
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 29
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 29
  , KNum 6
  , KNum 6
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 6
  , KRoundBra
  , KInstr "add"
  , KReg 29
  , KReg 28
  , KReg 0
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 30
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 30
  , KNum 7
  , KNum 7
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 7
  , KRoundBra
  , KReturn
  , KReg 29
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KId "main"
  , KRoundBra
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
  , KId "i"
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
  , KConst
  , KReg 1
  , KNum 4
  , KRoundKet
  , KRoundBra
  , KInstr "neg"
  , KReg 2
  , KReg 1
  , KReg 1
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 3
  , KId "fibonacci"
  , KReg 2
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 4
  , KNum 5
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 5
  , KId "catalan"
  , KReg 4
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 6
  , KNum 256
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 7
  , KId "sqrt"
  , KReg 6
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 8
  , KNum 5
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 9
  , KNum 4
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 10
  , KId "choose"
  , KReg 8
  , KReg 9
  , KRoundKet
  , KRoundBra
  , KPrint
  , KReg 3
  , KReg 5
  , KReg 7
  , KReg 10
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 11
  , KNum 47
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 12
  , KId "isPrime"
  , KReg 11
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 13
  , KNum 20
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 14
  , KId "isPrime"
  , KReg 13
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 15
  , KNum 10
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 16
  , KNum 5
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 17
  , KId "coprime"
  , KReg 15
  , KReg 16
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 18
  , KNum 13
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 19
  , KNum 7
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 20
  , KId "coprime"
  , KReg 18
  , KReg 19
  , KRoundKet
  , KRoundBra
  , KPrint
  , KReg 12
  , KReg 14
  , KReg 17
  , KReg 20
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 21
  , KNum 10
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 22
  , KId "prime"
  , KReg 21
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 23
  , KNum 1024
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 24
  , KId "numPrimeDivisors"
  , KReg 23
  , KRoundKet
  , KRoundBra
  , KPrint
  , KReg 22
  , KReg 24
  , KRoundKet
  , KRoundBra
  , KPrint
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 25
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 25
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 2
  , KRoundBra
  , KLoad
  , KReg 26
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 27
  , KNum 10
  , KRoundKet
  , KRoundBra
  , KInstr "lt"
  , KReg 28
  , KReg 26
  , KReg 27
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 28
  , KNum 3
  , KNum 4
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 3
  , KRoundBra
  , KLoad
  , KReg 30
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 31
  , KId "triangular"
  , KReg 30
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 32
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 33
  , KId "prime"
  , KReg 32
  , KRoundKet
  , KRoundBra
  , KInstr "mul"
  , KReg 34
  , KReg 31
  , KReg 33
  , KRoundKet
  , KRoundBra
  , KPrint
  , KReg 34
  , KRoundKet
  , KRoundBra
  , KLoad
  , KReg 35
  , KId "i"
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 36
  , KNum 1
  , KRoundKet
  , KRoundBra
  , KInstr "add"
  , KReg 37
  , KReg 35
  , KReg 36
  , KRoundKet
  , KRoundBra
  , KStore
  , KId "i"
  , KReg 37
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 38
  , KNum 0
  , KRoundKet
  , KRoundBra
  , KBranch
  , KReg 38
  , KNum 2
  , KNum 2
  , KRoundKet
  , KRoundKet
  , KRoundBra
  , KNum 4
  , KRoundBra
  , KPrint
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 39
  , KNum 13
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 40
  , KNum 299
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 41
  , KId "gcd"
  , KReg 39
  , KReg 40
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 42
  , KNum 20
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 43
  , KNum 45
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 44
  , KId "gcd"
  , KReg 42
  , KReg 43
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 45
  , KNum 36
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 46
  , KId "phi"
  , KReg 45
  , KRoundKet
  , KRoundBra
  , KPrint
  , KReg 41
  , KReg 44
  , KReg 46
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 47
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 48
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 49
  , KId "ackermann"
  , KReg 47
  , KReg 48
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 50
  , KNum 3
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 51
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 52
  , KNum 2
  , KRoundKet
  , KRoundBra
  , KCall
  , KReg 53
  , KId "uparrow"
  , KReg 50
  , KReg 51
  , KReg 52
  , KRoundKet
  , KRoundBra
  , KPrint
  , KReg 49
  , KReg 53
  , KRoundKet
  , KRoundBra
  , KConst
  , KReg 54
  , KNum 123
  , KRoundKet
  , KRoundBra
  , KInstr "neg"
  , KReg 55
  , KReg 54
  , KReg 54
  , KRoundKet
  , KRoundBra
  , KReturn
  , KReg 55
  , KRoundKet
  , KRoundKet
  , KRoundKet
  , KRoundKet
  ]

DIFF
2944,3333c2944
<   , KRoundKet
<   , KRoundBra
<   , KBranch
<   , KReg 0
<   , KNum 1
<   , KNum 1
<   , KRoundKet
<   , KRoundKet
<   , KRoundBra
<   , KNum 1
<   , KRoundBra
<   , KConst
<   , KReg 1
<   , KNum 4
<   , KRoundKet
<   , KRoundBra
<   , KInstr "neg"
<   , KReg 2
<   , KReg 1
<   , KReg 1
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 3
<   , KId "fibonacci"
<   , KReg 2
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 4
<   , KNum 5
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 5
<   , KId "catalan"
<   , KReg 4
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 6
<   , KNum 256
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 7
<   , KId "sqrt"
<   , KReg 6
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 8
<   , KNum 5
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 9
<   , KNum 4
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 10
<   , KId "choose"
<   , KReg 8
<   , KReg 9
<   , KRoundKet
<   , KRoundBra
<   , KPrint
<   , KReg 3
<   , KReg 5
<   , KReg 7
<   , KReg 10
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 11
<   , KNum 47
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 12
<   , KId "isPrime"
<   , KReg 11
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 13
<   , KNum 20
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 14
<   , KId "isPrime"
<   , KReg 13
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 15
<   , KNum 10
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 16
<   , KNum 5
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 17
<   , KId "coprime"
<   , KReg 15
<   , KReg 16
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 18
<   , KNum 13
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 19
<   , KNum 7
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 20
<   , KId "coprime"
<   , KReg 18
<   , KReg 19
<   , KRoundKet
<   , KRoundBra
<   , KPrint
<   , KReg 12
<   , KReg 14
<   , KReg 17
<   , KReg 20
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 21
<   , KNum 10
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 22
<   , KId "prime"
<   , KReg 21
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 23
<   , KNum 1024
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 24
<   , KId "numPrimeDivisors"
<   , KReg 23
<   , KRoundKet
<   , KRoundBra
<   , KPrint
<   , KReg 22
<   , KReg 24
<   , KRoundKet
<   , KRoundBra
<   , KPrint
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 25
<   , KNum 0
<   , KRoundKet
<   , KRoundBra
<   , KBranch
<   , KReg 25
<   , KNum 2
<   , KNum 2
<   , KRoundKet
<   , KRoundKet
<   , KRoundBra
<   , KNum 2
<   , KRoundBra
<   , KLoad
<   , KReg 26
<   , KId "i"
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 27
<   , KNum 10
<   , KRoundKet
<   , KRoundBra
<   , KInstr "lt"
<   , KReg 28
<   , KReg 26
<   , KReg 27
<   , KRoundKet
<   , KRoundBra
<   , KBranch
<   , KReg 28
<   , KNum 3
<   , KNum 4
<   , KRoundKet
<   , KRoundKet
<   , KRoundBra
<   , KNum 3
<   , KRoundBra
<   , KLoad
<   , KReg 30
<   , KId "i"
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 31
<   , KId "triangular"
<   , KReg 30
<   , KRoundKet
<   , KRoundBra
<   , KLoad
<   , KReg 32
<   , KId "i"
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 33
<   , KId "prime"
<   , KReg 32
<   , KRoundKet
<   , KRoundBra
<   , KInstr "mul"
<   , KReg 34
<   , KReg 31
<   , KReg 33
<   , KRoundKet
<   , KRoundBra
<   , KPrint
<   , KReg 34
<   , KRoundKet
<   , KRoundBra
<   , KLoad
<   , KReg 35
<   , KId "i"
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 36
<   , KNum 1
<   , KRoundKet
<   , KRoundBra
<   , KInstr "add"
<   , KReg 37
<   , KReg 35
<   , KReg 36
<   , KRoundKet
<   , KRoundBra
<   , KStore
<   , KId "i"
<   , KReg 37
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 38
<   , KNum 0
<   , KRoundKet
<   , KRoundBra
<   , KBranch
<   , KReg 38
<   , KNum 2
<   , KNum 2
<   , KRoundKet
<   , KRoundKet
<   , KRoundBra
<   , KNum 4
<   , KRoundBra
<   , KPrint
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 39
<   , KNum 13
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 40
<   , KNum 299
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 41
<   , KId "gcd"
<   , KReg 39
<   , KReg 40
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 42
<   , KNum 20
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 43
<   , KNum 45
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 44
<   , KId "gcd"
<   , KReg 42
<   , KReg 43
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 45
<   , KNum 36
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 46
<   , KId "phi"
<   , KReg 45
<   , KRoundKet
<   , KRoundBra
<   , KPrint
<   , KReg 41
<   , KReg 44
<   , KReg 46
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 47
<   , KNum 2
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 48
<   , KNum 2
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 49
<   , KId "ackermann"
<   , KReg 47
<   , KReg 48
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 50
<   , KNum 3
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 51
<   , KNum 2
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 52
<   , KNum 2
<   , KRoundKet
<   , KRoundBra
<   , KCall
<   , KReg 53
<   , KId "uparrow"
<   , KReg 50
<   , KReg 51
<   , KReg 52
<   , KRoundKet
<   , KRoundBra
<   , KPrint
<   , KReg 49
<   , KReg 53
<   , KRoundKet
<   , KRoundBra
<   , KConst
<   , KReg 54
<   , KNum 123
<   , KRoundKet
<   , KRoundBra
<   , KInstr "neg"
<   , KReg 55
<   , KReg 54
<   , KReg 54
<   , KRoundKet
<   , KRoundBra
<   , KReturn
<   , KReg 55
<   , KRoundKet
<   , KRoundKet
<   , KRoundKet
<   , KRoundKet
<   ]
---
>   , KRoundK

