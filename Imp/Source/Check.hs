
module Imp.Source.Check where
import Imp.Source.Check.Main
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Run all the available semantic checks on a program.
checkProgram :: Program -> [Error]
checkProgram program
        = checkMain program
          ++ checkFuncRedef program
          ++ checkVarRedef  program
          ++ checkIds    program

-- | Don't check for a main function.
checkLibrary :: Program -> [Error]
checkLibrary program
        = checkFuncRedef program
          ++ checkVarRedef  program
          ++ checkIds    program
