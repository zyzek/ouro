
module Imp.Source.Check where
import Imp.Source.Check.Main
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Run all the available semantic checks on a program.
checkProgram :: Program -> ([Error], [Warning])
checkProgram program
 = let (reacherr, reachwrn) = checkProgReachability program
   in  (checkMain      program
        ++ checkFuncRedef program
        ++ checkVarRedef  program
        ++ checkIds       program
        ++ reacherr
       ,
        reachwrn
       )

-- | Don't check for a main function.
checkLibrary :: Program -> ([Error], [Warning])
checkLibrary program
 = let (reacherr, reachwrn) = checkProgReachability program
   in  (checkFuncRedef     program
        ++ checkVarRedef  program
        ++ checkIds       program
        ++ reacherr
       ,
        reachwrn
       )

