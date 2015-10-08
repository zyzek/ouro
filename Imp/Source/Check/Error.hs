
module Imp.Source.Check.Error where


-- | Problems we might find in the input program.
data Error
        = ErrorNoMain
        | ErrorFuncUndef    String
        | ErrorFuncRedef    String
        | ErrorFuncSig      String Int
        | ErrorVarUndef     String
        | ErrorVarRedef     String
        | ErrorPolyAssign   Int Int
        | ErrorNoReturn     String
        | ErrorSyntax
        deriving (Show, Eq)

data Warning 
        = WarningUnreachableAfter String
        | WarningUnreachableBranch String
        | WarningRedundantConditional String
        | WarningInfiniteLoop String
        | FinalReturn
        deriving (Show, Eq)

-- | Pretty print an error.
prettyError :: Error -> String
prettyError err
 = case err of
        ErrorNoMain     
         -> "No main function defined."
        ErrorFuncUndef i
         -> "Function '" ++ i ++ "' undefined."
        ErrorFuncRedef i
         -> "Function '" ++ i ++ "' redefined."
        ErrorFuncSig i n
         -> "Function '" ++ i ++ "' expects " ++ show n ++ " argument(s)." 
        ErrorVarUndef i
         -> "Variable '" ++ i ++ "' undefined."
        ErrorVarRedef i
         -> "Variable '" ++ i ++ "' redefined."
        ErrorPolyAssign v e
         -> show e ++ " expressions assigned to " ++ show v ++ " variables in poly-assignment."
        ErrorNoReturn s
         -> "Function " ++ s ++ " does not return."
        ErrorSyntax
         -> "Syntax Error."

prettyWarn :: Warning -> String
prettyWarn wrn
 = case wrn of
        WarningUnreachableAfter s
         -> "Unreachable code after " ++ s
        WarningUnreachableBranch s
         -> "Branch never executes: " ++ s
        WarningInfiniteLoop s
         -> "Infinite loop in " ++ s
        WarningRedundantConditional s
         -> "Redundant conditional in " ++ s
        FinalReturn
         -> ""

