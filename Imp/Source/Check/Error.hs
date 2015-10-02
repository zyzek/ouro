
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
        | ErrorSyntax
        deriving Eq


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
         -> "Function '" ++ i ++ "' expects " ++ (show n) ++ " argument(s)." 
        ErrorVarUndef i
         -> "Variable '" ++ i ++ "' undefined."
        ErrorVarRedef i
         -> "Variable '" ++ i ++ "' redefined."
        ErrorPolyAssign v e
         -> (show e) ++ " expressions assigned to " ++ (show v) ++ " variables in poly-assignment."
        ErrorSyntax
         -> "Syntax Error."
