
module Imp.Source.Check.Error where


-- | Problems we might find in the input program.
data Error
        = ErrorNoMain
        | ErrorFuncUndef    String
        | ErrorFuncRedef    String
        | ErrorFuncSig      String Int
        | ErrorVarUndef     String
        | ErrorVarRedef     String
        | ErrorSyntax


-- | Pretty print an error.
prettyError :: Error -> String
prettyError err
 = case err of
        ErrorNoMain     
         -> "No main function defined."
        ErrorFuncUndef i
         -> "function '" ++ i ++ "' undefined."
        ErrorFuncRedef i
         -> "function '" ++ i ++ "' redefined."
        ErrorFuncSig i n
         -> "function '" ++ i ++ "' expects" ++ (show n) ++ " argument(s)." 
        ErrorVarUndef i
         -> "variable '" ++ i ++ "' undefined."
        ErrorVarRedef i
         -> "variable '" ++ i ++ "' redefined."
        ErrorSyntax
         -> "Syntax Error."
