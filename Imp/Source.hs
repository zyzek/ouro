
module Imp.Source where
import Imp.Source.Exp
import Imp.Source.Lexer
import Imp.Source.Tokens
import Imp.Parsec
import qualified Imp.Source.Parser      as PF
import qualified Imp.Source.MinParser   as MPF


-- | Lex a thing, then parse it.
lexParse :: Parser Token a -> String -> Maybe a
lexParse p str
 = case lexer str of
        Nothing         -> Nothing
        Just tokens'
         -> case parse p tokens' of
                 [(x, [])]       -> Just x -- Ambiguous or incomplete parses fail.
                 _               -> Nothing


-- Functions for testing at the ghci command-line.
-- | Parse a string as a program.
programOfString :: String -> Maybe Program
programOfString = lexParse PF.program

-- | Parse an expr as a program.
exprOfString  :: Id -> String -> Maybe Exp
exprOfString curFuncId = lexParse (PF.expr curFuncId)


-- Same as above, but use the minimal grammar.
minProgramOfString :: String -> Maybe Program
minProgramOfString = lexParse MPF.program

minExprOfString :: String -> Maybe Exp
minExprOfString = lexParse MPF.expr
