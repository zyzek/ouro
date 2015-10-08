
module Imp.Core where
import Imp.Core.Exp
import Imp.Core.Lexer
import Imp.Core.Tokens
import Imp.Parsec
import Imp.Core.Parser


-- | Lex a thing, then parse it.
lexParse :: Parser Token a -> String -> Maybe a
lexParse p str
 = case lexer str of
        Nothing         -> Nothing
        Just tokens'
         -> case parse p tokens' of
                 [(x, [])]       -> Just x
                 _               -> Nothing


-- Functions for testing at the ghci command-line.
-- | Parse a string as a program.
programOfString :: String -> Maybe Program
programOfString = lexParse program
