
module Imp.Source.Lexer where
import Imp.Source.Tokens
import Imp.Parsec


-- | Lex a string into tokens.
lexer :: String -> Maybe [Token]
lexer str
 = case parse tokens str of
        [(tokens', "")]  -> Just tokens'
        _                -> Nothing


-- | Parse a sequence of tokens,
--   separated by arbitrary white space.
tokens :: Parser Char [Token]
tokens
 = do   some space
        t  <- token
        some space
        ts <- alt tokens (return [])
        some space
        return (t : ts)


-- | Parse a single token.
token :: Parser Char Token
token 
 = altss
        [ do    match str           
                return tok          -- For each (str, tok) pair, attempt
        | (str, tok)    <- atoms]   -- a match

 $ alt  (do     n   <- nat           -- Default parser: no prim match
                return $ KNum n)     -- thus, token is either a num

        (do     str <- many alphanum -- or a variable/function ID
                return $ KId str)


-- | Atomic tokens.
atoms :: [(String, Token)]
atoms
 =      [ -- operators
          ("^",         KOp "^")

        , ("*",         KOp "*")
        , ("/",         KOp "/")
        , ("%",         KOp "%")

        , ("+",         KOp "+")
        , ("-",         KOp "-")

        , ("|",         KOp "|")
        , ("&",         KOp "&")
        , ("x|",        KOp "x|")

        , ("<",         KOp "<")
        , (">",         KOp ">")
        , ("==",        KOp "==")
        , ("!=",        KOp "!=")
        , ("!",         KOp "!")
        

          -- punctuation
        , ("(",         KRoundBra)
        , (")",         KRoundKet)
        , ("{",         KBraceBra)
        , ("}",         KBraceKet)
        , (",",         KComma)
        , (";",         KSemi)
        , ("=",         KEquals)
  
          -- keywords
        , ("fun",       Kfun)
        , ("vars",      Kvars)
        , ("if",        Kif)
        , ("then",      Kthen)
        , ("else",      Kelse)
        , ("return",    Kreturn) 
        , ("while",     Kwhile)
        ] 
