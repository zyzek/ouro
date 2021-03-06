
module Imp.Source.Lexer where
import Imp.Source.Tokens
import Imp.Parsec


-- | Lex a string into tokens.
lexer :: String -> Maybe [Token]
lexer str
 = case parse tokens str of
        [(tokens', "")]  -> Just tokens'
        _                -> Nothing

-- | Parse a sequence of tokens, separated by whitespace.
-- | Discard code comments.
tokens :: Parser Char [Token]
tokens
 = do   some space
        some quote
        mintokens

-- | As tokens, but disallow comments.
mintokens :: Parser Char [Token]
mintokens
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
                return tok            -- For each (str, tok) pair, attempt
        | (str, tok)    <- atoms]     -- a match

 $ alts [ do     n   <- int           -- Default parser: no prim match
                 return $ KNum n      -- thus, token is either a num

        , do     str <- many alphanum -- or a variable/function ID
                 return $ KId str
        ]


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

        , ("==",        KOp "==")
        , ("<=",        KOp "<=")
        , (">=",        KOp ">=")
        , ("!=",        KOp "!=")

        -- These must appear later than operators they prefix.
        , ("<",         KOp "<")
        , (">",         KOp ">")
        , ("!",         KOp "!")
        

          -- punctuation
        , ("(",         KRoundBra)
        , (")",         KRoundKet)
        , ("{",         KBraceBra)
        , ("}",         KBraceKet)
        , ("[",         KSquareBra)
        , ("]",         KSquareKet)
        , (",",         KComma)
        , (";",         KSemi)
        , ("=",         KEquals)
        , ("@",         KAt)
        , ("?",         KQMark)
  
          -- keywords
        , ("fun",       Kfun)
        , ("vars",      Kvars)
        , ("if",        Kif)
        , ("then",      Kthen)
        , ("else",      Kelse)
        , ("return",    Kreturn) 
        , ("while",     Kwhile)
        , ("true",      Ktrue)
        , ("false",     Kfalse)
        , ("print",     Kprint)
        ] 
