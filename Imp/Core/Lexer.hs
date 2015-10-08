
module Imp.Core.Lexer where
import Imp.Core.Tokens
import Imp.Parsec


-- | Lex a string into tokens.
lexer :: String -> Maybe [Token]
lexer str
 = case parse tokens str of
        [(tokens', "")]  -> Just tokens'
        _                -> Nothing


-- | Parse a sequence of tokens, separated by whitespace.
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
                return tok
        | (str, tok)    <- atoms]

 $ alts [ do     n   <- int
                 return $ KNum n
        
        , do     char 'r'
                 n   <- nat
                 return $ KReg n
            
        , do     str <- many alphanum 
                 return $ KId str
        ]


-- | Atomic tokens.
atoms :: [(String, Token)]
atoms
 =      [ -- instructions
          ("print",  Kprint)
        , ("lc",     KConst)
        , ("ld",     KLoad)
        , ("st",     KStore)
        , ("br",     KBranch)
        , ("ret",    KReturn)
        , ("call",   KCall)

          -- punctuation
        , ("(",         KRoundBra)
        , (")",         KRoundKet)
        , (",",         KComma)
        
          -- operators
        , ("add",    KInstr "add")
        , ("sub",    KInstr "sub")
        , ("mul",    KInstr "mul")
        , ("div",    KInstr "div")
        , ("mod",    KInstr "mod")
        , ("pow",    KInstr "pow") 
        , ("lt",      KInstr "lt")
        , ("gt",      KInstr "gt")
        , ("eq",      KInstr "eq")
        , ("neq",    KInstr "neq")
        , ("geq",    KInstr "geq")
        , ("leq",    KInstr "leq")
        , ("or",      KInstr "or")
        , ("and",    KInstr "and")
        , ("xor",    KInstr "xor")
        , ("not",    KInstr "not")
        , ("neg",    KInstr "neg")
        ]

