
module Imp.Source.Tokens where


-- | Tokens of the source language.
data Token
        = KNum  Int             -- Number
        | KId   String          -- Identifier
        | KOp   String          -- Operators

        -- Punctuation
        | KRoundBra             -- '('
        | KRoundKet             -- ')'
        | KBraceBra             -- '{'
        | KBraceKet             -- '}'

        | KComma                -- ','
        | KSemi                 -- ';'
        | KEquals               -- '='

        -- Keywords
        | Kfun                  -- fun
        | Kvars                 -- vars
        | Kif                   -- if
        | Kthen                 -- then
        | Kelse                 -- else
        | Kreturn               -- return
        | Kwhile                -- while
        | Ktrue                 -- true
        | Kfalse                -- false
        deriving (Show, Eq)


-- | Take a number from a token.
takeNum :: Token -> Maybe Int
takeNum tt
 = case tt of
        KNum n  -> Just n
        Ktrue   -> Just 1
        Kfalse  -> Just 0
        _       -> Nothing


-- | Take an identifier from a token.
takeId :: Token -> Maybe String
takeId tt
 = case tt of
        KId s   -> Just s
        _       -> Nothing

