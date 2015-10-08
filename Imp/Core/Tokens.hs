
module Imp.Core.Tokens where

-- | Tokens of the source language.
data Token
        = KNum   Int            -- Number
        | KId    String         -- Identifier
        | KInstr String         -- Instruction
        | KReg   Int            -- Register

        | KRoundBra             -- '('
        | KRoundKet             -- ')'
        | KComma                -- ','
        
        | Kprint                -- print
        | KConst                -- lc
        | KLoad                 -- ld
        | KStore                -- st
        | KBranch               -- br
        | KReturn               -- ret
        | KCall                 -- call
        | KPrint                -- print
        deriving (Show, Eq)


-- | Take a number from a token.
takeNum :: Token -> Maybe Int
takeNum tt
 = case tt of
        KNum n  -> Just n
        _       -> Nothing


-- | Take an identifier from a token.
takeId :: Token -> Maybe String
takeId tt
 = case tt of
        KId s   -> Just s
        _       -> Nothing


takeReg :: Token -> Maybe Int
takeReg tt
 = case tt of
        KReg n  -> Just n
        _       -> Nothing

