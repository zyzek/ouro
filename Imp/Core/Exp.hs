-- | Intermediate Language Grammar.
module Imp.Core.Exp where


-- | Core programs.
data Program
        = Program [Function]
        deriving Show

-- | Core Functions.
data Function
        = Function Id [Id] [Block] 
        deriving Show 


-- | Core Blocks.
data Block
        = Block Int [Instr]
        deriving Show
        

-- | Instructions.
data Instr
        = IConst        Reg Int                 -- ^ lc
        | ILoad         Reg Id                  -- ^ ld
        | IStore        Id  Reg                 -- ^ st
        | IArith        OpArith Reg Reg Reg     -- ^ arithmetic ops
        | IBranch       Reg Int Int             -- ^ br
        | IReturn       Reg                     -- ^ ret
        | ICall         Reg Id [Reg]            -- ^ call
        | IPrint        [Reg]                   -- ^ print
        deriving Show


-- | Arithmetic operators.
data OpArith
        = OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpMod
        | OpPow
        | OpLt
        | OpGt
        | OpEq
        | OpNeq
        | OpGeq
        | OpLeq
        | OpOr
        | OpAnd
        | OpXor
        | OpNot
        | OpNeg
        deriving Show


-- | Identifiers.
data Id
        = Id String
        deriving (Show, Eq)


-- | Register numbers.
data Reg
        = Reg Int
        deriving (Show, Eq, Ord)