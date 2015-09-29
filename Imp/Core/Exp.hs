-- | Intermediate Language Grammar.
module Imp.Core.Exp where


-- | Core programs.
data Program
        = Program [Function]
        deriving Show

-- | Core Functions.
data Function
        = Function { fId :: Id, fIds :: [Id], fBlocks :: [Block] } 
        deriving Show 


-- | Core Blocks.
data Block
        = Block { bId :: Int, bInstrs :: [Instr] }  -- (Instr:[Instr])
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