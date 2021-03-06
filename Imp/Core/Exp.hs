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
        deriving (Show, Eq)


-- | Arithmetic operators.
data OpArith
        -- Arithmetical Operators
        = OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpMod
        | OpPow

        -- Equalities and Inequalities
        | OpLt
        | OpGt
        | OpEq
        | OpNeq
        | OpGeq
        | OpLeq

        -- Logical Operators
        | OpOr
        | OpAnd
        | OpXor

        -- Unaries (both OpArith source Regs are taken to be the same register in the unary case)
        | OpNot
        | OpNeg
        deriving (Show, Eq)


-- | Identifiers.
data Id
        = Id String
        deriving (Show, Eq, Ord)


-- | Extract the string from an Id.
strOfId :: Id -> String
strOfId (Id s) = s


-- | Register numbers.
data Reg
        = Reg Int
        deriving (Show, Eq, Ord)

