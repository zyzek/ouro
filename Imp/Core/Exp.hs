-- | Intermediate Language Grammar.
module Imp.Core.Exp where


-- | Core programs.
data Program
        = Program [Function]
        deriving Show

-- | Core Functions.
data Function
        = Function Id [Id] Block:[Block] -- ?? Intended to specify that this list is non-empty.
        deriving Show

-- | Core Blocks.
data Block
        = Block Int Instr:[Instr]


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
        | OpDiv -- ?? This was excluded before. Forestall div by 0?
        | OpLt
        | OpGt
        | OpEq
        deriving Show


-- | Identifiers.
data Id
        = Id String
        deriving (Show, Eq)


-- | Register numbers.
data Reg
        = Reg Int
        deriving (Show, Eq)