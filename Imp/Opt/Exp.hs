-- | Intermediate Language Grammar.
module Imp.Opt.Exp where


data CFG
        = CFG Id [Id] [Block] [CFGEdge]
        deriving Show

data CFGEdge
        = CFGEdge Int Int
        deriving (Show, Eq)

data InstrAddr
        = InstrAddr Int Int
        deriving (Show, Eq, Ord)

data Val
        = Top
        | Bot
        | Val Int
        deriving (Show, Eq)

-- | Core Blocks; block id, instruction list, 
data Block
        = Block Int [InstrNode]
        deriving Show
        

-- |Instruction, Instruction id (block, instr), [predecessors] [descendents]
-- These are the nodes of a graph, encoding which instructions determine the value of which
-- other instructions.
data InstrNode
        = InstrNode Instr InstrAddr [InstrAddr] [InstrAddr]
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
        deriving Show


-- | Identifiers.
data Id
        = Id String
        deriving (Show, Eq)


-- | Extract the string from an Id.
strOfId :: Id -> String
strOfId (Id s) = s


-- | Register numbers.
data Reg
        = Reg Int
        deriving (Show, Eq, Ord)


edgesFrom :: [CFGEdge] -> Int -> [CFGEdge]
edgesFrom edges orig
 = filter (\(CFGEdge o _) -> o == orig) edges

edgesTo :: [CFGEdge] -> Int -> [CFGEdge]
edgesTo edges dest
 = filter (\(CFGEdge _ d) -> d == dest) edges
