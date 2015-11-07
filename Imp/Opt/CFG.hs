
-- | CFG Structure
module Imp.Opt.CFG where
import Imp.Core.Exp hiding (Block)


-- | A CFG, representing a function.
-- | Contains the same information as the underlying function,
-- | plus a list of the edges between blocks (determined by branches).
data CFG
        = CFG Id [Id] [Block] [(Int, Int)]
        deriving (Show, Eq)


-- | CFG Block.
-- | A block contains its ID, instruction list,
-- | the values in registers and variables at both the start and end of the block,
-- | a list of block IDs which this block branches to.
data Block
        = Block Int [InstrNode] InstrDets InstrDets [Int]
        deriving (Show, Eq)

deadBlock :: Block 
deadBlock = Block (-1) [] (InstrDets [] []) (InstrDets [] []) []


-- A node of the flow graph.
-- Each node contains an instruction, its address,
-- the addresses of its in-neighbours and out-neighbours
data InstrNode
        = InstrNode Instr InstrAddr [InstrAddr] [InstrAddr]
        deriving (Show, Eq)


-- | The address of an instruction:
-- | The first int is which block it belongs to; the second, its position within the block.
data InstrAddr
        = InstrAddr Int Int
        deriving (Show, Eq, Ord)


-- | This structure represents the program state at a given point in time.
-- | It contains two associative lists, one for registers, and one for variables.
-- | Each is a mapping from a given receptacle to its current value, and list of addresses 
-- | of the instructions which possibly determine it.
data InstrDets
        = InstrDets [(Reg, ([InstrAddr], Val))] [(Id, ([InstrAddr], Val))]
        deriving (Show, Eq, Ord)

-- | Values form a lattice.
-- | VTop: Intederminate, multiple possible values.
-- | VVar: The (indeterminate) value of some variable.
-- | VNum: A Numeric constant.
-- | VBot: Uninitialised.
data Val
       = VTop
       | VVar Id
       | VNum Int
       | VBot
       deriving (Show, Eq)


-- | For the purpose of ordering, Top and Bot are of course min and max values.
-- | A variable is considered to be larger than a constant -- though this choice is arbitrary.
instance Ord Val where
    _ <= VTop        = True
    VTop <= VVar _   = False
    VTop <= VNum _   = False
    VTop <= VBot     = False
    
    VBot <= _        = True
    VNum _ <= VBot   = False
    VVar _ <= VBot   = False

    VNum _ <= VVar _ = True
    VNum a <= VNum b = a <= b
    
    VVar a <= VVar b = a <= b
    VVar _ <= VNum _ = False


vJoin :: Val -> Val -> Val
vJoin a b 
 = case a of
        VTop -> VTop
        VBot -> b
        _    -> case b of
                     VTop -> VTop
                     VBot -> a
                     _    -> if a == b then a else VTop

vMeet :: Val -> Val -> Val
vMeet a b
 = case a of
        VBot -> VBot
        VTop -> b
        _    -> case b of
                     VBot -> VBot
                     VTop -> a
                     _    -> if a == b then a else VTop


valIsNum :: Val -> Bool
valIsNum v = case v of
                  VNum _ -> True
                  _      -> False

getValNum :: Val -> Int
getValNum val = case val of
                     VNum v -> v
                     _      -> 666

