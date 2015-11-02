-- | Intermediate Language Grammar.
module Imp.Opt.Exp where
import Data.List
import Data.Maybe

data CFG
        = CFG Id [Id] [Block] [CFGEdge]
        deriving Show

data CFGEdge
        = CFGEdge Int Int
        deriving (Show, Eq)

data InstrAddr
        = InstrAddr Int Int
        deriving (Show, Eq, Ord)


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
        deriving (Show, Eq, Ord)


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

data InstrDets
        = InstrDets [(Reg, [InstrAddr])] [(Id, [InstrAddr])]
        deriving (Show, Eq, Ord)

getRegDet :: InstrDets -> Reg -> [InstrAddr]
getRegDet (InstrDets regDets _) reg
 = fromMaybe [] (lookup reg regDets)

getVarDet :: InstrDets -> Id -> [InstrAddr]
getVarDet (InstrDets _ varDets) vId
 = fromMaybe [] (lookup vId varDets)

setRegDets :: (Reg, [InstrAddr]) -> InstrDets -> InstrDets
setRegDets regDet@(reg, _) (InstrDets regDets varDets)
 = InstrDets (regDet : filter(\(r, _) -> r /= reg) regDets) varDets

setVarDets :: (Id, [InstrAddr]) -> InstrDets -> InstrDets
setVarDets varDet@(vId, _) (InstrDets regDets varDets)
 = InstrDets regDets (varDet : filter(\(i, _) -> i /= vId) varDets)

addRegDets :: (Reg, [InstrAddr]) -> InstrDets -> InstrDets
addRegDets regDet@(reg, adrs) (InstrDets regDets varDets)
 = case lookup reg regDets of
        Just prevAdrs -> let newAdrs
                              = rmDups (prevAdrs ++ adrs)
                         in InstrDets ((reg, newAdrs) : filter (\(r, _) -> r /= reg) regDets) varDets
        _             -> InstrDets (regDet:regDets) varDets

addVarDets :: (Id, [InstrAddr]) -> InstrDets -> InstrDets
addVarDets varDet@(vId, adrs) (InstrDets regDets varDets)
 = case lookup vId varDets of
        Just prevAdrs -> let newAdrs
                              = rmDups (prevAdrs ++ adrs)
                         in InstrDets regDets ((vId, newAdrs) : filter (\(i, _) -> i /= vId) varDets)
        _             -> InstrDets regDets (varDet:varDets)

mergeRegDets :: [(Reg, [InstrAddr])] -> [(Reg, [InstrAddr])] -> [(Reg, [InstrAddr])]
mergeRegDets a b
 = case a of
        []  -> b
        rd:rds -> mergeRegDets rds (addRegDs rd b)
 where addRegDs regDet@(reg, adrs) regDets
        = case lookup reg regDets of
               Just prevAdrs -> let newAdrs
                                     = rmDups (prevAdrs ++ adrs)
                                in (reg, newAdrs) : filter (\(r, _) -> r /= reg) regDets
               _             -> regDet:regDets


mergeVarDets :: [(Id, [InstrAddr])] -> [(Id, [InstrAddr])] -> [(Id, [InstrAddr])]
mergeVarDets a b
 = case a of
        []  -> b
        vd:vds -> mergeVarDets vds (addVarDs vd b)
 where addVarDs varDet@(vId, adrs) varDets
        = case lookup vId varDets of
               Just prevAdrs -> let newAdrs
                                     = rmDups (prevAdrs ++ adrs)
                                in (vId, newAdrs) : filter (\(i, _) -> i /= vId) varDets
               _             -> varDet:varDets


mergeInstrDets :: InstrDets -> InstrDets -> InstrDets
mergeInstrDets (InstrDets rd vd) (InstrDets rd' vd')
 = InstrDets (mergeRegDets rd rd') (mergeVarDets vd vd')




xDetsEqual :: Ord a => [(a, [InstrAddr])] -> [(a, [InstrAddr])] -> Bool
xDetsEqual p q
 = let sa = sort p
       sb = sort q
   in case sa of
           [] -> case sb of
                      [] -> True
                      _  -> False
           (ar, aadrs):as -> case sb of
                                  (br, badrs):bs -> ar == br
                                                     && sort aadrs == sort badrs
                                                     && xDetsEqual as bs
                                  []             -> False

instrDetsEqual :: InstrDets -> InstrDets -> Bool
instrDetsEqual (InstrDets rd vd) (InstrDets rd' vd')
 = xDetsEqual rd rd' && xDetsEqual vd vd'


lookupInstr :: [Block] -> InstrAddr -> InstrNode
lookupInstr blks tAddr@(InstrAddr bId _)
 = let (Block _ instrs) = fromJust (find (\(Block b _) -> b == bId) blks)
   in fromJust (find (\(InstrNode _ addr _ _) -> tAddr == addr) instrs)

removeAddrFromInstr :: [Block] -> InstrAddr -> InstrAddr -> [Block]
removeAddrFromInstr blks tAddr@(InstrAddr bId _) remove
 = let (pre, Block _ instrs, post) 
        = spanElem (\(Block b _) -> bId == b) blks
       (ipre, InstrNode inst _ ins outs , ipost) 
        = spanElem (\(InstrNode _ addr _ _) -> addr == tAddr) instrs
       newInstrs 
        = ipre 
           ++ [InstrNode inst tAddr (filter (/= remove) ins) (filter (/= remove) outs)]
           ++ ipost
   in pre ++ [Block bId newInstrs] ++ post
   

removeInstr :: [Block] -> InstrAddr -> [Block]
removeInstr blks tAddr@(InstrAddr bId _)
 = let (InstrNode _ _ ins outs) = lookupInstr blks tAddr
       danglers = ins ++ outs
       (pre, Block _ instrs, post) = spanElem (\(Block b _) -> bId == b) blks
       newBlks = pre ++ [Block bId (filter (\(InstrNode _ addr _ _) -> addr /= tAddr) instrs)] ++ post
       rmEdges bs targets remove
        = case targets of
               []   -> bs
               t:ts -> rmEdges (removeAddrFromInstr bs t remove) ts remove
   in rmEdges newBlks danglers tAddr
 
            
            

rmDups :: Ord a => [a] -> [a]
rmDups l = map head $ group $ sort l

spanElem :: (a -> Bool) -> [a] -> ([a], a, [a])
spanElem preds sequ
 = let (pre, post) = span preds sequ
   in (pre, head post, tail post)
 
