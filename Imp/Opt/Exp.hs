-- | Intermediate Language Grammar.
module Imp.Opt.Exp where
import Data.List
import Data.Maybe
import Imp.Core.Exp hiding (Block)
-- import Debug.Trace

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


lookupInstr :: [Block] -> InstrAddr -> Maybe InstrNode
lookupInstr blks tAddr@(InstrAddr bId _)
 = let (Block _ instrs) = fromMaybe (Block (-1) []) (find (\(Block b _) -> b == bId) blks)
   in find (\(InstrNode _ addr _ _) -> tAddr == addr) instrs

removeAddrFromInstr :: [Block] -> InstrAddr -> InstrAddr -> [Block]
removeAddrFromInstr blks tAddr@(InstrAddr bId _) remove
 = let (pre, Block rBId instrs, post) 
        = breakElem (\(Block b _) -> bId == b) blks
       (ipre, InstrNode inst riAddr ins outs , ipost) 
        = breakElem (\(InstrNode _ addr _ _) -> addr == tAddr) instrs
       newInstrs 
        = ipre 
           ++ [InstrNode inst riAddr (filter (/= remove) ins) (filter (/= remove) outs)]
           ++ ipost
   in pre ++ [Block rBId newInstrs] ++ post
   

removeInstr :: [Block] -> InstrAddr -> [Block]
removeInstr blks tAddr@(InstrAddr bId _)
 = case lookupInstr blks tAddr of
        Nothing -> blks
        Just (InstrNode _ _ ins outs) 
         -> let danglers
                 = ins ++ outs
                (pre, Block rBId instrs, post) 
                 = breakElem (\(Block b _) -> bId == b) blks
                newBlks
                 = pre ++ [Block rBId (filter (\(InstrNode _ addr _ _) -> addr /= tAddr) instrs)] ++ post
            in rmEdges newBlks danglers tAddr

rmEdges :: [Block] -> [InstrAddr] -> InstrAddr -> [Block]
rmEdges blks targets remove
 = case targets of
        []   -> blks
        t:ts -> rmEdges (removeAddrFromInstr blks t remove) ts remove

removeAllInstr :: [Block] -> [InstrAddr] -> [Block]
removeAllInstr blks addrs
 = case addrs of
        []      -> blks
        a:as    -> removeAllInstr (removeInstr blks a) as


rmDups :: Ord a => [a] -> [a]
rmDups l = map head $ group $ sort l

breakElem :: (a -> Bool) -> [a] -> ([a], a, [a])
breakElem preds sequ
 = let (pre, post) = break preds sequ
   in (pre, head post, tail post)
 
