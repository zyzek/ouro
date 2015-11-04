-- | Intermediate Language Grammar.
module Imp.Opt.Exp where
import Data.List
import Data.Maybe
import Imp.Core.Exp hiding (Block)

data CFG
        = CFG Id [Id] [Block] [(Int, Int)]
        deriving (Show, Eq)

data InstrAddr
        = InstrAddr Int Int
        deriving (Show, Eq, Ord)


-- | Core Blocks; block id, instruction list, 
data Block
        = Block Int [InstrNode] InstrDets InstrDets [Int]
        deriving (Show, Eq)
        

-- |Instruction, Instruction id (block, instr), [predecessors] [descendents]
-- These are the nodes of a graph, encoding which instructions determine the value of which
-- other instructions.
data InstrNode
        = InstrNode Instr InstrAddr [InstrAddr] [InstrAddr]
        deriving (Show, Eq)

data InstrDets
        = InstrDets [(Reg, ([InstrAddr], Val))] [(Id, ([InstrAddr], Val))]
        deriving (Show, Eq, Ord)

data Val
       = VTop
       | VVar Id
       | VNum Int
       | VBot
       deriving (Show, Eq)

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

edgesFrom :: [(Int, Int)] -> Int -> [(Int, Int)]
edgesFrom edges orig
 = filter (\(o, _) -> o == orig) edges

edgesTo :: [(Int, Int)] -> Int -> [(Int, Int)]
edgesTo edges dest
 = filter (\(_, d) -> d == dest) edges

getRegPair :: InstrDets -> Reg -> ([InstrAddr], Val)
getRegPair (InstrDets regDets _) reg
 = fromMaybe ([], VBot) (lookup reg regDets)

getVarPair :: InstrDets -> Id -> ([InstrAddr], Val)
getVarPair (InstrDets _ varDets) vId
 = fromMaybe ([], VBot) (lookup vId varDets)

getRegVal :: InstrDets -> Reg -> Val
getRegVal dets reg
 = snd $ getRegPair dets reg

getVarVal :: InstrDets -> Id -> Val
getVarVal dets vId
 = snd $ getVarPair dets vId

getRegDet :: InstrDets -> Reg -> [InstrAddr]
getRegDet dets reg
 = fst $ getRegPair dets reg

getVarDet :: InstrDets -> Id -> [InstrAddr]
getVarDet dets vId
 = fst $ getVarPair dets vId

setRegDets :: (Reg, ([InstrAddr], Val)) -> InstrDets -> InstrDets
setRegDets regDet@(reg, _) (InstrDets regDets varDets)
 = InstrDets (regDet : filter(\(r, _) -> r /= reg) regDets) varDets

setVarDets :: (Id, ([InstrAddr], Val)) -> InstrDets -> InstrDets
setVarDets varDet@(vId, _) (InstrDets regDets varDets)
 = InstrDets regDets (varDet : filter(\(i, _) -> i /= vId) varDets)

setAllRegDets :: [(Reg, ([InstrAddr], Val))] -> InstrDets -> InstrDets
setAllRegDets rDets dets
 = case rDets of
        []   -> dets
        r:rs -> setAllRegDets rs (setRegDets r dets)

setAllVarDets :: [(Id, ([InstrAddr], Val))] -> InstrDets -> InstrDets
setAllVarDets vDets dets
 = case vDets of
        []   -> dets
        v:vs -> setAllVarDets vs (setVarDets v dets)

addRegDets :: (Reg, ([InstrAddr], Val)) -> InstrDets -> InstrDets
addRegDets regDet@(reg, (adrs, val)) (InstrDets regDets varDets)
 = case lookup reg regDets of
        Just (prevAdrs, pVal) -> let newAdrs
                                      = rmDups (prevAdrs ++ adrs)
                                 in InstrDets ((reg, (newAdrs, vJoin val pVal)) : filter (\(r, _) -> r /= reg) regDets) varDets
        _             -> InstrDets (regDet:regDets) varDets

addVarDets :: (Id, ([InstrAddr], Val)) -> InstrDets -> InstrDets
addVarDets varDet@(vId, (adrs, val)) (InstrDets regDets varDets)
 = case lookup vId varDets of
        Just (prevAdrs, pVal) -> let newAdrs
                                      = rmDups (prevAdrs ++ adrs)
                                 in InstrDets regDets ((vId, (newAdrs, vJoin val pVal)) : filter (\(i, _) -> i /= vId) varDets)
        _             -> InstrDets regDets (varDet:varDets)

mergeXDets :: Ord a => a -> [(a, ([InstrAddr], Val))] -> [(a, ([InstrAddr], Val))] -> [(a, ([InstrAddr], Val))]
mergeXDets err a b
 = let merged = groupBy (\(a1, _) (a2, _) -> a1 == a2) $ sortByKeys  (a ++ b)
       mergeDetList l = case l of
                             (r, (a1, v1)):(_, (a2, v2)):_ 
                               -> let newAdrs = rmDups (a1 ++ a2)
                                      newVal = vJoin v1 v2
                                  in (r, (newAdrs, newVal))
                             (r, (addr, _)):_
                               -> (r, (addr, VTop))
                             _ -> (err, ([], VBot))
  in map mergeDetList merged

derefVar :: InstrDets -> Id -> InstrDets
derefVar dets vId
 = let regs = map (setDetVal VTop) (detRegsByValue dets (VVar vId))
       vars = map (setDetVal VTop) (detVarsByValue dets (VVar vId))
   in setAllRegDets regs (setAllVarDets vars dets)
 where setDetVal v (a, (l, _)) = (a, (l, v))


sortByKeys :: Ord a => [(a, b)] -> [(a, b)]
sortByKeys al
 = let cmp (a1, _) (a2, _) = compare a1 a2
   in sortBy cmp al
                                                                            
mergeInstrDets :: InstrDets -> InstrDets -> InstrDets
mergeInstrDets (InstrDets rd vd) (InstrDets rd' vd')
 = InstrDets (mergeXDets (Reg 666) rd rd') (mergeXDets (Id "__ERR__")  vd vd')


xDetsEqual :: Ord a => [(a, ([InstrAddr], Val))] -> [(a, ([InstrAddr], Val))] -> Bool
xDetsEqual p q
 = let sa = sort p
       sb = sort q
   in case sa of
           [] -> case sb of
                      [] -> True
                      _  -> False
           (ar, (aadrs, aval)):as -> case sb of
                                          (br, (badrs, bval)):bs -> ar == br
                                                                     && aval == bval
                                                                     && sort aadrs == sort badrs
                                                                     && xDetsEqual as bs
                                          []             -> False

detRegsByValue :: InstrDets -> Val -> [(Reg, ([InstrAddr], Val))]
detRegsByValue (InstrDets regDets _) val
 = filter (\(_, (_, v)) -> val == v) regDets

detVarsByValue :: InstrDets -> Val -> [(Id, ([InstrAddr], Val))]
detVarsByValue (InstrDets _ varDets) val
 = filter (\(_, (_, v)) -> val == v) varDets


instrDetsEqual :: InstrDets -> InstrDets -> Bool
instrDetsEqual (InstrDets rd vd) (InstrDets rd' vd')
 = xDetsEqual rd rd' && xDetsEqual vd vd'

deadBlock :: Block 
deadBlock = Block (-1) [] (InstrDets [] []) (InstrDets [] []) []

setBlkInstrs :: Block -> [InstrNode] -> Block
setBlkInstrs (Block bId _ preDets postDets branches) newInstrs
 = Block bId newInstrs preDets postDets branches

setBlkBranches :: Block -> [Int] -> Block
setBlkBranches (Block bId instrs preDets postDets _)
 = Block bId instrs preDets postDets

setBlkPreDets :: Block -> InstrDets -> Block
setBlkPreDets (Block bId instrs _ postDets branches) preDets
 = Block bId instrs preDets postDets branches

lookupInstr :: [Block] -> InstrAddr -> Maybe InstrNode
lookupInstr blks tAddr@(InstrAddr bId _)
 = let (Block _ instrs _ _ _) 
        = fromMaybe deadBlock (find (\(Block b _ _ _ _) -> b == bId) blks)
   in find (\(InstrNode _ addr _ _) -> tAddr == addr) instrs

removeAddrFromInstr :: [Block] -> InstrAddr -> InstrAddr -> [Block]
removeAddrFromInstr blks tAddr@(InstrAddr bId _) remove
 = let (pre, blk@(Block _ instrs _ _ _), post) 
        = breakElem (\(Block b _ _ _ _) -> bId == b) blks
       (ipre, InstrNode inst riAddr ins outs , ipost) 
        = breakElem (\(InstrNode _ addr _ _) -> addr == tAddr) instrs
       newInstrs 
        = ipre 
           ++ [InstrNode inst riAddr (filter (/= remove) ins) (filter (/= remove) outs)]
           ++ ipost
   in pre ++ [setBlkInstrs blk newInstrs] ++ post


removeAddrFromInstrOuts :: [Block] -> InstrAddr -> InstrAddr -> [Block]
removeAddrFromInstrOuts blks tAddr@(InstrAddr bId _) remove
 = let (pre, blk@(Block _ instrs _ _ _), post) 
        = breakElem (\(Block b _ _ _ _) -> bId == b) blks
       (ipre, InstrNode inst riAddr ins outs , ipost) 
        = breakElem (\(InstrNode _ addr _ _) -> addr == tAddr) instrs
       newInstrs 
        = ipre 
           ++ [InstrNode inst riAddr ins (filter (/= remove) outs)]
           ++ ipost
   in pre ++ [setBlkInstrs blk newInstrs] ++ post

removeAddrsFromOuts :: [Block] -> [InstrAddr] -> InstrAddr -> [Block]
removeAddrsFromOuts blks targets remove
 = case targets of
        []   -> blks
        t:ts -> removeAddrsFromOuts (removeAddrFromInstrOuts blks t remove) ts remove


removeInstr :: [Block] -> InstrAddr -> [Block]
removeInstr blks tAddr@(InstrAddr bId _)
 = case lookupInstr blks tAddr of
        Nothing -> blks
        Just (InstrNode _ _ ins outs) 
         -> let danglers
                 = ins ++ outs
                (pre, blk@(Block _ instrs _ _ _), post) 
                 = breakElem (\(Block b _ _ _ _) -> bId == b) blks
                newBlks
                 = pre 
                    ++ [setBlkInstrs blk (filter (\(InstrNode _ addr _ _) -> addr /= tAddr) instrs)]
                    ++ post
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

setNodeOuts :: InstrNode -> [InstrAddr] -> InstrNode
setNodeOuts (InstrNode inst addr ins _)
 = InstrNode inst addr ins

setNodeAddr :: InstrNode -> InstrAddr -> InstrNode
setNodeAddr (InstrNode inst _ ins outs) addr
 = InstrNode inst addr ins outs

setNodeInstr :: InstrNode -> Instr -> InstrNode
setNodeInstr (InstrNode _ addr ins outs) inst
 = InstrNode inst addr ins outs

addOutEdge :: [Block] -> InstrAddr -> InstrAddr -> [Block]
addOutEdge blks adr@(InstrAddr adrb _) new
 = let (bpre, blk@(Block _ instrnodes _ _ _), bpost)
        = breakElem (\(Block b _ _ _ _) -> b == adrb) blks
       (ipre, node@(InstrNode _ _ _ outs), ipost)
        = breakElem (\(InstrNode _ n _ _) -> n == adr) instrnodes
       newOuts
        = if new `elem` outs then outs else new:outs
       newInstrs 
        = (ipre ++ [setNodeOuts node newOuts] ++ ipost)
   in bpre
       ++ [setBlkInstrs blk newInstrs]
       ++ bpost

addOutEdges :: [Block] -> [InstrAddr] -> InstrAddr -> [Block]
addOutEdges blks targets new
 = case targets of
        []   -> blks
        t:ts -> addOutEdges (addOutEdge blks t new) ts new


regenCFGEdges :: CFG -> CFG
regenCFGEdges (CFG name args blks _)
 = CFG name args blks $ regenBlkEdges blks

regenBlkEdges :: [Block] -> [(Int, Int)]
regenBlkEdges blks
 = let extractBlkEdges (Block bId _ _ _ branches) 
        = map (\i -> (bId, i)) branches
   in rmDups $ concatMap extractBlkEdges blks

setCFGAddrs :: CFG -> CFG
setCFGAddrs (CFG i args blks edges)
 = CFG i args (map setBlockAddrs blks) edges


setBlockAddrs :: Block -> Block
setBlockAddrs blk@(Block i instrnodes _ _ _)
 = let aPairs
        = zip [0..] instrnodes
   in setBlkInstrs blk (map (\(n, instrN) -> setNodeAddr instrN (InstrAddr i n)) aPairs)

valIsNum :: Val -> Bool
valIsNum v = case v of
                  VNum _ -> True
                  _      -> False

getValNum :: Val -> Int
getValNum val = case val of
                     VNum v -> v
                     _      -> 666

-- | CFG and instruction graph functions.

-- | Given a block, determine which blocks it might branch to.
-- | Only check up to the first return or branch instruction.
blockBranches :: Block -> Block
blockBranches blk@(Block _ instrnodes _ _ _)
 = let (rpre, rpost)
        = break isRet instrnodes
       brs
        = case find isBranch rpre of
               Just (InstrNode (IBranch _ j k) _ _ _) -> nub [j, k]
               _ -> []
       brsNoRet
        | null rpost && null brs = [-1]
        | otherwise              = brs
   in setBlkBranches blk brsNoRet
 where isRet (InstrNode i _ _ _)
        = case i of
               IReturn _     -> True
               _             -> False
       isBranch (InstrNode i _ _ _)
        = case i of
               IBranch{} -> True
               _             -> False 


