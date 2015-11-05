
-- | Flow Graph Manipulation Utilities
module Imp.Opt.CFGUtils where
import Imp.Opt.CFG
import Imp.Core.Exp hiding (Block)

import Data.List
import Data.Maybe


-- | Dets-Manipulation Functions ========================================

-- | Getters

-- | Get both addresses of determining instructions and the value of a receptacle.
getRegPair :: InstrDets -> Reg -> ([InstrAddr], Val)
getRegPair (InstrDets regDets _) reg
 = fromMaybe ([], VBot) (lookup reg regDets)

getVarPair :: InstrDets -> Id -> ([InstrAddr], Val)
getVarPair (InstrDets _ varDets) vId
 = fromMaybe ([], VBot) (lookup vId varDets)

-- | Just the Val.
getRegVal :: InstrDets -> Reg -> Val
getRegVal dets reg
 = snd $ getRegPair dets reg

getVarVal :: InstrDets -> Id -> Val
getVarVal dets vId
 = snd $ getVarPair dets vId

-- | Just the addresses.
getRegDet :: InstrDets -> Reg -> [InstrAddr]
getRegDet dets reg
 = fst $ getRegPair dets reg

getVarDet :: InstrDets -> Id -> [InstrAddr]
getVarDet dets vId
 = fst $ getVarPair dets vId


-- | Setters

-- | Set the Dets for a particular receptacle.
setRegDets :: (Reg, ([InstrAddr], Val)) -> InstrDets -> InstrDets
setRegDets regDet@(reg, _) (InstrDets regDets varDets)
 = InstrDets (regDet : filter(\(r, _) -> r /= reg) regDets) varDets

setVarDets :: (Id, ([InstrAddr], Val)) -> InstrDets -> InstrDets
setVarDets varDet@(vId, _) (InstrDets regDets varDets)
 = InstrDets regDets (varDet : filter(\(i, _) -> i /= vId) varDets)

-- | As above, but for a list of receptacles.
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

-- | Like the setters above, but merge the lists and join the vals, rather than overwriting.
-- | These are required for if the value of a receptacle is determined from more than one
-- |  parent branch.
addRegDets :: (Reg, ([InstrAddr], Val)) -> InstrDets -> InstrDets
addRegDets regDet@(reg, (adrs, val)) (InstrDets regDets varDets)
 = case lookup reg regDets of
        Just (prevAdrs, pVal)
         -> let newAdrs
                 = rmDups (prevAdrs ++ adrs)
                newRegDets
                 = (reg, (newAdrs, vJoin val pVal)) : filter (\(r, _) -> r /= reg) regDets
            in InstrDets newRegDets  varDets
        _
         -> InstrDets (regDet:regDets) varDets

addVarDets :: (Id, ([InstrAddr], Val)) -> InstrDets -> InstrDets
addVarDets varDet@(vId, (adrs, val)) (InstrDets regDets varDets)
 = case lookup vId varDets of
        Just (prevAdrs, pVal)
         -> let newAdrs
                 = rmDups (prevAdrs ++ adrs)
                newVarDets
                 = (vId, (newAdrs, vJoin val pVal)) : filter (\(i, _) -> i /= vId) varDets
            in InstrDets regDets newVarDets
        _
         -> InstrDets regDets (varDet:varDets)


-- | Merge two Dets structures, as if each element of one was added to the other.
mergeInstrDets :: InstrDets -> InstrDets -> InstrDets
mergeInstrDets (InstrDets rd vd) (InstrDets rd' vd')
 = InstrDets (mergeXDets (Reg 666) rd rd') (mergeXDets (Id "__ERR__")  vd vd')

mergeXDets :: Ord a => a 
           -> [(a, ([InstrAddr], Val))] -> [(a, ([InstrAddr], Val))] -> [(a, ([InstrAddr], Val))]
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


-- | If a variable's value changes, then any other variables or registers which contain
-- | a VVar of this variable's id are now out of date. Each such VVar must become a VTop,
-- | as at the time the VVar was assigned, the variable must have had indeterminate value.
derefVar :: InstrDets -> Id -> InstrDets
derefVar dets vId
 = let regs = map (setDetVal VTop) (detRegsByValue dets (VVar vId))
       vars = map (setDetVal VTop) (detVarsByValue dets (VVar vId))
   in setAllRegDets regs (setAllVarDets vars dets)
 where setDetVal v (a, (l, _)) = (a, (l, v))

                                                                            
-- | Determine if two Dets structures are equal.
-- | Note that VTops evaluate to equal, though though might contain distinct values.
instrDetsEqual :: InstrDets -> InstrDets -> Bool
instrDetsEqual (InstrDets rd vd) (InstrDets rd' vd')
 = xDetsEqual rd rd' && xDetsEqual vd vd'

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


-- | Find all receptacles that contain a certain value.
detRegsByValue :: InstrDets -> Val -> [(Reg, ([InstrAddr], Val))]
detRegsByValue (InstrDets regDets _) val
 = filter (\(_, (_, v)) -> val == v) regDets

detVarsByValue :: InstrDets -> Val -> [(Id, ([InstrAddr], Val))]
detVarsByValue (InstrDets _ varDets) val
 = filter (\(_, (_, v)) -> val == v) varDets

-- | Find all registers that contain any constant value.
constRegs :: InstrDets -> [(Reg, ([InstrAddr], Val))]
constRegs (InstrDets regDets _)
 = filter (\(_, (_, v)) -> valIsNum v) regDets



-- | Block and Node Manipulation Functions  ========================================

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


setNodeIns :: InstrNode -> [InstrAddr] -> InstrNode
setNodeIns (InstrNode inst addr _ outs) newIns
 = InstrNode inst addr newIns outs

setNodeOuts :: InstrNode -> [InstrAddr] -> InstrNode
setNodeOuts (InstrNode inst addr ins _)
 = InstrNode inst addr ins

setNodeAddr :: InstrNode -> InstrAddr -> InstrNode
setNodeAddr (InstrNode inst _ ins outs) addr
 = InstrNode inst addr ins outs

setNodeInstr :: InstrNode -> Instr -> InstrNode
setNodeInstr (InstrNode _ addr ins outs) inst
 = InstrNode inst addr ins outs



-- | Graph Manipulation Functions  ========================================

-- | Given a blocklist, a target address and an address to remove,
-- | purge it from the target node's in- and out-neighbourhoods.
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

-- | As above, but only remove from the out-neighbourhood.
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

-- | As above, but may take more than one target.
removeAddrsFromOuts :: [Block] -> [InstrAddr] -> InstrAddr -> [Block]
removeAddrsFromOuts blks targets remove
 = case targets of
        []   -> blks
        t:ts -> removeAddrsFromOuts (removeAddrFromInstrOuts blks t remove) ts remove

rmEdges :: [Block] -> [InstrAddr] -> InstrAddr -> [Block]
rmEdges blks targets remove
 = case targets of
        []   -> blks
        t:ts -> rmEdges (removeAddrFromInstr blks t remove) ts remove


-- | Remove a given instruction from the graph entirely, also removing dangling edges.
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

-- | Remove a list of instructions from the graph.
removeAllInstr :: [Block] -> [InstrAddr] -> [Block]
removeAllInstr blks addrs
 = case addrs of
        []      -> blks
        a:as    -> removeAllInstr (removeInstr blks a) as


-- | Add an address to the out-neighbourhood of an instruction.
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


-- Regenerate the inter-block edges in a CFG.
regenCFGEdges :: CFG -> CFG
regenCFGEdges (CFG name args blks _)
 = CFG name args blks $ regenBlkEdges blks

regenBlkEdges :: [Block] -> [(Int, Int)]
regenBlkEdges blks
 = let extractBlkEdges (Block bId _ _ _ branches) 
        = map (\i -> (bId, i)) branches
   in rmDups $ concatMap extractBlkEdges blks


-- Readdress all instructions. This may be necessary if blocks have been merged.
setCFGAddrs :: CFG -> CFG
setCFGAddrs (CFG i args blks edges)
 = CFG i args (map setBlockAddrs blks) edges

setBlockAddrs :: Block -> Block
setBlockAddrs blk@(Block i instrnodes _ _ _)
 = let aPairs
        = zip [0..] instrnodes
   in setBlkInstrs blk (map (\(n, instrN) -> setNodeAddr instrN (InstrAddr i n)) aPairs)


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

-- | If a block is not terminated by either a return or a branch, 
-- | we should add a branch to the next block in the list, if possible.
-- | this should be performed immediately after parsing IR source.
-- | Any such unterminated blocks have only -1 in their branch list.
branchUnterminated :: CFG -> CFG
branchUnterminated (CFG name args blks edges)
 = let (newBlks, correctedEdges)
        = branchUnterminatedBlocks blks []
       newEdges
        = correctedEdges ++ filter (\(_, d) -> d /= -1) edges
   in CFG name args newBlks newEdges

branchUnterminatedBlocks :: [Block] -> [(Int, Int)] -> ([Block], [(Int, Int)])
branchUnterminatedBlocks blks correctedEdges
 = let (pre, post)
        = break (\(Block _ _ _ _ br) -> br == [-1]) blks
   in case post of 
           [] -> (blks, correctedEdges)
           [blk] ->  (pre ++ [setBlkBranches blk []], correctedEdges) 
           thisBlk@(Block tbId instrs _ _ _):nBlk@(Block nbId _ _ _ _):rest
            -> let lastAddr = case instrs of 
                                   [] -> -1
                                   _  -> extractNodeAddr (last instrs)
                   jmpInstrs = [ InstrNode 
                                  (IConst (Reg 666) 0) 
                                  (InstrAddr tbId (lastAddr + 1)) 
                                  [] [InstrAddr tbId (lastAddr + 2)]
                               , InstrNode 
                                  (IBranch (Reg 666) nbId nbId)
                                  (InstrAddr tbId (lastAddr + 2))
                                  [InstrAddr tbId (lastAddr + 1)] [] ]
                   newBlk = setBlkInstrs thisBlk (instrs ++ jmpInstrs)
                   newEdge = (tbId, nbId)
                   (newRest, newCorrected)
                    = branchUnterminatedBlocks (nBlk:rest) (newEdge:correctedEdges)
               in ( pre 
                     ++ [setBlkBranches newBlk [nbId]]
                     ++ newRest
                  , newCorrected)
 where extractNodeAddr (InstrNode _ (InstrAddr _ q)  _ _) = q



-- | Miscellaneous Utility Functions  ========================================

rmDups :: Ord a => [a] -> [a]
rmDups l = map head $ group $ sort l

breakElem :: (a -> Bool) -> [a] -> ([a], a, [a])
breakElem preds sequ
 = let (pre, post) = break preds sequ
   in (pre, head post, tail post)

edgesFrom :: [(Int, Int)] -> Int -> [(Int, Int)]
edgesFrom edges orig
 = filter (\(o, _) -> o == orig) edges

edgesTo :: [(Int, Int)] -> Int -> [(Int, Int)]
edgesTo edges dest
 = filter (\(_, d) -> d == dest) edges

sortByKeys :: Ord a => [(a, b)] -> [(a, b)]
sortByKeys al
 = let cmp (a1, _) (a2, _) = compare a1 a2
   in sortBy cmp al

