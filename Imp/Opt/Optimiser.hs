
module Imp.Opt.Optimiser where
import Imp.Opt.Exp
import Data.List

-- | Graph building.

genGraphEdges :: CFG -> CFG
genGraphEdges (CFG name args blks edges)
 = let (newBlks, _, _) = graphInEdges edges blks [] [0]
   in CFG name args (graphOutEdges newBlks) edges


graphOutEdges :: [Block] -> [Block]
graphOutEdges blks
 = let gOuts 
        = graphOuts blks
       setInstrOuts iNode@(InstrNode i adr ins _)
        = case lookup adr gOuts of
               Just newOuts -> InstrNode i adr ins newOuts
               _            -> iNode
       setBlockOuts (Block bId instrs)
        = Block bId $ map setInstrOuts instrs
   in map setBlockOuts blks

graphOuts :: [Block] -> [(InstrAddr, [InstrAddr])]
graphOuts blks
 = let instEdges (InstrNode _ adr ins _)
        = map (\o -> (o, adr)) ins
       blkIncoming (Block _ instrs)
        = concatMap instEdges instrs
       pairs 
        = concatMap blkIncoming blks
       commonOrigin (a, _) (a', _) 
        = a == a'
   in map ((\(a, b) -> (head a, rmDups b)) . unzip) $ groupBy commonOrigin $ sort pairs


graphInEdges :: [CFGEdge] -> [Block] -> [(Int, InstrDets)] -> [Int] -> ([Block], [(Int, InstrDets)], [Int])
graphInEdges edges blks blkInstrDets queue
 = case queue of
        []    -> (blks, blkInstrDets, queue)
        b:bs  -> let (pre, thisBlock, post) = spanElem (\(Block bid _) -> bid /= b) blks
                     (instrDets, _) = getInstrDets b
                     (newBlock, newInstrDets) = blockInstrEdges thisBlock instrDets
                     queueItems = filter (isNewOrDiff newInstrDets) $ map (\(CFGEdge _ d) -> d) $ edgesFrom edges b
                     newqueue = bs ++ queueItems
                     newBlks = pre ++ [newBlock] ++ post
                     queueInstrDets = zip queueItems $ map (mergeInstrDets newInstrDets . fst . getInstrDets) queueItems
                     newBlkInstrDets =  queueInstrDets ++ filter (\(i, _) -> i `notElem` queueItems) blkInstrDets
                 in graphInEdges edges newBlks newBlkInstrDets newqueue
 where getInstrDets blkId
        = case lookup blkId blkInstrDets of
               Just rd -> (rd, False)
               _       -> (InstrDets [] [], True)
       isNewOrDiff rd blkId
        = let (res, new) = getInstrDets blkId
          in new || not (instrDetsEqual rd res)






blockInstrEdges :: Block -> InstrDets -> (Block, InstrDets)
blockInstrEdges (Block bid instrs) instrDets
 = let (newInstrs, newInstrDets)
        = instrEdges instrs instrDets
   in (Block bid newInstrs, newInstrDets)

instrEdges :: [InstrNode] -> InstrDets -> ([InstrNode], InstrDets)
instrEdges instrs instrDets
 = case instrs of
        []      -> ([], instrDets)
        i:is    -> let (newI, newInstrDets)
                        = instrEdge i instrDets
                       (fIs, fInstrDets)
                        = if isBranchOrRet i 
                             then (is, newInstrDets)
                             else instrEdges is newInstrDets
                   in (newI:fIs, fInstrDets)
 where isBranchOrRet (InstrNode i _ _ _)
        = case i of
               IReturn _ -> True
               IBranch{} -> True
               _         -> False

instrEdge :: InstrNode -> InstrDets -> (InstrNode, InstrDets)
instrEdge instrN@(InstrNode inst addr _ _) instrDets
 = case inst of
        IConst reg _  -> ( instrN
                         , setRegDets (reg, [addr]) instrDets )
        ILoad  reg vId  -> ( setInstrIns instrN (getVarD vId)
                           , setRegDets (reg, [addr]) instrDets )
        IStore vId reg  -> ( setInstrIns instrN (getRegD reg) 
                           , setVarDets (vId, [addr]) instrDets )
        IArith _ rout rin1 rin2 -> ( setInstrIns instrN (rmDups (concatMap getRegD [rin1, rin2]))
                                   , setRegDets (rout, [addr]) instrDets )
        IBranch reg _ _ -> ( setInstrIns instrN (getRegD reg)
                           , instrDets )

        IReturn reg     -> ( setInstrIns instrN (getRegD reg)
                           , instrDets )
        ICall reg _ rlist -> let detList 
                                  = rmDups $ concatMap getRegD rlist
                             in ( setInstrIns instrN detList
                                , setRegDets (reg, [addr]) instrDets )
        IPrint rlist      -> let detList
                                  = rmDups $ concatMap getRegD rlist
                             in ( setInstrIns instrN detList
                                , instrDets )
 where getRegD
        = getRegDet instrDets
       getVarD
        = getVarDet instrDets

setInstrIns :: InstrNode -> [InstrAddr] -> InstrNode
setInstrIns (InstrNode inst addr _ outs) newIns
 = InstrNode inst addr newIns outs


-- | Unreachable block/instruction removal.

zeroClosure :: CFG -> (Id, [Int])
zeroClosure cfg@(CFG i _ _ _) = (i, closure cfg [0])

closure :: CFG -> [Int] -> [Int]
closure cfg nodes
  | nodes == stepped   = nodes 
  | otherwise          = closure cfg stepped
 where stepped = reachStep cfg nodes

reachStep :: CFG -> [Int] -> [Int]
reachStep (CFG _ _ _ edges) reached
 = let isOrig n (CFGEdge o _)
        = o == n
       getDest (CFGEdge _ d)
        = d
       edgeDests n
        = map getDest (filter (isOrig n) edges)
   in rmDups reached ++ concatMap edgeDests reached

-- | Take a CFG, a list of block numbers to keep, 
-- | remove blocks not in the list, edges with endpoints not in the list.
retainBlocks :: CFG -> [Int] -> CFG
retainBlocks (CFG name args blocks edges) toRetain
 = let keptBlocks = filter (\(Block i _) -> i `elem` toRetain) blocks
       keptEdges  = filter (\(CFGEdge i j)  -> i `elem` toRetain || j `elem` toRetain) edges
   in CFG name args keptBlocks keptEdges

removeUnreachedInstrs :: CFG -> CFG
removeUnreachedInstrs (CFG cId args blks edges)
 = let instrReached (InstrNode _ _ ins outs)
        = not (null ins && null outs)
       removeInBlk (Block bId instrs)
        = Block bId (filter instrReached instrs)
  in CFG cId args (map removeInBlk blks) edges


-- | Dead Code Elimination
{-removeDeadCode :: CFG -> CFG
removeDeadCode (CFG cID args blks edges)
 = let used      = findAllUsed blks
       transused = backClosure blks used

instrComplement :: [Block] -> [InstrAddr] -> [InstrAddr]
instrComplement blks a
 = 
-}
backClosure :: [Block] -> [InstrAddr] -> [InstrAddr] -> [InstrAddr]
backClosure blks queue visited
 = case queue of 
        []      -> []
        q:qs    -> let ancestors
                        = filter (\e -> e `notElem` qs && e `notElem` visited) (reachedFrom q)
                       reachedFrom n
                        = let InstrNode _ _ ins _ = lookupInstr blks n
                          in ins
                   in q : backClosure blks (ancestors ++ qs) (q : visited)

       
findAllUsed :: [Block] -> [InstrAddr]
findAllUsed blks
 = map extractAddress $ concatMap (\(Block _ instrs) -> filter isUseInstr instrs) blks
 where isUseInstr (InstrNode i _ _ _)
        = case i of
               IBranch{}    -> True
               IReturn _    -> True
               IPrint _     -> True
               _            -> False
       extractAddress (InstrNode _ addr _ _)
        = addr
        




-- | Redundant Load Elimination


