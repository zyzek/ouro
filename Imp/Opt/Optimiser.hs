
module Imp.Opt.Optimiser where
import Imp.Opt.Exp
import Imp.Core.Exp hiding (Block)
import Data.List
import qualified Imp.Core.Interpreter   as I

-- | Graph building.

genGraphEdges :: CFG -> CFG
genGraphEdges (CFG name args blks edges)
 = let (newBlks, blkDets, _) = graphInEdges edges blks [] [0]
       fBlks = setBlkInstrDets newBlks blkDets
   in CFG name args (graphOutEdges fBlks) edges


graphOutEdges :: [Block] -> [Block]
graphOutEdges blks
 = let gOuts 
        = graphOuts blks
       setInstrOuts iNode@(InstrNode i adr ins _)
        = case lookup adr gOuts of
               Just newOuts -> InstrNode i adr ins newOuts
               _            -> iNode
       setBlockOuts (Block bId instrs dets)
        = Block bId (map setInstrOuts instrs) dets
   in map setBlockOuts blks

graphOuts :: [Block] -> [(InstrAddr, [InstrAddr])]
graphOuts blks
 = let instEdges (InstrNode _ adr ins _)
        = map (\o -> (o, adr)) ins
       blkIncoming (Block _ instrs _)
        = concatMap instEdges instrs
       pairs 
        = concatMap blkIncoming blks
       commonOrigin (a, _) (a', _) 
        = a == a'
   in map ((\(a, b) -> (head a, rmDups b)) . unzip) $ groupBy commonOrigin $ sort pairs


setBlkInstrDets :: [Block] -> [(Int, InstrDets)] -> [Block]
setBlkInstrDets blks blkDets
 = case blkDets of 
        []           -> blks
        (i, dets):ds -> let (pre, Block bId instrs _, post)
                             = breakElem (\(Block b _ _) -> b == i) blks
                            newBlks 
                             = pre 
                                ++ [Block bId instrs dets]
                                ++ post
                        in setBlkInstrDets newBlks ds


graphInEdges :: [CFGEdge] -> [Block] -> [(Int, InstrDets)] -> [Int] -> ([Block], [(Int, InstrDets)], [Int])
graphInEdges edges blks blkInstrDets queue
 = case queue of
        []    -> (blks, blkInstrDets, queue)
        b:bs  -> let (pre, thisBlock, post)
                      = breakElem (\(Block bid _ _) -> bid == b) blks
                     (instrDets, _)
                      = getInstrDets b
                     (newBlock, newInstrDets)
                      = blockInstrEdges thisBlock instrDets
                     queueItems 
                      = filter (isNewOrDiff newInstrDets) $ map (\(CFGEdge _ d) -> d) $ edgesFrom edges b
                     newqueue 
                      = bs ++ queueItems
                     newBlks 
                      = pre ++ [newBlock] ++ post
                     queueInstrDets 
                      = zip queueItems $ map (mergeInstrDets newInstrDets . fst . getInstrDets) queueItems
                     newBlkInstrDets 
                      = queueInstrDets ++ filter (\(i, _) -> i `notElem` queueItems) blkInstrDets
                 in graphInEdges edges newBlks newBlkInstrDets newqueue
 where getInstrDets blkId
        = case lookup blkId blkInstrDets of
               Just rd -> (rd, False)
               _       -> (InstrDets [] [], True)
       isNewOrDiff rd blkId
        = let (res, new) = getInstrDets blkId
          in new || not (instrDetsEqual rd res)


blockInstrEdges :: Block -> InstrDets -> (Block, InstrDets)
blockInstrEdges (Block bid instrs dets) instrDets
 = let (newInstrs, newInstrDets)
        = instrEdges instrs instrDets
   in (Block bid newInstrs dets, newInstrDets)

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

updateInstrDets :: InstrNode -> InstrDets -> InstrDets
updateInstrDets (InstrNode inst addr _ _) instrDets
 = case inst of
        IConst reg v  -> setRegDets (reg, ([addr], VNum v)) instrDets
        ILoad  reg vId  -> let varVal = getVarVal instrDets vId  
                               val = case varVal of
                                          VBot -> VVar vId
                                          VTop -> VVar vId
                                          v    -> v
                               newDets = case varVal of
                                              VBot -> setVarDets (vId, ([], VTop)) instrDets
                                              _    -> instrDets
                           in setRegDets (reg, ([addr], val)) newDets
        IStore vId reg  -> let val = getRegVal instrDets reg
                           in setVarDets (vId, ([addr], val)) instrDets
        IArith op rout rin1 rin2 -> let val1 = getRegVal instrDets rin1
                                        val2 = getRegVal instrDets rin2
                                        outval = if valIsNum val1 && valIsNum val2
                                                  then VNum (I.arithCalc op (getValNum val1) (getValNum val2))
                                                  else VTop
                                       
                                    in setRegDets (rout, ([addr], outval)) instrDets
        ICall reg _ _ -> setRegDets (reg, ([addr], VTop)) instrDets
        _             -> instrDets
 where valIsNum v
        = case v of
               VNum _ -> True
               _      -> False
       getValNum val 
        = case val of
               VNum v -> v
               _      -> 666


instrEdge :: InstrNode -> InstrDets -> (InstrNode, InstrDets)
instrEdge instrN@(InstrNode inst _ _ _) instrDets
 = let newInstrDets = updateInstrDets instrN instrDets
       newInstrN
        = case inst of
               IConst{}             ->  instrN
               ILoad  _ vId         ->  setInstrIns instrN (getVarD vId)
               IStore _ reg         -> setInstrIns instrN (getRegD reg)
               IArith _ _ rin1 rin2 -> setInstrIns instrN (rmDups (concatMap getRegD [rin1, rin2]))
               IBranch reg _ _      -> setInstrIns instrN (getRegD reg)
               IReturn reg          -> setInstrIns instrN (getRegD reg)
               ICall _ _ rlist      -> let detList 
                                            = rmDups $ concatMap getRegD rlist
                                       in setInstrIns instrN detList
               IPrint rlist         -> let detList
                                            = rmDups $ concatMap getRegD rlist
                                       in setInstrIns instrN detList
   in (newInstrN, newInstrDets)
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
 = let keptBlocks = filter (\(Block i _ _) -> i `elem` toRetain) blocks
       keptEdges  = filter (\(CFGEdge i j)  -> i `elem` toRetain || j `elem` toRetain) edges
   in CFG name args keptBlocks keptEdges

removeUnreachedInstrs :: CFG -> CFG
removeUnreachedInstrs (CFG cId args blks edges)
 = let instrReached (InstrNode _ _ ins outs)
        = not (null ins && null outs)
       removeInBlk (Block bId instrs dets)
        = Block bId (filter instrReached instrs) dets
  in CFG cId args (map removeInBlk blks) edges


-- | Dead Code Elimination
removeDeadCode :: CFG -> CFG
removeDeadCode (CFG cID args blks edges)
 = let used      = findAllUsed blks
       transused = backClosure blks used []
       compl     = instrComplement blks transused
       trimmed   = removeAllInstr blks compl
   in CFG cID args trimmed edges
       

instrComplement :: [Block] -> [InstrAddr] -> [InstrAddr]
instrComplement blks initSet
 = let blockCompInstrs (Block _ instrs _)
        = filter (`notElem` initSet) $ map (\(InstrNode _ addr _ _) -> addr) instrs
   in concatMap blockCompInstrs blks

backClosure :: [Block] -> [InstrAddr] -> [InstrAddr] -> [InstrAddr]
backClosure blks queue visited
 = case queue of 
        []      -> []
        q:qs    -> let ancestors
                        = filter (\e -> e `notElem` qs && e `notElem` visited) (reachedFrom q)
                       reachedFrom n
                        = case lookupInstr blks n of
                               Just (InstrNode _ _ ins _) -> ins
                               _                          -> []
                   in q : backClosure blks (ancestors ++ qs) (q : visited)

       
findAllUsed :: [Block] -> [InstrAddr]
findAllUsed blks
 = map extractAddress $ concatMap (\(Block _ instrs _) -> filter isUseInstr instrs) blks
 where isUseInstr (InstrNode i _ _ _)
        = case i of
               IBranch{}    -> True
               IReturn _    -> True
               IPrint _     -> True
               _            -> False
       extractAddress (InstrNode _ addr _ _)
        = addr
        

-- | Redundant Instruction Elimination

removeRedundantInstrs :: CFG -> CFG
removeRedundantInstrs (CFG name args blks edges)
 = let newBlks = removeRedundantBlkInstrs blks blockIds
   in genGraphEdges $ CFG name args newBlks edges
   where blockIds = map (\(Block i _ _) -> i) blks


removeRedundantBlkInstrs :: [Block] -> [Int] -> [Block]
removeRedundantBlkInstrs blks bIds
 = case bIds of
        []   -> blks
        i:is -> let (_, Block _ instrs dets, _)
                     = breakElem (\(Block b _ _) -> b == i) blks
                    (newBlks, _, _) = handleRedundantInstrs blks dets instrs
                in removeRedundantBlkInstrs newBlks  is

handleRedundantInstrs :: [Block] -> InstrDets -> [InstrNode] -> ([Block], InstrDets, [InstrNode])
handleRedundantInstrs blks dets instrs
 = case instrs of
        []
         -> (blks, dets, [])
        instrN@(InstrNode inst addr@(InstrAddr bId _) ins outs):is
         -> let (pre, Block b bInstrs bDets, post)
                 = breakElem (\(Block i _ _) -> i == bId) (removeAddrsFromOuts blks ins addr)
                (iPre, _, iPost)
                 = breakElem (\(InstrNode _ iAddr _ _) -> iAddr == addr) bInstrs
            in case inst of
                    (ILoad reg vId) -> case getVarPair dets vId of
                                            (_, VNum n) 
                                             -> let newInstr 
                                                     = InstrNode (IConst reg n) addr [] outs
                                                    newBlks 
                                                     = pre 
                                                       ++ [Block b (iPre ++ [newInstr] ++ iPost) bDets]
                                                       ++ post
                                                in (newBlks, updateInstrDets newInstr dets, is)
                                            (detAddrs, VVar newId) 
                                             -> let newInstr 
                                                     = InstrNode (ILoad reg newId) addr detAddrs outs
                                                    newBlks
                                                     = pre
                                                       ++ [Block b (iPre ++ [newInstr] ++ iPost) bDets]
                                                       ++ post
                                                in (addOutEdges newBlks detAddrs addr,
                                                    updateInstrDets newInstr dets, is)
                                            _ -> (blks, updateInstrDets instrN dets, is)
                    (IBranch reg b1 b2) -> case getRegVal dets reg of
                                                VNum 0 
                                                 -> let newIns = getRegDet dets (Reg b2)
                                                        newInstr 
                                                         = InstrNode (IBranch reg b2 b2) addr newIns outs
                                                        newBlks
                                                         = pre
                                                            ++ [Block b (iPre ++ [newInstr] ++ iPost) bDets]
                                                            ++ post
                                                    in (addOutEdges newBlks newIns addr,
                                                        updateInstrDets newInstr dets, is)
                                                VNum _ 
                                                 -> let newIns = getRegDet dets (Reg b1)
                                                        newInstr 
                                                         = InstrNode (IBranch reg b1 b1) addr newIns outs
                                                        newBlks
                                                         = pre
                                                            ++ [Block b (iPre ++ [newInstr] ++ iPost) bDets]
                                                            ++ post
                                                    in (addOutEdges newBlks newIns addr,
                                                        updateInstrDets newInstr dets, is)

                                                _      -> (blks, updateInstrDets instrN dets, is)
                    _             -> (blks, updateInstrDets instrN dets, is)
                    
                
       



