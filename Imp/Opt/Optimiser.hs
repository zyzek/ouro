
module Imp.Opt.Optimiser where
import Imp.Opt.Exp
import Imp.Core.Exp hiding (Block)
import Data.List
import Data.List.Utils
import qualified Imp.Core.Interpreter   as I

-- | Graph building.

genGraphEdges :: CFG -> CFG
genGraphEdges (CFG name args blks edges)
 = let newBlks = graphInEdges edges blks [0]
   in CFG name args (graphOutEdges newBlks) edges


graphOutEdges :: [Block] -> [Block]
graphOutEdges blks
 = let gOuts 
        = graphOuts blks
       setInstrOuts iNode@(InstrNode i adr ins _)
        = case lookup adr gOuts of
               Just newOuts -> InstrNode i adr ins newOuts
               _            -> iNode
       setBlockOuts (Block bId instrs preDets postDets)
        = Block bId (map setInstrOuts instrs) preDets postDets
   in map setBlockOuts blks

graphOuts :: [Block] -> [(InstrAddr, [InstrAddr])]
graphOuts blks
 = let instEdges (InstrNode _ adr ins _)
        = map (\o -> (o, adr)) ins
       blkIncoming (Block _ instrs _ _)
        = concatMap instEdges instrs
       pairs 
        = concatMap blkIncoming blks
       commonOrigin (a, _) (a', _) 
        = a == a'
   in map ((\(a, b) -> (head a, rmDups b)) . unzip) $ groupBy commonOrigin $ sort pairs


graphInEdges :: [CFGEdge] -> [Block] -> [Int] -> [Block]
graphInEdges edges blks queue
 = case queue of
        []    -> blks
        b:bs  -> let (pre, Block blkId blkInstrs preDets postDets, post)
                      = breakElem (\(Block bid _ _ _) -> bid == b) blks
                     inNeighbours
                      = map edgeHead $ edgesTo edges b
                     newPreDets
                      = case inNeighbours of
                             []  -> preDets
                             [n] -> snd (getInstrDets n)
                             l   -> let d:ds = map (snd . getInstrDets) l
                                    in foldr mergeInstrDets d  ds
                     queueItems 
                      = if null inNeighbours || preDets /= newPreDets
                         then map edgeTail $ edgesFrom edges b
                         else []
                     newqueue 
                      = bs ++ queueItems
                     newBlock
                      = blockInstrEdges (Block blkId blkInstrs newPreDets postDets)
                     newBlks 
                      = pre ++ [newBlock] ++ post
                 in graphInEdges edges newBlks newqueue
 where getInstrDets blkId
        = case find (\(Block bId _ _ _) -> bId == blkId) blks of
               Just (Block _ _ pre post) -> (pre, post)
               _       -> (InstrDets [] [], InstrDets [] [])


blockInstrEdges :: Block -> Block
blockInstrEdges (Block bid instrs preDets _)
 = let (newInstrs, newInstrDets)
        = instrEdges instrs preDets
   in Block bid newInstrs preDets newInstrDets

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
               IConst{}             -> instrN
               ILoad  _ vId         -> setInstrIns instrN (getVarD vId)
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
 = let keptBlocks = filter (\(Block i _ _ _) -> i `elem` toRetain) blocks
       keptEdges  = filter (\(CFGEdge i j)  -> i `elem` toRetain || j `elem` toRetain) edges
   in CFG name args keptBlocks keptEdges

removeUnreachedInstrs :: CFG -> CFG
removeUnreachedInstrs (CFG cId args blks edges)
 = let instrReached (InstrNode _ _ ins outs)
        = not (null ins && null outs)
       removeInBlk (Block bId instrs preDets postDets)
        = Block bId (filter instrReached instrs) preDets postDets
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
 = let blockCompInstrs (Block _ instrs _ _)
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
 = map extractAddress $ concatMap (\(Block _ instrs _ _) -> filter isUseInstr instrs) blks
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
 = let blockIds = map (\(Block i _ _ _) -> i) blks
       newBlks = removeRedundantBlkInstrs blks [] blockIds
   in genGraphEdges $ CFG name args newBlks edges


removeRedundantBlkInstrs :: [Block] -> [InstrAddr] -> [Int] -> [Block]
removeRedundantBlkInstrs blks forbidRewrite bIds
 = case bIds of
        []   -> blks
        i:is -> let (_, Block _ instrs preDets _, _)
                     = breakElem (\(Block b _ _ _) -> b == i) blks
                    (newBlks, newForbids) = redundantInstrs blks preDets instrs forbidRewrite
                in removeRedundantBlkInstrs newBlks newForbids is

redundantInstrs :: [Block] -> InstrDets -> [InstrNode] -> [InstrAddr] -> ([Block], [InstrAddr])
redundantInstrs blks dets instrs forbidRewrite
 = case instrs of
        []
         -> (blks, forbidRewrite)
        instrN@(InstrNode inst addr@(InstrAddr bId _) ins outs):is
         -> let (pre, Block b bInstrs preDets postDets, post)
                 = breakElem (\(Block i _ _ _) -> i == bId) (removeAddrsFromOuts blks ins addr)
                (iPre, _, iPost)
                 = breakElem (\(InstrNode _ iAddr _ _) -> iAddr == addr) bInstrs
            in case inst of
                    (ILoad reg vId) -> case getVarVal dets vId of
                                            VNum n 
                                             -> let newInstr 
                                                     = InstrNode (IConst reg n) addr [] outs
                                                    newBlks 
                                                     = pre 
                                                       ++ [Block b (iPre ++ [newInstr] ++ iPost) preDets postDets]
                                                       ++ post
                                                in redundantInstrs newBlks 
                                                                   (updateInstrDets newInstr dets) 
                                                                   is
                                                                   forbidRewrite
                                            VVar _
                                             -> let (newBlks, success)
                                                     = tryRewriteRegs blks outs (addr:forbidRewrite) reg
                                                    newForbids 
                                                     = if success
                                                        then outs ++ forbidRewrite
                                                        else forbidRewrite
                                                in redundantInstrs newBlks
                                                                   (updateInstrDets instrN dets)
                                                                   is
                                                                   newForbids
                                            _ -> redundantInstrs blks 
                                                                 (updateInstrDets instrN dets) 
                                                                 is
                                                                 forbidRewrite
                    (IBranch reg b1 b2) -> case getRegVal dets reg of
                                                VNum 0 
                                                 -> let newInstr 
                                                         = InstrNode (IBranch reg b2 b2) addr ins outs
                                                        newBlks
                                                         = pre
                                                            ++ [Block b (iPre ++ [newInstr] ++ iPost) preDets postDets]
                                                            ++ post
                                                    in redundantInstrs newBlks 
                                                                       (updateInstrDets newInstr dets)
                                                                       is
                                                                       forbidRewrite
                                                VNum _ 
                                                 -> let newInstr 
                                                         = InstrNode (IBranch reg b1 b1) addr ins outs
                                                        newBlks
                                                         = pre
                                                            ++ [Block b (iPre ++ [newInstr] ++ iPost) preDets postDets]
                                                            ++ post
                                                    in redundantInstrs newBlks 
                                                                       (updateInstrDets newInstr dets)
                                                                       is
                                                                       forbidRewrite
                                                _      -> redundantInstrs blks
                                                                          (updateInstrDets instrN dets)
                                                                          is
                                                                          forbidRewrite
                    _             -> redundantInstrs blks 
                                                     (updateInstrDets instrN dets)
                                                     is
                                                     forbidRewrite
                    

tryRewriteRegs :: [Block] -> [InstrAddr] -> [InstrAddr] -> Reg -> ([Block], Bool)
tryRewriteRegs blks instrsToRewrite forbidRewrite targetReg
 = case instrsToRewrite of
        [] -> (blks, True)
        _  -> let br:_ = rmDups $ map (\(InstrAddr b _) -> b) instrsToRewrite
                  (bInstrs, rest) 
                   = partition (\(InstrAddr b _) -> b == br) instrsToRewrite
                  (rewritten, success) 
                   = rewriteBlock blks br bInstrs forbidRewrite targetReg
              in if success 
                  then tryRewriteRegs rewritten rest forbidRewrite targetReg
                  else (blks, False)

rewriteBlock :: [Block] -> Int -> [InstrAddr] -> [InstrAddr] -> Reg -> ([Block], Bool)
rewriteBlock blks bId instrsToRewrite forbidRewrite targetReg
 = let (_, Block _ bInstrs preDets _, _) = breakElem (\(Block b _ _ _) -> b == bId) blks
       instrAddrs = map (\(InstrNode _ addr _ _) -> addr) bInstrs
   in rewriteBlockInstrs blks bId preDets instrsToRewrite forbidRewrite instrAddrs targetReg

rewriteBlockInstrs :: [Block] -> Int -> InstrDets -> [InstrAddr] -> [InstrAddr] -> [InstrAddr] -> Reg -> ([Block], Bool)
rewriteBlockInstrs blks bId dets instrsToRewrite forbidRewrite blkInstrAddrs targetReg
 = let (pre, Block _ bInstrs preDets postDets, post) = breakElem (\(Block b _ _ _) -> b == bId) blks
   in case instrsToRewrite of
           []   -> (blks, True)
           i:is -> case blkInstrAddrs of
                        []     -> (blks, False)
                        ba:bas -> let (iPre, instrN@(InstrNode _ iAddr ins _), iPost)
                                       = breakElem (\(InstrNode _ addr _ _) -> addr == ba) bInstrs
                                      (newInstr@(InstrNode _ _ newIns _), success) 
                                       = rewriteInstr dets targetReg instrN forbidRewrite
                                      newBlks 
                                       = pre 
                                          ++ [Block bId (iPre ++ [newInstr] ++ iPost) preDets postDets]
                                          ++ post
                                      rBlks 
                                       = addOutEdges (removeAddrsFromOuts newBlks ins iAddr) newIns iAddr
                                      newDets = updateInstrDets newInstr dets
                                      (fBlks, fSuccess) 
                                       = rewriteBlockInstrs rBlks bId newDets is forbidRewrite bas targetReg
                                  in if iAddr `elem` instrsToRewrite
                                      then if success && fSuccess
                                            then (fBlks, True)
                                            else (blks, False)
                                      else rewriteBlockInstrs blks bId 
                                                              (updateInstrDets instrN dets) 
                                                              (i:is)
                                                              forbidRewrite
                                                              bas
                                                              targetReg
           


rewriteInstr :: InstrDets -> Reg -> InstrNode -> [InstrAddr] -> (InstrNode, Bool)
rewriteInstr dets targetReg instrN forbidRewrite
 = let val = getRegVal dets targetReg
   in case val of
           VTop   -> (instrN, False)
           VBot   -> (instrN, False)
           _      -> case filter (\(r, (addrs, _)) -> (r /= targetReg) 
                                                       && not (hasAny addrs forbidRewrite))
                                 (detRegsByValue dets val) of
                          []                      -> (instrN, False)
                          (newReg, _):_ -> let newInstr 
                                                = replaceInstrReg dets targetReg newReg instrN
                                           in (newInstr, True)


replaceInstrReg :: InstrDets -> Reg -> Reg -> InstrNode -> InstrNode
replaceInstrReg dets oldReg newReg instrN@(InstrNode inst addr _ outs)
 = case inst of 
        IStore vId r          -> let newR = ifOldNew r
                                     newIns = newRegDets [newR]
                                 in InstrNode (IStore vId newR) addr newIns outs
        IArith op rout r1 r2  -> let newR1 = ifOldNew r1
                                     newR2 = ifOldNew r2
                                     newIns = newRegDets [newR1, newR2]
                                 in InstrNode (IArith op rout newR1 newR2) addr newIns outs
        IBranch r b1 b2       -> let newR = ifOldNew r
                                     newIns = newRegDets [newR]
                                 in InstrNode (IBranch newR b1 b2) addr newIns outs
        IReturn r             -> let newR = ifOldNew r
                                     newIns = newRegDets [newR]
                                 in InstrNode (IReturn newR) addr newIns outs
        ICall rout vId rs     -> let newRs = map ifOldNew rs
                                     newIns = newRegDets newRs
                                 in InstrNode (ICall rout vId newRs) addr newIns outs
        IPrint rs             -> let newRs = map ifOldNew rs
                                     newIns = newRegDets newRs
                                 in InstrNode (IPrint newRs) addr newIns outs
        _                     -> instrN
 where ifOldNew r
        = if r == oldReg then newReg else r
       newRegDets rs
        = rmDups $ concatMap (getRegDet dets) rs

