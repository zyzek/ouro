
module Imp.Opt.Optimiser where
import Imp.Opt.Exp
import Imp.Core.Exp hiding (Block)
import Data.List
import Data.List.Utils
import Data.Maybe
import qualified Imp.Core.Interpreter   as I

-- | Graph building.

genGraphEdges :: CFG -> CFG
genGraphEdges (CFG name args blks _)
 = let fresh    = wipeGraphEdges blks
       newEdges = regenBlkEdges fresh
       newBlks  = graphInEdges newEdges fresh [0]
   in CFG name args (graphOutEdges newBlks) newEdges

wipeGraphEdges :: [Block] -> [Block]
wipeGraphEdges = map wipeBlockEdges

wipeBlockEdges :: Block -> Block
wipeBlockEdges (Block i instrs _ _ _)
 = let newInstrs = map wipeInstrEdges instrs
   in  blockBranches $ Block i newInstrs (InstrDets [] []) (InstrDets [] []) []

wipeInstrEdges :: InstrNode -> InstrNode
wipeInstrEdges (InstrNode inst addr _ _)
 = InstrNode inst addr [] []


graphOutEdges :: [Block] -> [Block]
graphOutEdges blks
 = let gOuts 
        = graphOuts blks
       setInstrOuts iNode@(InstrNode i adr ins _)
        = case lookup adr gOuts of
               Just newOuts -> InstrNode i adr ins newOuts
               _            -> iNode
       setBlockOuts blk@(Block _ instrs _ _ _)
        = setBlkInstrs blk (map setInstrOuts instrs)
   in map setBlockOuts blks

graphOuts :: [Block] -> [(InstrAddr, [InstrAddr])]
graphOuts blks
 = let instEdges (InstrNode _ adr ins _)
        = map (\o -> (o, adr)) ins
       blkIncoming (Block _ instrs _ _ _)
        = concatMap instEdges instrs
       pairs 
        = concatMap blkIncoming blks
       commonOrigin (a, _) (a', _) 
        = a == a'
   in map ((\(a, b) -> (head a, rmDups b)) . unzip) $ groupBy commonOrigin $ sort pairs


graphInEdges :: [(Int, Int)] -> [Block] -> [Int] -> [Block]
graphInEdges edges blks queue
 = case queue of
        []    -> blks
        b:bs  -> let (pre, blk@(Block _ _ preDets _ _), post)
                      = breakElem (\(Block bid _ _ _ _) -> bid == b) blks
                     inNeighbours
                      = map fst $ edgesTo edges b
                     newPreDets
                      = case inNeighbours of
                             []  -> preDets
                             [n] -> snd (getInstrDets n)
                             l   -> let d:ds = map (snd . getInstrDets) l
                                    in foldr mergeInstrDets d  ds
                     queueItems 
                      = if null inNeighbours || preDets /= newPreDets
                         then map snd $ edgesFrom edges b
                         else []
                     newqueue 
                      = bs ++ queueItems
                     newBlock
                      = blockInstrEdges $ setBlkPreDets blk newPreDets 
                     newBlks 
                      = pre ++ [newBlock] ++ post
                 in graphInEdges edges newBlks newqueue
 where getInstrDets blkId
        = case find (\(Block bId _ _ _ _) -> bId == blkId) blks of
               Just (Block _ _ pre post _) -> (pre, post)
               _       -> (InstrDets [] [], InstrDets [] [])


blockInstrEdges :: Block -> Block
blockInstrEdges (Block bid instrs preDets _ branches)
 = let (newInstrs, newPostDets)
        = instrEdges instrs preDets
   in Block bid newInstrs preDets newPostDets branches

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
                                              VBot -> setVarDets (vId, ([], VVar vId )) instrDets
                                              _    -> instrDets
                           in setRegDets (reg, ([addr], val)) newDets
        IStore avId reg  -> let val = getRegVal instrDets reg
                                newDets = case val of
                                               VVar bvId -> if avId == bvId 
                                                              then instrDets
                                                              else derefVar instrDets avId
                                               VTop      -> let deref
                                                                 = derefVar instrDets avId
                                                                oldRegDets 
                                                                 = getRegDet deref reg
                                                            in setRegDets 
                                                                (reg, (oldRegDets, VVar avId))
                                                                deref
                                               _         -> derefVar instrDets avId
                           in setVarDets (avId, ([addr], val)) newDets
        IArith op rout rin1 rin2 -> let val1 = getRegVal instrDets rin1
                                        val2 = getRegVal instrDets rin2
                                        outval = if valIsNum val1 && valIsNum val2
                                                  then VNum (I.arithCalc op (getValNum val1) (getValNum val2))
                                                  else VTop
                                       
                                    in setRegDets (rout, ([addr], outval)) instrDets
        ICall reg _ _ -> setRegDets (reg, ([addr], VTop)) instrDets
        _             -> instrDets


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
 = let isOrig n (o, _)
        = o == n
       edgeDests n
        = map snd (filter (isOrig n) edges)
   in rmDups reached ++ concatMap edgeDests reached

-- | Take a CFG, a list of block numbers to keep, 
-- | remove blocks not in the list, edges with endpoints not in the list.
retainBlocks :: CFG -> [Int] -> CFG
retainBlocks (CFG name args blocks edges) toRetain
 = let keptBlocks = filter (\(Block i _ _ _ _) -> i `elem` toRetain) blocks
       keptEdges  = filter (\(i, j)  -> i `elem` toRetain || j `elem` toRetain) edges
   in CFG name args keptBlocks keptEdges

removeUnreachedInstrs :: CFG -> CFG
removeUnreachedInstrs (CFG cId args blks edges)
 = let instrReached (InstrNode _ _ ins outs)
        = not (null ins && null outs)
       removeInBlk blk@(Block _ instrs _ _ _)
        = setBlkInstrs blk (filter instrReached instrs)
  in CFG cId args (map removeInBlk blks) edges


-- | Dead Code Elimination
removeDeadCode :: CFG -> CFG
removeDeadCode (CFG cID args blks edges)
 = let used      = findAllUsed blks
       transused = backClosure blks used []
       compl     = instrComplement blks transused
       trimmed   = removeAllInstr blks compl
   in genGraphEdges $ CFG cID args trimmed edges
       

instrComplement :: [Block] -> [InstrAddr] -> [InstrAddr]
instrComplement blks initSet
 = let blockCompInstrs (Block _ instrs _ _ _)
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
 = map extractAddress $ concatMap (\(Block _ instrs _ _ _) -> filter isUseInstr instrs) blks
 where isUseInstr (InstrNode i _ _ _)
        = case i of
               IBranch{}    -> True
               IReturn _    -> True
               IPrint _     -> True
               _            -> False
       extractAddress (InstrNode _ addr _ _)
        = addr
        

-- | Redundant Instruction Elimination

mutateRedundantInstrs :: CFG -> CFG
mutateRedundantInstrs (CFG name args blks edges)
 = let blockIds = map (\(Block i _ _ _ _) -> i) blks
       newBlks = mutateRedundantBlkInstrs blks [] blockIds
   in regenCFGEdges $ genGraphEdges $ CFG name args newBlks edges


mutateRedundantBlkInstrs :: [Block] -> [InstrAddr] -> [Int] -> [Block]
mutateRedundantBlkInstrs blks forbidRewrite bIds
 = case bIds of
        []   -> blks
        i:is -> let (_, Block _ instrs preDets _ _, _)
                     = breakElem (\(Block b _ _ _ _) -> b == i) blks
                    (newBlks, newForbids) = redundantInstrs blks preDets instrs forbidRewrite
                in mutateRedundantBlkInstrs newBlks newForbids is

redundantInstrs :: [Block] -> InstrDets -> [InstrNode] -> [InstrAddr] -> ([Block], [InstrAddr])
redundantInstrs blks dets instrs forbidRewrite
 = case instrs of
        []
         -> (blks, forbidRewrite)
        instrN@(InstrNode inst addr@(InstrAddr bId iId) ins outs):is
         -> let (pre, blk@(Block _ bInstrs _ _ _), post)
                 = breakElem (\(Block i _ _ _ _) -> i == bId) (removeAddrsFromOuts blks ins addr)
                (iPre, _, iPost)
                 = breakElem (\(InstrNode _ iAddr _ _) -> iAddr == addr) bInstrs
                dflt
                 = redundantInstrs blks (updateInstrDets instrN dets) is forbidRewrite
            in case inst of
                    (ILoad reg vId)
                     -> case getVarVal dets vId of
                             VNum n 
                              -> let newInstr 
                                      = InstrNode (IConst reg n) addr [] outs
                                     newBlks 
                                      = pre 
                                        ++ [setBlkInstrs blk (iPre ++ [newInstr] ++ iPost)]
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
                             _ -> dflt 
                    (IBranch reg b1 b2)
                     -> if b1 == b2
                        then case getRegVal dets reg of
                                  VNum _ 
                                   -> dflt
                                  _
                                   -> let newLC
                                           = InstrNode 
                                              (IConst (Reg 666) 0) 
                                              addr 
                                              [] [InstrAddr bId (iId + 1)]
                                          newBr
                                           = InstrNode
                                             (IBranch (Reg 666) b1 b1)
                                             (InstrAddr bId (iId + 1)) 
                                             [addr] []
                                          newBlks
                                           = pre ++ [setBlkInstrs blk (iPre ++ [newLC, newBr])] ++ post
                                      in redundantInstrs newBlks
                                                         (updateInstrDets newLC dets)
                                                         is
                                                         forbidRewrite
                        else case getRegVal dets reg of
                                  VNum 0 
                                   -> let newInstr 
                                           = InstrNode (IBranch reg b2 b2) addr ins outs
                                          newBlks
                                           = pre
                                              ++ [setBlkBranches (setBlkInstrs blk (iPre
                                                                                     ++ [newInstr] 
                                                                                     ++ iPost)) [b2]]
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
                                              ++ [setBlkBranches (setBlkInstrs blk (iPre
                                                                                     ++ [newInstr] 
                                                                                     ++ iPost)) [b1]]
                                              ++ post
                                      in redundantInstrs newBlks 
                                                         (updateInstrDets newInstr dets)
                                                         is
                                                         forbidRewrite
                                  _
                                   -> dflt
                    (IArith op rout rin1 rin2)
                     -> let v1 = getRegVal dets rin1
                            v2 = getRegVal dets rin2
                        in if valIsNum v1 && valIsNum v2 
                           then let res = I.arithCalc op (getValNum v1) (getValNum v2)
                                    newInstr
                                     = InstrNode (IConst rout res) addr [] outs
                                    newBlks
                                     = pre
                                        ++ [setBlkInstrs blk (iPre ++ [newInstr] ++ iPost)]
                                        ++ post
                                in redundantInstrs newBlks
                                                   (updateInstrDets newInstr dets)
                                                   is
                                                   forbidRewrite
                           else dflt
                    _
                     -> dflt


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
 = let (_, Block _ bInstrs preDets _ _, _) = breakElem (\(Block b _ _ _ _) -> b == bId) blks
       instrAddrs = map (\(InstrNode _ addr _ _) -> addr) bInstrs
   in rewriteBlockInstrs blks bId preDets instrsToRewrite forbidRewrite instrAddrs targetReg

rewriteBlockInstrs :: [Block] -> Int -> InstrDets -> [InstrAddr] -> [InstrAddr] -> [InstrAddr] -> Reg -> ([Block], Bool)
rewriteBlockInstrs blks bId dets instrsToRewrite forbidRewrite blkInstrAddrs targetReg
 = let (pre, blk@(Block _ bInstrs _ _ _), post) = breakElem (\(Block b _ _ _ _) -> b == bId) blks
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
                                          ++ [setBlkInstrs blk (iPre ++ [newInstr] ++ iPost)]
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


-- Block Merging
coalesceCFG :: CFG -> CFG
coalesceCFG (CFG i args blks edges)
 = let bridges = filter (isBridge edges) edges
       noChains = removeChains bridges
       newEdges = filter (`notElem` bridges) edges
   in regenCFGEdges $ genGraphEdges $ setCFGAddrs $ CFG i args (mergeBlocks blks noChains) newEdges

mergeBlocks :: [Block] -> [(Int, Int)]-> [Block]
mergeBlocks blks bridges
 = case bridges of     
        []   -> blks
        b:bs -> mergeBlocks (joinBridge blks b) bs

joinBridge :: [Block] -> (Int, Int) -> [Block]
joinBridge blks (p, c)
 = let (pre, Block pId pInstrs pPreDets _ _, post)
        = breakElem (\(Block i _ _ _ _) -> i == p) blks
       (Block cId cInstrs _ cPostDets cBranches)
        = fromJust $ find (\(Block i _ _ _ _) -> i == c) blks
       newInstrsPre
        = takeWhile (\(InstrNode i _ _ _) -> isNotBranch i) pInstrs
       newBlock 
        = Block pId (newInstrsPre ++ cInstrs) pPreDets cPostDets cBranches
       newBlks
        = pre ++ [newBlock] ++ post
  in filter (\(Block i _ _ _ _) -> i /= cId) newBlks
 where isNotBranch i
        = case i of
               IBranch{} -> False
               _         -> True

isBridge :: [(Int, Int)] -> (Int, Int) -> Bool
isBridge edges (h, t)
 = snd (findDegree h edges) == 1 && fst (findDegree t edges) == 1

findDegree :: Int -> [(Int, Int)] -> (Int, Int)
findDegree bId edges
 = let ins   = length (filter (\(_, q) -> (q == bId)) edges)
       outs  = length (filter (\(p, _) -> (p == bId)) edges)
   in (ins, outs)

removeChains :: [(Int, Int)] -> [(Int, Int)]
removeChains edges
 = case edges of
        []   -> []
        (p, c):es -> if not (any (\(t, _) -> t == c) es
                             || any (\(_, s) -> s == p) es)
                      then (p, c) : removeChains es
                      else removeChains es
