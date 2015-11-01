
module Imp.Opt.Optimiser where
import Imp.Opt.Exp
import Data.List


-- | Graph building.

genGraphEdges :: CFG -> CFG
genGraphEdges (CFG name blks edges)
 = let (newBlks, _, _) = graphInEdges edges blks [] [0]
   in (CFG name newBlks edges)

graphInEdges :: [CFGEdge] -> [Block] -> [( Int, [(Reg, [InstrAddr])] )] -> [Int] -> ([Block], [( Int, [(Reg, [InstrAddr])] )], [Int])
graphInEdges edges blks blkRegDets queue
 = case queue of
        []    -> (blks, blkRegDets, queue)
        b:bs  -> let (pre, thisBlock, post) = spanElem (\(Block bid _) -> bid == b) blks
                     (regDets, _) = getRegDets b
                     (newBlock, newRegDets) = blockInstrEdges thisBlock regDets
                     queueItems = filter (isNewOrDiff newRegDets) $ map (\(CFGEdge _ d) -> d) $ edgesFrom edges b
                     newqueue = bs ++ queueItems
                     newBlks = pre ++ [newBlock] ++ post
                     newBlkRegDets = (b, newRegDets) : filter (\(i, _) -> i /= b) blkRegDets
                 in graphInEdges edges newBlks newBlkRegDets newqueue
 where getRegDets blkId
        = case lookup blkId blkRegDets of
               Just rd -> (rd, False)
               _       -> ([], True)
       isNewOrDiff rd blkId
        = let (res, new) = getRegDets blkId
          in new || not (regDetsEqual rd res)



regDetsEqual :: [(Reg, [InstrAddr])] -> [(Reg, [InstrAddr])] -> Bool
regDetsEqual p q
 = let sa = sort p
       sb = sort q
   in case sa of
           [] -> case sb of
                      [] -> True
                      _  -> False
           (ar, aadrs):as -> case sb of
                                  (br, badrs):bs -> ar == br 
                                                     && sort aadrs == sort badrs 
                                                     && regDetsEqual as bs
                                  []             -> False
       
 

spanElem :: (a -> Bool) -> [a] -> ([a], a, [a])
spanElem preds sequ
 = let (pre, post) = span preds sequ
   in (pre, head post, tail post)
 


blockInstrEdges :: Block -> [(Reg, [InstrAddr])] -> (Block, [(Reg, [InstrAddr])])
blockInstrEdges (Block bid instrs) regDets
 = let (newInstrs, newRegDets)
        = instrEdges instrs regDets
   in ((Block bid newInstrs), newRegDets)

instrEdges :: [InstrNode] -> [(Reg, [InstrAddr])] -> ([InstrNode], [(Reg, [InstrAddr])])
instrEdges instrs regDets
 = case instrs of
        []      -> (instrs, regDets)
        i:is    -> let (newI, newRegDets)
                        = instrEdge i regDets
                       (fIs, fRegDets)
                        = instrEdges is newRegDets
                   in (newI:fIs, fRegDets)

instrEdge :: InstrNode -> [(Reg, [InstrAddr])] -> (InstrNode, [(Reg, [InstrAddr])])
instrEdge instrN@(InstrNode inst addr _ _) regDets
 = case inst of
        IConst reg _  -> ( instrN
                         , setRegDets (reg, [addr]) regDets )
        
        -- TODO: Get edges in between variable-setting/reading instructions
        ILoad  reg _  -> ( instrN
                         , setRegDets (reg, [addr]) regDets )
        IStore _ reg  -> ( setInstrIns instrN (getRegDet reg) 
                         , regDets )

        IArith _ rout rin1 rin2 -> ( setInstrIns instrN (rmDups (concatMap getRegDet [rin1, rin2]))
                                   , setRegDets (rout, [addr]) regDets )
        IBranch reg _ _ -> ( setInstrIns instrN (getRegDet reg)
                           , regDets )

        IReturn reg     -> ( setInstrIns instrN (getRegDet reg)
                           , regDets )

        ICall reg _ rlist -> let detList 
                                  = rmDups $ concatMap getRegDet rlist
                             in ( setInstrIns instrN detList
                                , setRegDets (reg, [addr]) regDets )
        IPrint rlist      -> let detList
                                  = rmDups $ concatMap getRegDet rlist
                             in ( setInstrIns instrN detList
                                , regDets )
 where getRegDet reg
        = case lookup reg regDets of
               Nothing -> []
               Just r  -> r

setInstrIns :: InstrNode -> [InstrAddr] -> InstrNode
setInstrIns (InstrNode inst addr _ outs) newIns
 = InstrNode inst addr newIns outs

setRegDets :: (Reg, [InstrAddr]) -> [(Reg, [InstrAddr])] -> [(Reg, [InstrAddr])]
setRegDets regDet@(reg, _) regDets
 = regDet : filter (\(r, _) -> r /= reg) regDets

addRegDets :: (Reg, [InstrAddr]) -> [(Reg, [InstrAddr])] -> [(Reg, [InstrAddr])]
addRegDets regDet@(reg, adrs) regDets
 = case lookup reg regDets of
        Just prevAdrs -> let newAdrs
                              = rmDups prevAdrs ++ adrs
                         in (reg, newAdrs) : filter (\(r, _) -> r /= reg) regDets
        _             -> regDet:regDets


-- | Unreachable block removal.

zeroClosure :: CFG -> (Id, [Int])
zeroClosure cfg@(CFG i _ _) = (i, closure cfg [0])

closure :: CFG -> [Int] -> [Int]
closure cfg nodes
  | nodes == stepped   = nodes 
  | otherwise          = closure cfg stepped
 where stepped = reachStep cfg nodes

reachStep :: CFG -> [Int] -> [Int]
reachStep (CFG _ _ edges) reached
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
retainBlocks (CFG name blocks edges) toRetain
 = let keptBlocks = filter (\(Block i _) -> i `elem` toRetain) blocks
       keptEdges  = filter (\(CFGEdge i j)  -> i `elem` toRetain || j `elem` toRetain) edges
   in CFG name keptBlocks keptEdges


rmDups :: Ord a => [a] -> [a]
rmDups l = map head $ group $ sort l

-- | Dead Code Elimination


-- | Redundant Load Elimination


