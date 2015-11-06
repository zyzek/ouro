
module Imp.Opt.Optimiser where
import Imp.Opt.CFG
import Imp.Opt.CFGUtils
import Imp.Core.Exp hiding (Block)
import Data.List
import Data.List.Utils
import Data.Maybe
import qualified Imp.Core.Interpreter   as I

-- | Flow Graph Construction  ========================================

-- | Immediately after parsing, no edges or state information exists, so needs to be calculated.
-- | Optimisations may add and remove instructions, blocks, so that edges and state are changed.
-- | Updated state information, in particular, must be propagated if instructions are modified.
-- |
-- | This process happens in several stages.
-- |
-- |
-- | First, determine the edges that go between blocks, for the next stage to operate on.
-- | This only requires finding the first branch instruction in each block, if it exists.
-- |
-- |
-- | Second, determine which registers and variables are defined in which blocks.
-- | This involves first finding for each block, the blocks which originate it.
-- | Everything defined in the originating blocks is defined in this block, and all of these
-- | variables and registers are added to the state at the start of the block, with
-- | an uninitialised status.
-- |
-- | To actually build the map from blocks to sets of originators, it's important to realise that
-- | block A originates block B iff block A also originates all the parents of block B, block 0,
-- | being the entry point, originates every other block, and each block originates itself.
-- | Hence, first set zero to be originated only by itself. Then set each other block
-- | to be originated by every block.
-- | Then, for each block, look at the sets of originators of the parents of this block.
-- | The intersection of these sets, plus the block itself, is the block's new set of originators.
-- | Keep performing this until no further changes can be made, and we have obtained the
-- | final mapping of originators.
-- | 
-- | An invariant is maintained that the true set of originators for a block is a subset of the 
-- | interim set at all stages of the algorithm.
-- | The set of correct sets of originators propagates out from block 0 as the algorithm iterates, 
-- | and the size of each set is monotonically decreasing, so that it must eventually terminate
-- | with the final sets being only the correct ones.
-- |
-- |
-- | The third step is to produce the actual flow graph by starting a traversal from Block 0.
-- | The traversal maintains a queue of blocks to update, which it pops off one at a time until 
-- | there are none left.
-- | For each block: 
-- |   First determine the program state at the start of the block,
-- |   which requires merging the terminal states of all blocks which branch to this one.
-- |   If this new state differs from the state the block previously had, then we might need
-- |   to update the blocks this one branches to, so add their ids to the queue.
-- |   
-- |   Then, for each instruction, determine its incoming edges by examining the current state.
-- |   Each of its input registers had its value determined last by some instruction(s), each of
-- |   which contributes one incoming edge. 
-- |   Deduce the value the instruction computes, update the state, and continue to the next
-- |   instruction.
-- |   
-- |   When there are no instructions left, set the terminal state of this block with the current
-- |   state, and proceed to the next item in the queue.
-- | The queue must eventually empty once the states stabilise.
-- |
-- |
-- | Finally, collect all in-neighbourhoods in the graph and add the corresponding addresses to 
-- | the out-neighbourhood of each instruction.
-- |
-- | Now we have a flow graph, the initial and final states of each block, and the edges between
-- | blocks.

-- | Recalculate the flow graph.
genGraph :: CFG -> CFG
genGraph (CFG name args blks _)
 = let fresh     = wipeGraph blks
       newEdges  = regenBlkEdges fresh
       zChildren = filter (/= 0) $ map snd $ edgesFrom newEdges 0
       scoped    = setOrigScopes (setBlockPostScopes (setArgScope fresh args)) newEdges
       newBlks   = graphInEdges newEdges (setArgDets scoped args) (0:zChildren)
   in CFG name args (graphOutEdges newBlks) newEdges



-- | Step 1 - Wipe the graph clean and calculate interblock edges.

-- | Destroy all edges and state in a graph. Returns the block list with block edges recomputed.
wipeGraph :: [Block] -> [Block]
wipeGraph = map wipeBlock

wipeBlock :: Block -> Block
wipeBlock (Block i instrs _ _ _)
 = let newInstrs = map wipeInstrEdges instrs
   in  blockBranches $ Block i newInstrs (InstrDets [] []) (InstrDets [] []) []

wipeInstrEdges :: InstrNode -> InstrNode
wipeInstrEdges (InstrNode inst addr _ _)
 = InstrNode inst addr [] []



-- | Step 2 - Determine originators and define objects in the scopes where they should exist.

-- | Make sure the function arguments are defined throughout the graph.
setArgScope :: [Block] -> [Id] -> [Block]
setArgScope blks args 
 = case blks of 
        []   -> []
        b:bs -> argedBlk b : setArgScope bs args
 where argDets
        = map (\i -> (i, ([], VBot))) args
       argedBlk (Block bId instrs preDets postDets brs)
        = Block bId instrs (setAllVarDets argDets preDets) postDets brs


-- Set the program state at the end of each block to know about objects defined within that block.
setBlockPostScopes :: [Block] -> [Block]
setBlockPostScopes blks
 = case blks of 
        []   -> []
        b:bs -> setBlockPostScope b : setBlockPostScopes bs
 where setBlockPostScope (Block bid instrs preDets _ branches)
        = let newPostDets = instrScopes instrs preDets
          in Block bid instrs preDets newPostDets branches


-- | Set the initial state of each block to be the union of the final states of all the
-- | blocks that originate it.
setOrigScopes :: [Block] -> [(Int, Int)] -> [Block]
setOrigScopes blks edges
 = let propped = propOrigScopes blks $ filterOrigs (findOriginators edges) (blockIds blks)
   in unionPostDets propped
 where unionDets (Block _ _ preDets postDets _)
        = unionScopes preDets postDets
       unionPostDets inBlks
        = case inBlks of
               []   -> []
               b:bs -> setBlkPostDets b (unionDets b) : unionPostDets bs

propOrigScopes :: [Block] -> [(Int, [Int])] -> [Block]
propOrigScopes blks origList
 = case origList of
        []   -> blks
        o:os -> propOrigScopes (getOrigs o) os
   where getOrigs (n, origs)
          = let (pre, blk@(Block _ _ preDets _ _), post)
                 = breakElem (\(Block b _ _ _ _) -> b == n) blks
                newPreDets
                 = unionScopeList $ preDets : map (snd . getInstrDets blks) origs

            in pre ++ [setBlkPreDets blk newPreDets] ++ post

unionScopeList :: [InstrDets] -> InstrDets
unionScopeList scopeList
 = case scopeList of
        []   -> InstrDets [] []
        s:ss -> unionScopes s (unionScopeList ss)

unionScopes :: InstrDets -> InstrDets -> InstrDets
unionScopes (InstrDets rd vd) (InstrDets rd' vd')
 = InstrDets (unionScopeDets rd rd') (unionScopeDets vd vd')
 where unionDetScopeList l = case l of
                             (r, _):_ -> [(r, ([], VBot))]
                             _          -> []
       unionScopeDets a b
        = let merged = groupBy (\(a1, _) (a2, _) -> a1 == a2) $ sortByKeys  (a ++ b)
          in concatMap unionDetScopeList merged


-- Determine which objects were defined in a sequence of instructions.
instrScopes :: [InstrNode] -> InstrDets -> InstrDets
instrScopes instrs instrDets
 = case instrs of
        []      -> instrDets
        i:is    -> let newInstrDets = instrScope i instrDets
                   in if isBranchOrRet i then newInstrDets else instrScopes is newInstrDets
 where instrScope (InstrNode i _ _ _) dets
        = case i of
               IConst reg _     -> setRegDets (newDet reg) dets
               ILoad  reg _     -> setRegDets (newDet reg) dets
               IArith _ reg _ _ -> setRegDets (newDet reg) dets
               ICall reg _ _    -> setRegDets (newDet reg) dets
               IStore vId _     -> setVarDets (newDet vId) dets
               _                ->  dets
       newDet a = (a, ([], VBot))


-- | Given an edge list, determine for each block which blocks it originates from.
-- | That is to say, the set of blocks where registers and variables defined in them are also 
-- | defined in the entirety of the block we're looking at.
findOriginators :: [(Int, Int)] -> [(Int, [Int])]
findOriginators edges 
 = let nodes = rmDups $ concatMap (\(o, d) -> [o, d]) edges
       origList =  (0, [0]) : filter ((/= 0) . fst) (map (\n -> (n, nodes)) nodes)
   in map (\(n, os) -> (n, filter (/=n) os)) $ originators (filter (/= 0) nodes) edges origList

originators :: [Int] -> [(Int, Int)] -> [(Int, [Int])] -> [(Int, [Int])]
originators nodes edges origList
 | stepped == origList
   = origList
 | otherwise
   = originators nodes edges stepped
 where stepped 
        = map (recalcOrigs edges origList . fst) origList 

recalcOrigs :: [(Int, Int)] -> [(Int, [Int])] -> Int -> (Int, [Int])
recalcOrigs edges origList node
 = let ins = map fst (edgesTo edges node)
       inOrigs =  map snd $ filter ((`elem` ins) . fst) origList 
       newOrigs = rmDups $ node : polyIntersect inOrigs
   in (node, newOrigs)
 where polyIntersect ls = case ls of
                               []     -> []
                               [l]      -> l
                               l:rest -> l `intersect` polyIntersect rest



-- | Step 3 - Flow Graph Construction

-- | Define program arguments in the initial program state.
setArgDets :: [Block] -> [Id] -> [Block]
setArgDets blks args 
 = let argDets 
        = map (\i -> (i, ([], VVar i))) args
       (pre, Block _ instrs preDets postDets brs, post)
        = breakElem (\(Block bId _ _ _ _) -> bId == 0) blks
       newBlk
        = Block 0 instrs (setAllVarDets argDets preDets) postDets brs
   in pre ++ [newBlk] ++ post


-- | Flow graph in-edges and block initial/terminal states.
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
                             [n] -> snd (getInstrDets blks n)
                             l   -> let d:ds = map (snd . getInstrDets blks) l
                                    in foldr mergeInstrDets d ds
                     queueItems 
                      = if preDets /= newPreDets
                         then map snd $ edgesFrom edges b
                         else []
                     newqueue 
                      = bs ++ filter (`notElem` bs) queueItems
                     newBlock
                      = blockInstrEdges $ setBlkPreDets blk newPreDets 
                     newBlks 
                      = pre ++ [newBlock] ++ post
                 in graphInEdges edges newBlks newqueue


-- | Update the edges and states of a given block.
blockInstrEdges :: Block -> Block
blockInstrEdges (Block bid instrs preDets _ branches)
 = let (newInstrs, newPostDets)
        = instrEdges instrs preDets
   in Block bid newInstrs preDets newPostDets branches


-- | Update the edges of a sequence of instructions in order and return the final state.
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


-- | The value of a register is determined by the last instruction that writes to it.
-- | The value itself depends on the instruction, and its inputs:
-- | 
-- | lc: loading a constant into a register sets that register's value to that constant.
-- |
-- | ld: If the value of the variable being loaded is indeterminate, then the register is marked 
-- |     as containing the same value as the input variable.
-- |     Otherwise, the register inherits the value that was in the variable.
-- |     If the variable is not an arg and was not previously defined by a st, it is indeterminate. 
-- |
-- | st: First we convert all references to the value of the target variable to VTop.
-- |     Since the variable has changed values, any receptacles marked as holding its value
-- |     are now indeterminate. This does not happen if input register contains the variable's 
-- |     own value. 
-- |     If the register's value is indeterminate, then not only is the variable's value set,
-- |     but the register is now marked as containing the same value that is in the variable.
-- |     It would by possible, if the register contained the value of some other
-- |     variable, to transitively propagate the information that the two variables hold the same 
-- |     value. Although it would be straightforward, this has not been implemented.
-- |
-- | op: Arithmetic operations load VTop into the output register unless both input registers
-- |     contain constants, in which case the appropriate operation is performed, and the correct
-- |     constant is output.
-- |     I briefly played with building trees representing the structure of what computations
-- |     had been performed to obtain a value, so as to be able to save instructions, and determine
-- |     the identity of values which otherwise would be swallowed up in a VTop.
-- |
-- | call: Function calls are just treated as unpredictable. They simply make the value of the 
-- |     output register indeterminate.
-- |
-- | Other instructions do not change any values, so do on update the program state.

-- | Update the program state given a particular instruction.
updateInstrDets :: InstrNode -> InstrDets -> InstrDets
updateInstrDets (InstrNode inst addr _ _) instrDets
 = case inst of
        IConst reg v  -> setRegDets (reg, ([addr], VNum v)) instrDets
        ILoad  reg vId  -> let varVal = getVarVal instrDets vId  
                               val = case varVal of
                                          VBot -> VTop
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
                                                  then VNum (I.arithCalc op 
                                                                         (getValNum val1)
                                                                         (getValNum val2))
                                                  else VTop
                                    in setRegDets (rout, ([addr], outval)) instrDets
        ICall reg _ _ -> setRegDets (reg, ([addr], VTop)) instrDets
        _             -> instrDets


-- | Determine which instructions determine the inputs of an instruction.
-- | This is simply a concatenation of the determining addresses of all input registers/variables.
instrEdge :: InstrNode -> InstrDets -> (InstrNode, InstrDets)
instrEdge instrN@(InstrNode inst _ _ _) instrDets
 = let newInstrDets = updateInstrDets instrN instrDets
       newInstrN
        = case inst of
               IConst{}             -> instrN
               ILoad  _ vId         -> setNodeIns instrN (getVarD vId)
               IStore _ reg         -> setNodeIns instrN (getRegD reg)
               IArith _ _ rin1 rin2 -> setNodeIns instrN (rmDups (concatMap getRegD [rin1, rin2]))
               IBranch reg _ _      -> setNodeIns instrN (getRegD reg)
               IReturn reg          -> setNodeIns instrN (getRegD reg)
               ICall _ _ rlist      -> let detList 
                                            = rmDups $ concatMap getRegD rlist
                                       in setNodeIns instrN detList
               IPrint rlist         -> let detList
                                            = rmDups $ concatMap getRegD rlist
                                       in setNodeIns instrN detList
   in (newInstrN, newInstrDets)
 where getRegD
        = getRegDet instrDets
       getVarD
        = getVarDet instrDets



-- | Step 4 -- Set outgoing flow graph edges.

-- | Set all out-neighbourhoods of nodes in the graph, given that all in-neighbourhoods
-- | have already been populated.
graphOutEdges :: [Block] -> [Block]
graphOutEdges blks
 = let setInstrOuts iNode@(InstrNode i adr ins _)
        = case lookup adr (graphOuts blks) of
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



-- | Optimisation Passes  ========================================

-- | Unreachable Block/Instruction Removal  ========================================

-- | In order to remove unreachable blocks, it is straightforward to find the transitive closure
-- | of the graph from Block 0. Any that are not reached by this are discarded.
blockClosure :: CFG -> CFG
blockClosure cfg
 = let (_, reachable) = zeroClosure cfg
   in retainBlocks cfg reachable 

closureIds :: [CFG] -> [(Id, [Int])]
closureIds
 = map zeroClosure

-- | Transitive closure of graph blocks starting from the 0 block.
zeroClosure :: CFG -> (Id, [Int])
zeroClosure cfg@(CFG i _ _ _) = (i, closure cfg [0])

-- | Find the transitive closure of a set of blocks in the graph.
closure :: CFG -> [Int] -> [Int]
closure cfg nodes
  | nodes == stepped   = nodes 
  | otherwise          = closure cfg stepped
 where stepped = closureStep cfg nodes

closureStep :: CFG -> [Int] -> [Int]
closureStep (CFG _ _ _ edges) reached
 = let isOrig n (o, _)
        = o == n
       edgeDests n
        = map snd (filter (isOrig n) edges)
   in rmDups $ reached ++ concatMap edgeDests reached

-- | Take a CFG, a list of block numbers to keep, 
-- | remove blocks not in the list, and edges with endpoints not in the list.
retainBlocks :: CFG -> [Int] -> CFG
retainBlocks (CFG name args blocks edges) toRetain
 = let keptBlocks = filter (\(Block i _ _ _ _) -> i `elem` toRetain) blocks
       keptEdges  = filter (\(i, j)  -> i `elem` toRetain || j `elem` toRetain) edges
   in CFG name args keptBlocks keptEdges



-- | Dead Code Elimination ========================================

-- | What does it mean for code to be dead?
-- | A reasonable person might say that an instruction is dead if it can ultimately have no 
-- | impact on the result of running the program of which it is a part.
-- | That is, it plays no part in contributing to any of flow control, IO effects, 
-- | or the return value of the function.
-- | To this end, return, print, and branch instructions are themselves considered to be used.
-- | 
-- | Starting with this kernel of used instructions, we perform a transitive closure in reverse
-- | of this set over the flow graph. Following each edge backwards, we expand the used set until
-- | no more instructions can be added. In this way, the set contains the instructions along every 
-- | path that terminates in one of the primitive "used" instructions listed above. We can then
-- | deduce that all such instructions are also used, since they contribute to the behaviour
-- | of something that prints, returns, or branches.
-- | 
-- | The complement of this set of instructions, then, is dead code, and the corresponding nodes
-- | can be discarded from the flow graph.
-- |
-- | This will not eliminate orphaned code that, while not actually reachable, still terminates
-- | with a used instruction. However, the removeUnreachedInstrs optimisation below will
-- | eliminate them, as it relies upon the forward traversal performed in constructing the
-- | flow graph initially; it is the inverse counterpart to that performed here.

removeDeadCode :: CFG -> CFG
removeDeadCode (CFG cID args blks edges)
 = let used      = findBasicUsed blks
       transused = backClosure blks used []
       compl     = instrComplement blks transused
       trimmed   = removeAllInstr blks compl
   in genGraph $ removeUnreachedInstrs $ CFG cID args trimmed edges


instrComplement :: [Block] -> [InstrAddr] -> [InstrAddr]
instrComplement blks initSet
 = let blockCompInstrs (Block _ instrs _ _ _)
        = filter (`notElem` initSet) $ map (\(InstrNode _ addr _ _) -> addr) instrs
   in concatMap blockCompInstrs blks


-- Find the addresses of instructions in the backwards closure of a flow graph,
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
       
findBasicUsed :: [Block] -> [InstrAddr]
findBasicUsed blks
 = map extractAddress $ concatMap (\(Block _ instrs _ _ _) -> filter isUseInstr instrs) blks
 where extractAddress (InstrNode _ addr _ _) = addr


-- | In constructing the flow graph, only valid block edges are followed, and nothing after
-- | a branch or a return is processed. Hence, any instruction that can never be executed should
-- | have no edges incident upon them. This function removes such instructions. This will also
-- | remove some dead code, such as isolated loads.
removeUnreachedInstrs :: CFG -> CFG
removeUnreachedInstrs (CFG cId args blks edges)
 = let instrReached instrN@(InstrNode _ _ ins outs)
        = not (null ins && null outs) || isUseInstr instrN
       removeInBlk blk@(Block _ instrs _ _ _)
        = let (pre, post) = break isBranchOrRet instrs
              truncated = case post of
                               []   -> pre
                               br:_ -> pre ++ [br]
          in setBlkInstrs blk (filter instrReached truncated)
  in CFG cId args (map removeInBlk blks) edges

isUseInstr :: InstrNode -> Bool
isUseInstr (InstrNode i _ _ _)
 = case i of
        IBranch{}    -> True
        IReturn _    -> True
        IPrint _     -> True
        _            -> False



-- | Instruction Mutation and Elimination  ========================================

-- | Given the program state and a given instruction, it may be possible to rewrite that
-- | instruction while preserving the meaning of the program, yet increasing its speed,
-- | or providing opportunities for further optimisation.
-- |
-- | Load optimisations:
-- |
-- |  Constant Propagation: 
-- |   If the input variable contains a constant, then the load can be replaced with a load const.
-- |
-- |  Redundant Load Elimination:  
-- |   If the input variable is indeterminate, we can follow all outgoing edges from this node.
-- |   For each such instruction, we can check to see if its input registers can be switched, so
-- |   that the instruction is no longer determined by the load we were initially examining.
-- |   If this is possible for every instruction in the out-neighbourhood of our original node,
-- |   then rewrite them all, and forbid any other rewrite from changing an instruction to be
-- |   determined by the one we just rewrote.
-- |   The end product is a node with 0 out-degree, and which is not a "use", so that it can later
-- |   be removed by dead code elimination.
-- |
-- |   This also works for Load Constant instructions. If a number is already available in some
-- |   register there is no need for a new one.
-- |
-- | Branch optimisations:
-- |
-- |  Const Register -> Unconditional Jump:
-- |   If the value in the input register of a branch is a constant, then one or the other of the
-- |   branches can be eliminated entirely, which can later be cleaned up by the unreachable code
-- |   removal optimisation.
-- |
-- |  Unconditional Jump -> Const Register:
-- |   Conversely, if the destinations of a branch are the same, then which register it reads from
-- |   is irrelevant. It is switched to rely on some register containing a const on the assumption
-- |   that this will mean fewer dependencies on loads and stores. Wherever the const value came
-- |   from can eventually be replaced by an lc.
-- |   We preferentially choose registers which obtained their value in some earlier block, 
-- |   to make it likelier that this block will be emptied out.A
-- |   If no constant register is available, one is created by the insertion of an lc immediately
-- |   before the branch.
-- |   I had intended this register to be the lowest unused register number, but for now it 
-- |   simply uses the reserved Register 666, to avoid complicating the function still further,
-- |   and because it is fine to reuse this register for this purpose, as it will never have
-- |   either ancestors or descendents except the branch that it determines.
-- |
-- | Arithmetic Optimisations:
-- |
-- |  Constant Propagation:
-- |   If the result of an arithmetic expression is constant, replace the operation by an lc.
-- |  
-- |  I did play a little with arithmetic trees so that sequences of operations could be folded
-- |  up: e.g. things could be factored out, strings of additions merged, and so forth.
-- |  Not included here.

-- | Apply all of the above-listed optimisations
mutateGraph :: CFG -> CFG
mutateGraph (CFG name args blks edges)
 = let newBlks = mutateBlks blks [] $ blockIds blks
   in regenCFGEdges $ genGraph $ CFG name args newBlks edges


mutateBlks :: [Block] -> [InstrAddr] -> [Int] -> [Block]
mutateBlks blks forbidRewrite bIds
 = case bIds of
        []   -> blks
        i:is -> let (_, Block _ instrs preDets _ _, _)
                     = breakElem (\(Block b _ _ _ _) -> b == i) blks
                    (newBlks, newForbids) = mutateInstrs blks preDets instrs forbidRewrite
                in mutateBlks newBlks newForbids is

-- | Given the instruction sequence of a block, apply optimisations to each of the instructions,
-- | updating the graph and state as we go.
mutateInstrs :: [Block] -> InstrDets -> [InstrNode] -> [InstrAddr] -> ([Block], [InstrAddr])
mutateInstrs blks dets instrs forbidRewrite
 = case instrs of
        []
         -> (blks, forbidRewrite)
        instrN@(InstrNode inst addr@(InstrAddr bId iId) ins outs):is
         -> let (pre, blk@(Block _ bInstrs _ _ _), post)
                 = breakElem (\(Block i _ _ _ _) -> i == bId) (removeAddrsFromOuts blks ins addr)
                (iPre, _, iPost)
                 = breakElem (\(InstrNode _ iAddr _ _) -> iAddr == addr) bInstrs
                dflt
                 = mutateInstrs blks (updateInstrDets instrN dets) is forbidRewrite
            in case inst of
                    (ILoad reg vId)
                     -> case getVarVal dets vId of
                             VNum n                  -- Constant Propagation
                              -> let newInstr 
                                      = InstrNode (IConst reg n) addr [] outs
                                     newBlks 
                                      = pre 
                                        ++ [setBlkInstrs blk (iPre ++ [newInstr] ++ iPost)]
                                        ++ post
                                 in mutateInstrs newBlks 
                                                 (updateInstrDets newInstr dets) 
                                                 is
                                                 forbidRewrite
                             _                       -- Redundant Load Elimination
                              -> let (newBlks, success)
                                      = tryRewriteRegs blks outs (addr:forbidRewrite) reg
                                     newForbids 
                                      = if success
                                         then addr:forbidRewrite
                                         else forbidRewrite
                                 in mutateInstrs newBlks
                                                 (updateInstrDets instrN dets)
                                                 is
                                                 newForbids
                    (IBranch reg b1 b2)
                     -> if b1 == b2                  -- Unconditional Jump -> Const Register
                        then case getRegVal dets reg of
                                  VNum _ 
                                   -> dflt
                                  _
                                   -> let newLC
                                           = InstrNode 
                                              (IConst (Reg 666) 0) 
                                              addr 
                                              [] [InstrAddr bId (iId + 1)]
                                          (newBrReg, newBrIns)
                                           = let (otherBlkRegs, thisBlkRegs)
                                                  = partition 
                                                     (\(_, (addrs, _)) 
                                                      -> all (\(InstrAddr ibId _) 
                                                              -> ibId /= bId) addrs)
                                                     (constRegs dets)
                                             in if not (null otherBlkRegs) 
                                                 then let (r, (as, _)) = head otherBlkRegs
                                                      in (r, as)
                                                 else if not (null thisBlkRegs)
                                                       then let (r, (as, _)) = head thisBlkRegs
                                                            in (r, as)
                                                       else (Reg 666, [addr])
                                          newBrAddr 
                                           = if newBrIns == [addr]
                                              then InstrAddr bId (iId + 1)
                                              else addr
                                          newBr
                                           = InstrNode
                                             (IBranch newBrReg b1 b1)
                                             newBrAddr
                                             newBrIns []
                                          newBrInstrs
                                           = if newBrIns == [addr]
                                              then [newLC, newBr]
                                              else [newBr]
                                          newBlks
                                           = pre ++ [setBlkInstrs blk (iPre ++ newBrInstrs)] ++ post
                                          fBlks 
                                           = addOutEdges newBlks newBrIns newBrAddr
                                      in mutateInstrs fBlks
                                                      (updateInstrDets newLC dets)
                                                      is
                                                      forbidRewrite
                        else case getRegVal dets reg of     -- Const Register -> Unconditional Jump
                                  VNum 0 
                                   -> let newInstr 
                                           = InstrNode (IBranch reg b2 b2) addr ins outs
                                          newBlks
                                           = pre
                                              ++ [setBlkBranches (setBlkInstrs blk 
                                                                               (iPre
                                                                                 ++ [newInstr] 
                                                                                 ++ iPost)) [b2]]
                                              ++ post
                                      in mutateInstrs newBlks 
                                                      (updateInstrDets newInstr dets)
                                                      is
                                                      forbidRewrite
                                  VNum _ 
                                   -> let newInstr 
                                           = InstrNode (IBranch reg b1 b1) addr ins outs
                                          newBlks
                                           = pre
                                              ++ [setBlkBranches (setBlkInstrs blk 
                                                                               (iPre
                                                                                 ++ [newInstr] 
                                                                                 ++ iPost)) [b1]]
                                              ++ post
                                      in mutateInstrs newBlks 
                                                      (updateInstrDets newInstr dets)
                                                      is
                                                      forbidRewrite
                                  _
                                   -> dflt
                    (IArith op rout rin1 rin2)          -- Constant Propagation
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
                                in mutateInstrs newBlks
                                                (updateInstrDets newInstr dets)
                                                is
                                                forbidRewrite
                           else dflt
                    (IConst reg _)                      -- Redundant Constant Load Elimination
                     -> let (newBlks, success)
                             = tryRewriteRegs blks outs (addr:forbidRewrite) reg
                            newForbids 
                             = if success
                                then addr:forbidRewrite
                                else forbidRewrite
                        in mutateInstrs newBlks
                                        (updateInstrDets instrN dets)
                                        is
                                        newForbids
                    _
                     -> dflt


-- | Attempt to rewrite a set of instructions not to use a certain register, and 
-- | not to use registers relying on the addresses in the forbidRewrite list.
-- | Returns the resulting rewritten block list and the success status. If the rewrite failed,
-- | then the output block list is the input block list.
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

-- | Since we must know the current state in order to rewrite a register,
-- | The instructions to rewrite are grouped by block. For a given block, the state is updated 
-- | as the instruction sequence is iterated through, rewriting each target instruction as it is
-- | arrived at.
rewriteBlock :: [Block] -> Int -> [InstrAddr] -> [InstrAddr] -> Reg -> ([Block], Bool)
rewriteBlock blks bId instrsToRewrite forbidRewrite targetReg
 = let (_, Block _ bInstrs preDets _ _, _) = breakElem (\(Block b _ _ _ _) -> b == bId) blks
       instrAddrs = map (\(InstrNode _ addr _ _) -> addr) bInstrs
   in rewriteBlockInstrs blks bId preDets instrsToRewrite forbidRewrite instrAddrs targetReg

rewriteBlockInstrs :: [Block] -> Int -> InstrDets -> [InstrAddr] 
                   -> [InstrAddr] -> [InstrAddr] -> Reg -> ([Block], Bool)
rewriteBlockInstrs blks bId dets instrsToRewrite forbidRewrite instrAddrs targetReg
 = let (pre, blk@(Block _ bInstrs _ _ _), post) = breakElem (\(Block b _ _ _ _) -> b == bId) blks
   in case instrsToRewrite of
           []   
            -> (blks, True)
           i:is 
            -> case instrAddrs of
                    []     
                     -> (blks, False)
                    ba:bas 
                     -> let (iPre, instrN@(InstrNode _ iAddr ins _), iPost)
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
          
-- | We can only rewrite a register if we can find another register that shares its value.
-- | That is, if the register contains either a constant or the value of a variable,
-- | then we may be able to find one. This is not possible if its value is indeterminate.
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

-- | Replace the the occurences of an old register with a new register in some instruction.
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



-- | Block Merging Optimisations  ========================================


-- Contract the graph by removing removable blocks.
-- Since we modify the large scale structure, the graph may need to be relabeled after doing so.
-- In what follows, block 0 is never removed
coalesceCFG :: CFG -> CFG
coalesceCFG (CFG i args blks edges)
 = let (newBlks, newEdges) = mergeBlocks (blks, edges)
   in regenCFGEdges $ genGraph $ setCFGAddrs $ CFG i args newBlks newEdges

mergeBlocks :: ([Block], [(Int, Int)]) -> ([Block], [(Int, Int)])
mergeBlocks = linearMerge . emptyMerge


-- | Linear Merge

-- | If block A branches only to block B, and block B is branched to only by block A;
-- | that is, the edge between them is a bridge, then they can be merged.
-- | The instructions of B are appended to the instructions of A, minus the final branch.
-- | The incoming edges to the new block are the in-edges of A, and its outgoing edges are those
-- | of B.
linearMerge :: ([Block], [(Int, Int)]) -> ([Block], [(Int, Int)])
linearMerge (blks, edges)
 = let bridges = filter (isBridge edges) edges
   in joinBridges (blks, edges, bridges)

joinBridges :: ([Block], [(Int, Int)], [(Int, Int)]) -> ([Block], [(Int, Int)])
joinBridges beb@(blks, edges, bridges)
 = case bridges of     
        []   -> (blks, edges)
        _ -> joinBridges $ joinBridge beb

joinBridge :: ([Block], [(Int, Int)], [(Int, Int)]) -> ([Block], [(Int, Int)], [(Int, Int)])
joinBridge beb@(blks, edges, bridges)
 = case bridges of
        [] 
         -> beb
        (p, c):bs 
         -> let (pre, Block pId pInstrs pPreDets _ _, post)
                 = breakElem (\(Block i _ _ _ _) -> i == p) blks
                (Block cId cInstrs _ cPostDets cBranches)
                 = fromJust $ find (\(Block i _ _ _ _) -> i == c) blks
                newInstrsPre
                 = takeWhile (\(InstrNode i _ _ _) -> isNotBranch i) pInstrs
                newBlock 
                 = Block pId (newInstrsPre ++ cInstrs) pPreDets cPostDets cBranches
                newBlks
                 = filter (\(Block i _ _ _ _) -> i /= cId) (pre ++ [newBlock] ++ post)
                newEdges
                 = filter (\e -> e /= (p, c)) $ map (reRootFrom c p) edges
                newBridges
                 = filter (\(o, d) -> o /= d) $ map (reRootFrom c p) bs
            in (newBlks, newEdges, newBridges)
 where isNotBranch i
        = case i of
               IBranch{} -> False
               _         -> True
       reRootFrom c p (o, d) 
        = if o == c then (p, d) else (o, d)

isBridge :: [(Int, Int)] -> (Int, Int) -> Bool
isBridge edges (h, t)
 = snd (findDegree h edges) == 1 && fst (findDegree t edges) == 1 && h /= t && t /= 0

findDegree :: Int -> [(Int, Int)] -> (Int, Int)
findDegree bId edges
 = let ins   = length (filter (\(_, q) -> (q == bId)) edges)
       outs  = length (filter (\(p, _) -> (p == bId)) edges)
   in (ins, outs)

-- | We can remove linear chains as a crap way of avoiding 
-- |  having to redesignate edges in the bridge list. Unused.
removeChains :: [(Int, Int)] -> [(Int, Int)]
removeChains edges
 = case edges of
        []   -> []
        (p, c):es -> if not (any (\(t, _) -> t == c) es
                             || any (\(_, s) -> s == p) es)
                      then (p, c) : removeChains es
                      else removeChains es



-- | Empty Block Deletion

-- | A block is considered to be empty if it contains only a single unconditional jump, 
-- | or a load const followed by an unconditional jump, such that the load is used only
-- | used by the jump.
-- | If either of these conditions obtains, the block can be safely deleted, and the branches
-- | coming from other blocks redirected towards the new destination.
-- | A truly empty block with no instructions can never exist, as all unterminated blocks
-- | have branches inserted during the parsing stage.

emptyMerge :: ([Block], [(Int, Int)]) -> ([Block], [(Int, Int)])
emptyMerge be@(blks, _) = removeEmptyBlocks be $ emptyBlockIds blks

-- | Given a graph and a known list of empty blocks, remove each empty block from the graph.
removeEmptyBlocks :: ([Block], [(Int, Int)]) -> [Int] -> ([Block], [(Int, Int)])
removeEmptyBlocks be empties
 = case empties of 
        []   -> be
        e:es -> removeEmptyBlocks (removeEmptyBlock be e) es

removeEmptyBlock :: ([Block], [(Int, Int)]) -> Int -> ([Block], [(Int, Int)])
removeEmptyBlock (blks, edges) emptyBlkId
 = let newDest 
        = snd $ head $ filter (\(o, _) -> o == emptyBlkId) edges
       toRedirect 
        = map fst $ filter (\(_, d) -> d == emptyBlkId) edges
       (newBlks, newEdges)
        = redirectBlocks (blks, edges) toRedirect (emptyBlkId, newDest)
       fBlks 
        = filter (\(Block b _ _ _ _) -> b /= emptyBlkId) newBlks
       fEdges 
        = filter (\(o, d) -> o /= emptyBlkId && d /= emptyBlkId) newEdges
   in (fBlks, fEdges)


-- | When a block is removed, its antecedents must be redirected to point to the block it jumps to.
redirectBlocks :: ([Block], [(Int, Int)]) -> [Int] -> (Int, Int) -> ([Block], [(Int, Int)])
redirectBlocks be toRedirect conv
 = case toRedirect of
        []   -> be
        r:rs -> redirectBlocks (redirectBlock be r conv) rs conv

redirectBlock :: ([Block], [(Int, Int)]) -> Int -> (Int, Int) -> ([Block], [(Int, Int)])
redirectBlock (blks, edges) blkId (old, new)
 = let (pre, Block bId instrs preDets postDets branches, post)
        = breakElem (\(Block b _ _ _ _) -> b == blkId) blks
       newBranches 
        = map ifOldNew branches
       (iPre, instrN@(InstrNode (IBranch reg b1 b2) _ _ _), _)
        = breakElem isBranch instrs
       newBrN
        = setNodeInstr instrN (IBranch reg (ifOldNew b1) (ifOldNew b2))
       newBlk
        = Block bId (iPre ++ [newBrN]) preDets postDets newBranches
       newEdges
        = map (\e -> if e == (blkId, old) then (blkId, new) else e) edges
   in (pre ++ [newBlk] ++ post, newEdges)
 where ifOldNew b = if b == old then new else b
       

-- | Return a list of the ids of empty blocks in a graph.
emptyBlockIds :: [Block] -> [Int]
emptyBlockIds blks = map (\(Block bId _ _ _ _) -> bId) $ filter isEmptyBlock blks

isEmptyBlock :: Block -> Bool
isEmptyBlock (Block bId instrs _ _ brs)
 = length brs == 1 && emptyInstrs instrs && bId /= 0
 where emptyInstrs is
        = case is of
               [ InstrNode (IBranch _ b1 b2) _ _ _ ]         -> b1 == b2 && b1 /= bId
               [  InstrNode (IConst{}) cAddr _ cOut
                , InstrNode (IBranch _ b1 b2) bAddr bIn _ ]  -> b1 == b2 && b1 /= bId
                                                                 && cOut == [bAddr] 
                                                                 && bIn == [cAddr]
               _                                             -> False

