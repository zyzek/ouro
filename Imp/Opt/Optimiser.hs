
module Imp.Opt.Optimiser where
import Imp.Opt.Exp
import Data.List
import Data.List.Utils

zeroClosure :: CFG -> (Id, [Int])
zeroClosure cfg@(CFG i _ _) = (i, closure cfg [0])


closure :: CFG -> [Int] -> [Int]
closure cfg nodes
  | nodes == stepped   = nodes 
  | otherwise          = closure cfg stepped
 where stepped = reachStep cfg nodes


reachStep :: CFG -> [Int] -> [Int]
reachStep (CFG _ _ edges) reached
 = let isOrig n (Edge o _)
        = o == n
       getDest (Edge _ d)
        = d
       edgeDests n
        = map getDest (filter (isOrig n) edges)
   in map head $ group $ merge reached $ concatMap edgeDests reached


-- | Take a CFG, a list of block numbers to keep, 
-- | remove blocks not in the list, edges with endpoints not in the list.
retainBlocks :: CFG -> [Int] -> CFG
retainBlocks (CFG name blocks edges) toRetain
 = let keptBlocks = filter (\(Block i _) -> i `elem` toRetain) blocks
       keptEdges  = filter (\(Edge i j)  -> i `elem` toRetain || j `elem` toRetain) edges
   in CFG name keptBlocks keptEdges
