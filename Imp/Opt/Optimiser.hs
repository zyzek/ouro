
module Imp.Opt.Optimiser where
import Imp.Opt.Exp
import Data.List
import Data.List.Utils

zeroClosure :: CFG -> [Int]
zeroClosure cfg = closure cfg [0]

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
