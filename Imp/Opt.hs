module Imp.Opt where
import Imp.Opt.CFG
import Imp.Opt.CFGUtils
import Imp.Opt.Parser
import Imp.Opt.Optimiser
import Imp.Core.Tokens
import Imp.Core.Lexer
import Imp.Parsec
import qualified Imp.Core.Exp  as C

lexParseCFG :: Parser Token [CFG] -> String -> Maybe [CFG]
lexParseCFG p str
 = case lexer str of
        Nothing     -> Nothing
        Just tokens'
         -> case parse p tokens' of
                [(cfgs, [])]       -> Just $ map genGraphEdges cfgs
                _               -> Nothing

cfgsOfString :: String -> Maybe [CFG]
cfgsOfString = lexParseCFG progCFGs

closureIds :: [CFG] -> [(C.Id, [Int])]
closureIds
 = map zeroClosure

blockClosure :: CFG -> CFG
blockClosure cfg
 = let (_, reachable) = zeroClosure cfg
   in retainBlocks cfg reachable 

unreach :: CFG -> CFG
unreach = regenCFGEdges . genGraphEdges . removeUnreachedInstrs . blockClosure

cfgsToProgram :: [CFG] -> C.Program
cfgsToProgram cfgs
 = let nodeToInst (InstrNode i _ _ _)
        = i
       cToBlock (Block i instrs _ _ _)
        = C.Block i $ map nodeToInst instrs
       cfgToFunc (CFG name args blks _)
        = C.Function name args $ map cToBlock blks
   in C.Program $ map cfgToFunc cfgs

optUntilFixed :: CFG -> CFG
optUntilFixed cfg
 = let stepped = (
                    coalesceCFG .
                    mutateRedundantInstrs .
                    removeDeadCode .
                    unreach) cfg
   in if stepped == cfg then cfg else optUntilFixed stepped
 
 
