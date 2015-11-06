
module Imp.Opt where
import Imp.Opt.CFG
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
                [(cfgs, [])]       -> Just $ map genGraph cfgs
                _               -> Nothing

cfgsOfString :: String -> Maybe [CFG]
cfgsOfString = lexParseCFG progCFGs


-- Convert from a CFG back to a Program
cfgsToProgram :: [CFG] -> C.Program
cfgsToProgram cfgs
 = let nodeToInst (InstrNode i _ _ _)
        = i
       cToBlock (Block i instrs _ _ _)
        = C.Block i $ map nodeToInst instrs
       cfgToFunc (CFG name args blks _)
        = C.Function name args $ map cToBlock blks
   in C.Program $ map cfgToFunc cfgs


-- Apply all available optimisations in sequence until we reach a fixed point.
optUntilFixed :: CFG -> CFG
optUntilFixed cfg
 = let stepped = (
                    coalesceCFG .
                    mutateGraph .
                    removeDeadCode .
                    blockClosure) cfg
   in if stepped == cfg then cfg else optUntilFixed stepped
 
