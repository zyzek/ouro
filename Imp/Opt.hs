module Imp.Opt where
import Imp.Opt.Exp
import Imp.Opt.Parser
import Imp.Opt.Optimiser
import Imp.Core.Tokens
import Imp.Core.Lexer
import Imp.Parsec

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

closureIds :: [CFG] -> [(Id, [Int])]
closureIds
 = map zeroClosure

blockClosure :: CFG -> CFG
blockClosure cfg
 = let (_, reachable) = zeroClosure cfg
   in retainBlocks cfg reachable 

minusUnreachInstrs :: CFG -> CFG
minusUnreachInstrs = removeUnreachedInstrs

