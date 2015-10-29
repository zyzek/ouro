module Imp.Opt where
import Imp.Opt.Exp
import Imp.Opt.Parser
import Imp.Opt.Optimiser
import Imp.Core.Tokens
import Imp.Core.Lexer
import Imp.Parsec

lexParse :: Parser Token a -> String -> Maybe a
lexParse p str
 = case lexer str of
        Nothing     -> Nothing
        Just tokens'
         -> case parse p tokens' of
                [(x, [])]       -> Just x
                _               -> Nothing

cfgsOfString :: String -> Maybe [CFG]
cfgsOfString = lexParse progCFGs

reachables :: String -> Maybe [[Int]]
reachables s
 = do cfgs      <- cfgsOfString s
      return    $  map zeroClosure cfgs

