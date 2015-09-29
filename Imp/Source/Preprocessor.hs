module Imp.Source.Preprocessor where
--import Data.String.Utils
--import Imp.Parsec

preprocess :: String -> String
preprocess = id

--preprocessor :: String -> String
--preprocessor source 
-- = let (paths, rest) = head (parse includes [source])
--   in concat $ (map readFile paths) ++ rest


--include :: Parser Char String
--include =
--    do  (char '"')
--        s          <- some char
--        (char '"')
--        return     s

--includes :: Parser String [String]
--includes =
-- alts
-- [ do   i       <- include
--        some space
--        is      <- includes
--        return  $ i : is
-- , do   i       <- include
--        return  [i] ]