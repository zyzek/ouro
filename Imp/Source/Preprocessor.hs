module Imp.Source.Preprocessor where
import Imp.Parsec
import Data.List
import Data.List.Utils


preprocess :: String -> String -> IO String -- (IO String, String)
preprocess source main
 = let (paths, rest) 
        = case (parse includes source) of
            [] -> ([], source)
            l  -> head l
       readFileAddLine path
        = if ((head path) == '.' || (head path) == '/') 
           then (do contents <- readFile ((intercalate "/" (init (split "/" main))) ++ "/" ++ path ++ ".imp")
                    return $ contents ++ "\n")
           else (do contents <- readFile ("lib/" ++ path ++ ".imp")
                    return $ contents ++ "\n")
       allContents 
        = do contents <- fmap concat $ mapM readFileAddLine paths
             return contents
   in 
    do c <- allContents
       return $ c ++ rest

include :: Parser Char String
include =
    do  some space
        match "#include"
        some space
        (char '"')
        s          <- some $ notChars ['"']
        (char '"')
        some space
        return     s

includes :: Parser Char [String]
includes =
 alts
 [ do   i       <- include
        some space
        is      <- includes
        return  $ i : is
 , do   i       <- include
        return  [i] ]