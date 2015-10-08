
module Imp.Source.Preprocessor where
import Imp.Parsec
import Data.List
import Data.List.Utils


-- | Preprocess a source file.
-- | Take the source code as a string, and the path to the main file.
-- | Return a string with the code of any included libraries prepended.
-- |
-- | #include directives are required to be at the start of the file, before anything else.
preprocess :: String -> String -> IO String
preprocess source main
 = preprocmemo source main [main]


-- | For each file included, also preprocess its source recursively.
-- |
-- | A list of the paths of the files already included is passed in,
-- |  so that any file already included can be skipped, 
-- |  which resolves circular and multiple inclusion
-- |
-- | Keep processing until the list of included files is no longer changing.
preprocmemo :: String -> String -> [String] -> IO String
preprocmemo source main fnames
 = let (paths, rest) 
        = case parse includes source of
               [] -> ([], source)
               l  -> head l
       
       readFileAddLine path
        | path `elem` fnames
          = return ""
        | head path == '.' || head path == '/'
          = do  contents  <-  readFile (intercalate "/" (init (split "/" main))
                                         ++ "/" ++ path ++ ".imp")
                return $ contents ++ "\n"
        | otherwise
          = do  contents  <-  readFile ("lib/" ++ path ++ ".imp")
                return $ contents ++ "\n"
       
       allContents 
        = concat <$> mapM readFileAddLine paths
       
       newfnames
        = nub (fnames ++ paths)
   
   in
    if null (newfnames \\ fnames)
     then
      do c <- allContents
         return $ c ++ rest
     else
      do c <- allContents
         preprocmemo (c ++ rest) main newfnames


-- | Parse a single include.
include :: Parser Char String
include =
    do  some space
        match "#include"
        some space
        char '"'
        s          <- some $ notChars "\""
        char '"'
        some space
        return     s


-- | Parse a sequence of includes, separated by whitespace.
includes :: Parser Char [String]
includes =
 alts
 [ do   i       <- include
        some space
        is      <- includes
        return  $ i : is
 , do   i       <- include
        return  [i] ]
