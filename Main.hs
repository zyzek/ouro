
module Main where
import qualified Imp.Source                     as S
import qualified Imp.Source.Lexer               as S
import qualified Imp.Source.Check               as S
import qualified Imp.Source.Check.Error         as S
import qualified Imp.Source.Convert             as S
import qualified Imp.Source.Preprocessor        as S

import qualified Imp.Core                       as C
import qualified Imp.Core.Interpreter           as C
import qualified Imp.Core.Transcriber           as C
import qualified Imp.Core.Lexer                 as C

import qualified Imp.Opt                        as O
import qualified Imp.Opt.Optimiser              as O

import qualified Data.Algorithm.Diff            as Diff
import qualified Data.Algorithm.DiffOutput      as Diff
import qualified Text.Show.Pretty               as Text
import qualified System.Environment             as System
import qualified System.Directory               as System

import Control.Monad
import Data.List
import Data.Maybe


main :: IO ()
main  
  = do  flags     <- System.getArgs
        
        -- Determine which grammar the user has chosen, and use it. -e for extended.
        let lang
             | not (null flags) = head flags
             | otherwise        = ""

        let (args, progOfString, lexer)
             | lang == "-e" = (tail flags, S.programOfString, S.lexer)
             | otherwise    = (flags, S.minProgramOfString, S.minlexer)

        case args of

         -- Lex a file.
         ["-lex",   file]
          | ".imp" `isSuffixOf` file 
          -> do contents   <- readFile file
                str        <- S.preprocess contents file
                let out = Text.ppShow $ lexer str
                showResult out (file ++ ".lex")
          
          | ".ir" `isSuffixOf` file
          -> do contents   <- readFile file
                let out = Text.ppShow $ C.lexer contents
                showResult out (file ++ ".lex")
          
          | otherwise
          -> error $ "Cannot lex " ++ file

         -- Parse a file.
         ["-parse", file]
          | ".imp" `isSuffixOf` file
          -> do contents   <- readFile file
                str     <- S.preprocess contents file
                let out = Text.ppShow $ progOfString str
                showResult out (file ++ ".parse")
          
          | ".ir" `isSuffixOf` file
          -> do contents <- readFile file
                let out = Text.ppShow $ C.programOfString contents
                showResult out (file ++ ".parse")
  
          | otherwise
          -> error $ "Cannot parse " ++ file

         -- Check a file.
         ["-check", file]
          | ".imp" `isSuffixOf` file
          -> do contents     <- readFile file
                str          <- S.preprocess contents file
                case progOfString str of
                 Nothing -> error "Syntax error."
                 Just prog
                  -> do let (errs, wrns) = S.checkProgram prog
                            errmsg err = "Error: " ++ S.prettyError err
                            wrnmsg w = case w of 
                                            S.FinalReturn -> ""
                                            _             -> "Warning: " ++ S.prettyWarn w
                            errout = case unlines 
                                           $ map errmsg errs of
                                          ""  -> "No Errors."
                                          es  -> es
                            wrnout = unlines $ filter (not . null) $ map wrnmsg wrns
                            out    = errout ++ "\n" ++ wrnout
                        showResult out (file ++ ".check")

          | otherwise
          -> error $ "Cannot check " ++ file

          -- Convert a file.
         ["-convert", file]
          | ".imp" `isSuffixOf` file
          -> do contents     <- readFile file
                str          <- S.preprocess contents file
                case progOfString str of
                 Nothing -> error "Syntax error."
                 Just progSource
                  -> do let core = S.convertProgram progSource
                        let out  = C.progString core
                        showResult out (file ++ ".convert")
                
          | otherwise
          -> error $ "Cannot convert " ++ file

          -- Generate the CFG of some IR code.
         ["-cfg", file]
          | ".ir" `isSuffixOf` file
          -> do contents    <- readFile file
                let out = Text.ppShow $ O.cfgsOfString contents
                showResult out (file ++ ".cfg")
          | otherwise
          -> error $ "Cannot parse " ++ file
         
         ["-cir", file]
          | ".ir" `isSuffixOf` file
          -> do contents    <- readFile file
                let out = C.progString $ O.cfgsToProgram $ fromJust $ O.cfgsOfString contents
                showResult out (file ++ ".cfg")
          | otherwise
          -> error $ "Cannot parse " ++ file

          -- Produce the ids of the blocks in each CFG's zero closure.
         ["-cid", file]
          | ".ir" `isSuffixOf` file
          -> do contents     <- readFile file
                let cfgs = fromJust $ O.cfgsOfString contents
                let out  = Text.ppShow $ O.closureIds cfgs
                showResult out (file ++ ".cid")
          | otherwise
          -> error $ "Cannot parse " ++ file
          
          -- Eliminate unreachable IR instructions/blocks.
         ["-unreach", file]
          | ".ir" `isSuffixOf` file         
          -> do contents    <- readFile file
                let cfgs     = fromJust $ O.cfgsOfString contents
                let out = C.progString $ O.cfgsToProgram $ map O.blockClosure cfgs
                showResult out (file ++ ".cb")
          | otherwise
          -> error $ "Cannot parse " ++ file

         ["-cb", file]
          | ".ir" `isSuffixOf` file         
          -> do contents    <- readFile file
                let cfgs     = fromJust $ O.cfgsOfString contents
                let out = Text.ppShow $ map O.blockClosure cfgs
                showResult out (file ++ ".cb")
          | otherwise
          -> error $ "Cannot parse " ++ file
        
          -- Remove dead code.
         ["-dead", file] 
          | ".ir" `isSuffixOf` file
          -> do contents  <- readFile file
                let cfgs   = fromJust $ O.cfgsOfString contents
                let out    = C.progString $ O.cfgsToProgram $ map O.removeDeadCode cfgs
                showResult out (file ++ ".dead")
          | otherwise
          -> error $ "Cannot parse " ++ file

         ["-cd", file]
          | ".ir" `isSuffixOf` file
          -> do contents   <- readFile file
                let cfgs    = fromJust $ O.cfgsOfString contents
                let out     = Text.ppShow $ map O.removeDeadCode cfgs
                showResult out (file ++ ".cd")
          | otherwise
          -> error $ "Cannot parse " ++ file
            
          -- Modify instructions in order to, for example, remove redundant ones.
         ["-cr", file]
          | ".ir" `isSuffixOf` file
          -> do contents   <- readFile file
                let cfgs   =  fromJust $ O.cfgsOfString contents
                let out    =  Text.ppShow $ map O.mutateGraph cfgs
                showResult out (file ++ ".cr")
          | otherwise
          -> error $ "Cannot parse " ++ file
        
         ["-mutate", file]
          | ".ir" `isSuffixOf` file
          -> do contents  <- readFile file
                let cfgs   = fromJust $ O.cfgsOfString contents
                let out    = C.progString $ O.cfgsToProgram $ map O.mutateGraph cfgs
                showResult out (file ++ ".redund")
          | otherwise
          -> error $ "Cannot parse " ++ file

         -- Merge mergable blocks
         ["-cm", file]
          | ".ir" `isSuffixOf` file
          -> do contents   <- readFile file
                let cfgs   =  fromJust $ O.cfgsOfString contents
                let out    =  Text.ppShow $ map O.coalesceCFG cfgs
                showResult out (file ++ ".cm")
          | otherwise
          -> error $ "Cannot parse " ++ file
        
         ["-merge", file]
          | ".ir" `isSuffixOf` file
          -> do contents  <- readFile file
                let cfgs   = fromJust $ O.cfgsOfString contents
                let out    = C.progString $ O.cfgsToProgram $ map O.coalesceCFG cfgs
                showResult out (file ++ ".merge")
          | otherwise
          -> error $ "Cannot parse " ++ file

        
          -- Optimise until stable
         ["-fix", file]
          | ".ir" `isSuffixOf` file
          -> do contents  <- readFile file
                let cfgs   = fromJust $ O.cfgsOfString contents
                let out    = C.progString $ O.cfgsToProgram $ map O.optUntilFixed cfgs
                showResult out (file ++ ".fix")
          | otherwise
          -> error $ "Cannot parse " ++ file
         
         ["-cf", file]
          | ".ir" `isSuffixOf` file
          -> do contents  <- readFile file
                let cfgs   = fromJust $ O.cfgsOfString contents
                let out    = Text.ppShow $ map O.optUntilFixed cfgs
                showResult out (file ++ ".cf")
          | otherwise
          -> error $ "Cannot parse " ++ file

          -- Interpret a file.
         ("-interpret":file:progArgs)
          | ".imp" `isSuffixOf` file
          -> do contents   <- readFile file
                str        <- S.preprocess contents file
                case progOfString str of
                     Nothing -> error "Syntax error."
                     Just progSource
                      -> do let core = S.convertProgram progSource
                            result <- C.startProgram core (map read progArgs)
                            showResult (show result) (file ++ ".interpret")

          | ".ir" `isSuffixOf` file
          -> do contents  <- readFile file
                case C.programOfString contents of
                     Nothing -> error "Syntax error."
                     Just mprog
                      -> do result <- C.startProgram mprog (map read progArgs)
                            showResult (show result) (file ++ ".interpret")

          | otherwise
          -> error $ "Cannot interpret " ++ file

         _ -> help


-- | Display command-line help.
help :: IO ()
help
 = putStr
 $ unlines
        [ "imp'n it up"
        , ""
        , "  imp -lex       <file>      Lex a file."
        , "  imp -parse     <file>      Parse a file."
        , "  imp -check     <file>      Check a file for problems."
        , "  imp -convert   <file>      Convert a file from source to core." 
        , "  imp -interpret <file>      Interpret a file." 
        , ""
        , "Use -e as the first flag to use an extended language instead, e.g.:"
        , ""
        , "  imp -e -interpret <file>   Interpret a file in the extended language."
        , ""
        , "This program will accept .imp (source) and .ir (intermediate language) files."
        , "-lex, -parse, and -interpret should work on both .imp and .ir extensions, while"
        , "  -check and -convert are restricted to .imp source code."
        ]


-- | Given an result string and the path to a file containing the expected
--   output, if the file exists then show the diff between the actual and
--   expected, otherwise just show the expected.
showResult :: String -> FilePath -> IO ()
showResult strResult fileExpected
 = do   
        putStrLn strResult
        exists  <- System.doesFileExist fileExpected

        when exists
         $ do   strExpected <- readFile fileExpected
                
                unless (null strExpected)
                 $ do   let diff    = Diff.ppDiff 
                                    $ Diff.getGroupedDiff
                                        (lines strResult)
                                        (lines strExpected)
                        if diff == "\n"
                         then putStrLn "\nOK"
                         else putStrLn $ "\nDIFF\n" ++ diff
