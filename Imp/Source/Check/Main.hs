module Imp.Source.Check.Main where
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Check that the program contains a main function.
checkMain :: Program -> [Error]
checkMain (Program funs)
 = let  hasMain = foldl (||) False 
                $ map (== (Id "main")) 
                $ map nameOfFunction funs

   in   if hasMain 
         then []
         else [ErrorNoMain]


checkFuncRedef :: Program -> [Error]
checkFuncRedef (Program funs)
 = let  names = (map nameOfFunction funs)
   in   case repeated names of
             []   -> []
             ns -> map (\name -> ErrorFuncRedef (extractIDString name)) ns


-- | Get the name of a function.
nameOfFunction :: Function -> Id
nameOfFunction (Function name _ _ _) = name


-- | Return elements appearing more than once in an input list
repeated :: Eq a => [a] -> [a]
repeated []      = []
repeated [_]     = []
repeated (x:xs)  = if (elem x xs) && not (elem x rest) 
                    then x:rest
                    else rest
                   where rest = repeated xs

