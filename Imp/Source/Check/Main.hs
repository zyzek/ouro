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
             ns   -> map (\name -> ErrorFuncRedef (idString name)) ns


checkDupeVars :: Program -> [Error]
checkDupeVars (Program funs) = concat $ map checkFuncDupeVars funs 

checkFuncDupeVars :: Function -> [Error]   -- ?? Possibly merge this with checkFuncRedef?
checkFuncDupeVars (Function _ args vars _)
 = let names = (args ++ vars) 
   in  case repeated names of
             []    -> []
             ns    -> map (\name -> ErrorVarRedef (idString name)) ns


-- | TODO: Work out a traversal method.
-- | Checking for undefined variables, functions, plus invalid function signatures
-- | Requires a traversal of the AST, I think.

-- checkFuncVarsUndef :: Function -> [Error]
-- checkFuncVarsUndef (Function _ args vars body)

--checkUndefVars :: CheckNode -> [Id] -> [Error]
--checkUndefVars node varids
-- = case node of
--        Function _ args vars body -> checkFuncVars body (varids ++ args ++ vars)


-- | Get the name of a function.
nameOfFunction :: Function -> Id
nameOfFunction (Function name _ _ _) = name

-- | Get the name and number of arguments of a function
numFunctionArgs :: Function -> (Id, Int)
numFunctionArgs (Function name args _ _) = (name, (length args))

-- | Return elements appearing more than once in an input list
repeated :: Eq a => [a] -> [a]
repeated []      = []
repeated [_]     = []
repeated (x:xs)  = if (elem x xs) && not (elem x rest) 
                    then x:rest
                    else rest
                   where rest = repeated xs
