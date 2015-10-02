
module Imp.Source.Check.Main where
import Imp.Source.Check.Error
import Imp.Source.Exp
import Data.List


-- | Check that the program contains a main function.
checkMain :: Program -> [Error]
checkMain (Program funs)
 = let  hasMain = foldl (||) False 
                $ map (== (Id "main")) 
                $ map nameOfFunction funs
   in   if hasMain 
         then []
         else [ErrorNoMain]


-- | Check whether any function has been defined more than once.
checkFuncRedef :: Program -> [Error]
checkFuncRedef (Program funs)
 = let  names = (map nameOfFunction funs)
   in   case repeated names of
             []   -> []
             ns   -> map (\name -> ErrorFuncRedef (idString name)) ns


-- | Check whether any variables have been defined more than once in the program.
checkVarRedef :: Program -> [Error]
checkVarRedef (Program funs)
 = concat $ map checkFuncVarsRedef funs 


-- | Check whether there are any variables defined more than once in a given function.
checkFuncVarsRedef :: Function -> [Error]
checkFuncVarsRedef (Function _ args vars _)
 = let  names = (args ++ vars) 
   in   case repeated names of
             []    -> []
             ns    -> map (\name -> ErrorVarRedef (idString name)) ns


-- | The following collection of functions traverse the AST, looking for 
-- |  undefined variables, undefined functions, and incorrect function signatures.
-- |
-- | To accomplish this, a list of which variables are currently defined is passed around
-- |  along with an associative list from function ids to their number of arguments.

-- | Start point of the traversal. Here the function signatures list is populated.
checkIds :: Program -> [Error]
checkIds (Program funs) 
 = let  funsigs   = map numFunctionArgs funs
        funerrors = map (checkFunIds [] funsigs) funs 
   in nub (concat funerrors)
   where numFunctionArgs (Function name args _ _) = (name, (length args))

-- | Check that all ids in a block are correctly invoked.
-- | Each function gets its own variable namespace, which is constructed here.
checkFunIds :: [Id] -> [(Id, Int)] -> Function -> [Error]
checkFunIds vars funsigs (Function _ args vardefs block) 
 = checkBlockIds (vars ++ args ++ vardefs) funsigs block


-- | Check the ids in a block.
checkBlockIds :: [Id] -> [(Id, Int)] -> Block -> [Error]
checkBlockIds vars funsigs (Block stmts) 
 = concat (map (checkStmtIds vars funsigs) stmts) 


-- | Check the ids in a statement.
checkStmtIds :: [Id] -> [(Id, Int)] -> Stmt -> [Error]
checkStmtIds vars funsigs stmt
 = case stmt of
        SAssign targs exprs    ->    (checkAssign vars funsigs targs exprs)

        SFAssign var _ expr    ->    (checkVarId vars var) 
                                      ++ (checkExpIds vars funsigs expr)

        SBAssign var _ expr    ->    (checkVarId vars var) 
                                      ++ (checkExpIds vars funsigs expr)

        SWhile expr b          ->    (checkExpIds vars funsigs expr)
                                      ++ (checkBlockIds vars funsigs b)

        SIf expr block         ->    (checkExpIds vars funsigs expr)
                                      ++ (checkBlockIds vars funsigs block)

        SIfElse expr b1 b2     ->    (checkExpIds vars funsigs expr) 
                                      ++ (checkBlockIds vars funsigs b1) 
                                      ++ (checkBlockIds vars funsigs b2)

        SReturn expr           ->    checkExpIds vars funsigs expr

        SPrint exprs           ->    concat (map (checkExpIds vars funsigs) exprs)

        SExp expr              ->    checkExpIds vars funsigs expr


-- | Check the ids in an expression.
checkExpIds :: [Id] -> [(Id, Int)] -> Exp -> [Error]
checkExpIds vars funsigs expr
 = case expr of
        XNum _                 ->    []

        XId i                  ->    checkVarId vars i

        XApp i args            ->    checkFuncApp vars funsigs i args

        XOpUn _ e              ->    checkExpIds vars funsigs e

        XOpBin _ e1 e2         ->    (checkExpIds vars funsigs e1)
                                      ++ (checkExpIds vars funsigs e2)

        XAssign i e            ->    (checkVarId vars i)
                                      ++ (checkExpIds vars funsigs e)

        XFAssign i _ e         ->    (checkVarId vars i) 
                                      ++ (checkExpIds vars funsigs e)

        XBAssign i _ e         ->    (checkVarId vars i) 
                                      ++ (checkExpIds vars funsigs e)

        XTernary e1 e2 e3      ->    (checkExpIds vars funsigs e1)
                                      ++ (checkExpIds vars funsigs e2)
                                      ++ (checkExpIds vars funsigs e3)


-- | Check that a variable is defined. 
checkVarId :: [Id] -> Id -> [Error]
checkVarId vars (Id s) 
 = if not (elem (Id s) vars)
    then [ErrorVarUndef s]
    else []


-- | Check that a called function is defined, and has the
-- |  correct number of arguments, which must be correct expressions.
checkFuncApp :: [Id] -> [(Id, Int)] -> Id -> [Exp] -> [Error]
checkFuncApp vars funsigs name args
 = case lookup name funsigs of
        Just numargs           -> if (length args) /= numargs
                                   then (ErrorFuncSig (idString name) numargs):argsdef
                                   else argsdef
        Nothing                -> (ErrorFuncUndef (idString name)):argsdef
   where argsdef = (concat (map (checkExpIds vars funsigs) args))


-- | Check that there are as many (defined) variables on the LHS of 
-- |  an assignment as there are (correct) expressions on the RHS. 
checkAssign :: [Id] -> [(Id, Int)] -> [Id] -> [Exp] -> [Error]
checkAssign vars funsigs targs exprs
 = let vlen = length targs
       elen = length exprs
   in 
    if elen == vlen
       then checks
       else (ErrorPolyAssign vlen elen):checks
   where checks = (concat (map (checkVarId vars) targs))
                   ++ (concat (map (checkExpIds vars funsigs) exprs))


-- | Utilities

-- | Get the name of a function.
nameOfFunction :: Function -> Id
nameOfFunction (Function name _ _ _) = name


-- | Extract the name of an id.
idString :: Id -> String
idString (Id s) = s


-- | Return elements appearing more than once in an input list
repeated :: Eq a => [a] -> [a]
repeated []      = []
repeated [_]     = []
repeated (x:xs)  = if (elem x xs) && not (elem x rest) 
                    then x:rest
                    else rest
                   where rest = repeated xs
