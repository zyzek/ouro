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


checkFuncRedef :: Program -> [Error]
checkFuncRedef (Program funs)
 = let  names = (map nameOfFunction funs)
   in   case repeated names of
             []   -> []
             ns   -> map (\name -> ErrorFuncRedef (idString name)) ns


checkVarRedef :: Program -> [Error]
checkVarRedef (Program funs) = concat $ map checkFuncDupeVars funs 


checkFuncDupeVars :: Function -> [Error]   -- ?? Possibly merge this with checkFuncRedef
checkFuncDupeVars (Function _ args vars _)
 = let names = (args ++ vars) 
   in  case repeated names of
             []    -> []
             ns    -> map (\name -> ErrorVarRedef (idString name)) ns


-- | The following collection of functions traverse the AST, looking for 
-- |  undefined variables, undefined functions, and incorrect function signatures.
-- | To accomplish this, a list of which variables are currently defined is passed around
-- |  along with an associative list from function ids to their number of arguments.

-- | TODO: Work out a better method of doing this.
-- |  current method seems like pure shit.
-- | Observation: concat-map composition looks list-monadic to me.

-- | Start point of the traversal. Here the function signatures list is populated. 
checkIdDefs :: Program -> [Error]
checkIdDefs (Program funs) 
 = let funsigs = map numFunctionArgs funs
       funerrors = map (checkIdDefsFun [] funsigs) funs 
   in nub (concat funerrors)
   where numFunctionArgs (Function name args _ _) = (name, (length args))


-- | Each function gets its own variable namespace, which is constructed here.
checkIdDefsFun :: [Id] -> [(Id, Int)] -> Function -> [Error]
checkIdDefsFun vars funsigs (Function _ args vardefs block) 
 = checkIdDefsBlock (vars ++ args ++ vardefs) funsigs block


checkIdDefsBlock :: [Id] -> [(Id, Int)] -> Block -> [Error]
checkIdDefsBlock vars funsigs (Block stmts) 
 = concat (map (checkIdDefsStmt vars funsigs) stmts) 


-- ?? TODO: Find a better way to do this: these concatenations are you gee elle why. 
checkIdDefsStmt :: [Id] -> [(Id, Int)] -> Stmt -> [Error]
checkIdDefsStmt vars funsigs stmt
 = case stmt of
    SAssign var expr        -> (checkIdDefsVar vars var) 
                              ++ (checkIdDefsExp vars funsigs expr)
    SFAssign var _ expr     -> (checkIdDefsVar vars var) 
                              ++ (checkIdDefsExp vars funsigs expr)
    SBAssign var _ expr     -> (checkIdDefsVar vars var) 
                              ++ (checkIdDefsExp vars funsigs expr)
    SIf expr block          -> (checkIdDefsExp vars funsigs expr)
                              ++ (checkIdDefsBlock vars funsigs block)
    SIfElse expr b1 b2      -> (checkIdDefsExp vars funsigs expr) 
                              ++ (checkIdDefsBlock vars funsigs b1) 
                              ++ (checkIdDefsBlock vars funsigs b2)
    SReturn expr            -> checkIdDefsExp vars funsigs expr
    SWhile expr b           -> (checkIdDefsExp vars funsigs expr)
                              ++ (checkIdDefsBlock vars funsigs b)
    SPrint expr        -> checkIdDefsExp vars funsigs expr
    SExp expr          -> checkIdDefsExp vars funsigs expr


checkIdDefsExp :: [Id] -> [(Id, Int)] -> Exp -> [Error]
checkIdDefsExp vars funsigs expr
 = case expr of
    XNum _              -> []
    XId i               -> checkIdDefsVar vars i
    XApp i args         -> checkFuncApp vars funsigs i args
    XOpUn _ e           -> checkIdDefsExp vars funsigs e
    XOpBin _ e1 e2      -> (checkIdDefsExp vars funsigs e1)
                          ++ (checkIdDefsExp vars funsigs e2)
    XAssign i e         -> (checkIdDefsVar vars i)
                          ++ (checkIdDefsExp vars funsigs e)
    XFAssign i _ e      -> (checkIdDefsVar vars i) 
                          ++ (checkIdDefsExp vars funsigs e)
    XBAssign i _ e      -> (checkIdDefsVar vars i) 
                          ++ (checkIdDefsExp vars funsigs e)


-- | Check that a function is defined, has the correct number of arguments, which must each exist.
--checkFuncApp :: [Id] -> [(Id, Int)] -> Id -> [Id] -> [Error]
--checkFuncApp vars funsigs name args
-- = case lookup name funsigs of
--   Just numargs     -> if (length args) /= numargs
--                        then (ErrorFuncSig (idString name) numargs):argsdef
--                        else argsdef
--   Nothing          -> (ErrorFuncUndef (idString name)):argsdef
--   where argsdef = (concat (map (checkIdDefsVar vars) args))

-- | Check that a function is defined, has the correct number of arguments, which must each exist.
checkFuncApp :: [Id] -> [(Id, Int)] -> Id -> [Exp] -> [Error]
checkFuncApp vars funsigs name args
 = case lookup name funsigs of
   Just numargs     -> if (length args) /= numargs
                        then (ErrorFuncSig (idString name) numargs):argsdef
                        else argsdef
   Nothing          -> (ErrorFuncUndef (idString name)):argsdef
   where argsdef = (concat (map (checkIdDefsExp vars funsigs) args))


-- | Check that a variable is defined. No need to pass in function signatures if this is a variable. 
checkIdDefsVar :: [Id] -> Id -> [Error]
checkIdDefsVar vars (Id s) = if not (elem (Id s) vars)
                               then [ErrorVarUndef s]
                               else []


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
