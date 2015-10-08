
module Imp.Source.Check.Main where
import Imp.Source.Check.Error
import Imp.Source.Exp
import Data.List

-- | Check that the program contains a main function.
checkMain :: Program -> [Error]
checkMain (Program funs)
 = let  hasMain = any ((== Id "main") . nameOfFunction) funs
   in   if hasMain 
         then []
         else [ErrorNoMain]


-- | Check whether any function has been defined more than once.
checkFuncRedef :: Program -> [Error]
checkFuncRedef (Program funs)
 = let  names = map nameOfFunction funs
   in   case repeated names of
             []   -> []
             ns   -> map (ErrorFuncRedef . idString) ns


-- | Check whether any variables have been defined more than once in the program.
checkVarRedef :: Program -> [Error]
checkVarRedef (Program funs)
 = concatMap checkFuncVarsRedef funs 


-- | Check whether there are any variables defined more than once in a given function.
checkFuncVarsRedef :: Function -> [Error]
checkFuncVarsRedef (Function _ args vars _)
 = let  names = (args ++ vars) 
   in   case repeated names of
             []    -> []
             ns    -> map (ErrorVarRedef . idString) ns


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
   where numFunctionArgs (Function name args _ _) = (name, length args)


-- | Check that all ids in a block are correctly invoked.
-- | Each function gets its own variable namespace, which is constructed here.
checkFunIds :: [Id] -> [(Id, Int)] -> Function -> [Error]
checkFunIds vars funsigs (Function _ args vardefs block) 
 = checkBlockIds (vars ++ args ++ vardefs) funsigs block


-- | Check the ids in a block.
checkBlockIds :: [Id] -> [(Id, Int)] -> Block -> [Error]
checkBlockIds vars funsigs (Block stmts) 
 = concatMap (checkStmtIds vars funsigs) stmts


-- | Check the ids in a statement.
checkStmtIds :: [Id] -> [(Id, Int)] -> Stmt -> [Error]
checkStmtIds vars funsigs stmt
 = case stmt of
        SAssign targs exprs    ->    checkAssign vars funsigs targs exprs

        SWhile expr b          ->    checkExpIds vars funsigs expr
                                      ++ checkBlockIds vars funsigs b

        SIf expr block         ->    checkExpIds vars funsigs expr
                                      ++ checkBlockIds vars funsigs block

        SIfElse expr b1 b2     ->    checkExpIds vars funsigs expr 
                                      ++ checkBlockIds vars funsigs b1
                                      ++ checkBlockIds vars funsigs b2

        SReturn expr           ->    checkExpIds vars funsigs expr

        SPrint exprs           ->    concatMap (checkExpIds vars funsigs) exprs

        SExp expr              ->    checkExpIds vars funsigs expr


-- | Check the ids in an expression.
checkExpIds :: [Id] -> [(Id, Int)] -> Exp -> [Error]
checkExpIds vars funsigs expr
 = case expr of
        XNum _                 ->    []

        XId i                  ->    checkVarId vars i

        XApp i args            ->    checkFuncApp vars funsigs i args

        XOpUn _ e              ->    checkExpIds vars funsigs e

        XOpBin _ e1 e2         ->    checkExpIds vars funsigs e1
                                      ++ checkExpIds vars funsigs e2

        XAssign i e            ->    checkVarId vars i
                                      ++ checkExpIds vars funsigs e

        XTernary e1 e2 e3      ->    checkExpIds vars funsigs e1
                                      ++ checkExpIds vars funsigs e2
                                      ++ checkExpIds vars funsigs e3


-- | Check that a variable is defined. 
checkVarId :: [Id] -> Id -> [Error]
checkVarId vars (Id s) 
 = [ErrorVarUndef s | Id s `notElem` vars]


-- | Check that a called function is defined, and has the
-- |  correct number of arguments, which must be correct expressions.
checkFuncApp :: [Id] -> [(Id, Int)] -> Id -> [Exp] -> [Error]
checkFuncApp vars funsigs name args
 = case lookup name funsigs of
        Just numargs           -> if length args /= numargs
                                   then ErrorFuncSig (idString name) numargs : argsdef
                                   else argsdef
        Nothing                -> ErrorFuncUndef (idString name) : argsdef
   where argsdef = concatMap (checkExpIds vars funsigs) args


-- | Check that there are as many (defined) variables on the LHS of 
-- |  an assignment as there are (correct) expressions on the RHS. 
checkAssign :: [Id] -> [(Id, Int)] -> [Id] -> [Exp] -> [Error]
checkAssign vars funsigs targs exprs
 = let vlen = length targs
       elen = length exprs
   in 
    if elen == vlen
       then checks
       else ErrorPolyAssign vlen elen : checks
   where checks = concatMap (checkVarId vars) targs
                   ++ concatMap (checkExpIds vars funsigs) exprs


-- | Check each function in a program to see what's reachable inside it, traversing recursively
-- |  through the AST to determine this.
-- | Warn upon unreachable/redundant code; Error if a function never returns. 
checkProgReachability :: Program -> ([Error], [Warning])
checkProgReachability (Program funs)
 = let (errs, wrns) = (unzip . map checkFuncReachability) funs
   in (concat errs, concat wrns)


-- | Return true iff a list of warnings contains at least one occurrence of WarningUnreachableAfter
cntnsUnreachWrn :: [Warning] -> Bool
cntnsUnreachWrn
 = let branchWrn w t = t || case w of
                                 WarningUnreachableAfter _ -> True
                                 FinalReturn               -> True
                                 _                         -> False
   in foldr branchWrn False


-- | Return any warnings: Error if there are no return statements (which would produce warnings)
checkFuncReachability :: Function -> ([Error], [Warning])
checkFuncReachability (Function i _ _ b) 
 = let fname = idString i 
       reach = checkBlockReachability fname b
   in ( [ErrorNoReturn fname | not (cntnsUnreachWrn reach)]
      , reach)


-- | Check each statement in a block recursively for reachability.
-- | Produce a warning at the first return statement which has further statements following it.
-- | If a return is the last statement, produce a silent "warning" FinalReturn to signify that
-- | This block returns, but no further statements are made redundant by it.
checkBlockReachability :: String -> Block -> [Warning]
checkBlockReachability wrnpath (Block stmts)
 = let checkstmtreach = checkStmtReachability wrnpath
       spanpair = break (cntnsUnreachWrn . checkstmtreach) stmts
       wrn = concatMap checkstmtreach $ fst spanpair 
       restwrnlists = map checkstmtreach $ snd spanpair
       restwrn = concat $ if null restwrnlists
                           then []
                           else init restwrnlists ++ [squelchLastWrn (last restwrnlists)]
   in if null restwrn 
       then if null wrn then [] else wrn
       else if null wrn then [head restwrn] else wrn ++ [head restwrn]
   where squelchLastWrn l = if null l
                             then []
                             else case last l of
                                       WarningUnreachableAfter _ -> init l ++ [FinalReturn]
                                       _                         -> l


-- | Return statements generate reachability warnings, which are handled in the enclosing block.
checkStmtReachability :: String -> Stmt -> [Warning]
checkStmtReachability wrnpath s
 = case s of
        SReturn _           ->  [WarningUnreachableAfter ("return in " ++ wrnpath)]
        SIfElse g blk1 blk2 ->  let b1wrn = checkBlockReachability 
                                             ("if-else true branch in " ++ wrnpath)
                                             blk1
                                    b2wrn = checkBlockReachability
                                             ("if-else false branch in " ++ wrnpath)
                                             blk2
                                    cnstg = case g of
                                                 XNum 0 -> [WarningUnreachableBranch
                                                             ("conditional then in "
                                                               ++ wrnpath)]
                                                 XNum _ -> [WarningUnreachableBranch
                                                             ("conditional else in "
                                                               ++ wrnpath)]
                                                 _      -> []
                                in if (not . cntnsUnreachWrn) b1wrn 
                                       || (not . cntnsUnreachWrn) b2wrn
                                    then cnstg
                                    else cnstg
                                          ++ [WarningUnreachableAfter ("if-else in " ++ wrnpath)]
                                          
        SIf g block         -> case g of 
                                    XNum 0 -> [WarningUnreachableBranch 
                                                ("if-then in " ++ wrnpath)]
                                    XNum _ -> WarningRedundantConditional wrnpath 
                                               : checkBlockReachability 
                                                  ("if-then in " ++ wrnpath)
                                                  block
                                    _      -> []
        SWhile g block      -> case g of
                                    XNum 0  -> [WarningUnreachableBranch 
                                                 ("loop never executes in " ++ wrnpath)]
                                    XNum _  -> WarningInfiniteLoop wrnpath
                                                : checkBlockReachability 
                                                   ("loop in " ++ wrnpath)
                                                   block
                                    _       -> []
        _                   -> []



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
repeated (x:xs)  = if (x `elem` xs) && (x `notElem` rest) 
                    then x:rest
                    else rest
                   where rest = repeated xs
