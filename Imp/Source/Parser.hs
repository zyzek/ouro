
module Imp.Source.Parser where
import Imp.Source.Exp
import Imp.Source.Tokens
import Imp.Parsec


-- | Whole program; everything at the top level is a function definition.
program  :: Parser Token Program
program 
 = do   funcs      <- some function
        return     $  Program funcs


-- | Single function definition: 
-- | keyword, function name, list of arguments, list of variables, then the function body itself.
function  :: Parser Token Function
function
 = do   only Kfun
        i          <- ident
        arg_list   <- argList ident
        var_list   <- funvars
        b          <- block i
        return     $  Function i arg_list var_list b


-- | Source block: a sequence of statements between curly braces.
block :: Id -> Parser Token Block
block curFuncId
 = alt  ( do   only KBraceBra
               stmt_list      <- some $ stmt curFuncId
               only KBraceKet
               return         $  Block stmt_list )

        ( do   only KBraceBra
               stmt_list      <- some $ stmt curFuncId
               e              <- expr curFuncId
               only KBraceKet
               return         $  Block (stmt_list ++ [SReturn e]) )


-- | Statement: assignments, conditionals, loops, returns, prints.
-- | Statements themselves contain expressions.
stmt :: Id -> Parser Token Stmt
stmt curFuncId
 = alts  [ assignStmt  curFuncId
         , fassignStmt curFuncId
         , bassignStmt curFuncId
         , ifelseStmt  curFuncId
         , ifthenStmt  curFuncId -- must appear after ifelse, being a prefix
         , whileStmt   curFuncId
         , returnStmt  curFuncId
         , printStmt   curFuncId
         , exprStmt    curFuncId
         ]


-- | Expression: a series of operations applied to sub-expressions.
-- | We start parsing at the lowest precedence level in order that looser operators
-- |  appear higher in the resulting AST. 
expr :: Id -> Parser Token Exp
expr = opL 1



-- || Component Parsers ==================================================

-- | Comma-separated list of objects.
commaList :: Parser Token a -> Parser Token [a]
commaList p
 = alt  ( do    o       <- p
                only KComma
                os      <- commaList p
                return  $ o : os )

        ( do    o       <- p
                return [o] )


-- | Comma-separated list of items between parentheses.
argList :: Parser Token a -> Parser Token [a]
argList p
 = do   only KRoundBra
        arg_list        <- alt (commaList p) $ result []
        only KRoundKet
        return arg_list


-- | Comma-separated list of ids, prefixed by "vars"
funvars :: Parser Token [Id]
funvars
 = alt  ( do   only Kvars
               commaList ident )

        ( result [] )


-- | Parse a number.
num   :: Parser Token Int
num = from takeNum


-- | Parse an identifier.
ident :: Parser Token Id
ident
 = do   i       <- from takeId
        return  $  Id i



-- || Operators ==========================================================
-- ||
-- ||  Expressions consist of left and right components, opL and opR, themselves expressions. 
-- ||  Each has a precedence level associated, so let an opL at precedence p be denoted opL(p),
-- ||  and similarly for opR.
-- || 
-- ||  An opL(p) consists of an opL(p+1), followed by  a possibly-empty string of opR(p).
-- ||  At the highest precedence level, an opL(7) resolves to an atom, which serves as a
-- ||  base case. The atoms may themselves contain expressions, however.
-- ||
-- ||  An opR(p) consists of a precedence-p operator followed by an opL(p+1).
-- ||  Each opR only supplies one operand to its operator, so in the calling opL, the string
-- ||  of opRs are stitched together, left-to-right, with each expression substituting into the 
-- ||  RHS of the previous one.
-- ||
-- ||  Atoms are indivisible units that operators act upon as a whole.
-- ||  Valid atoms: numeric literals, variable identifiers, function calls/ouroboroi,
-- ||  unary operations, parenthesised expressions, assignment expressions. 
-- ||
-- ||  The number 7 here is a bit of a hack: I would prefer a more elegant way than manually
-- ||  specifying that the precedence of atoms is maximal.

-- | Parse an atom.
atom :: Id -> Parser Token Exp
atom = opL 7


-- | Parse an opL.
opL :: Int -> Id -> Parser Token Exp
opL 7 curFuncId
 = alts [ unaryOpExpr curFuncId
        , appExpr     curFuncId  -- app must appear before ident
        , assignExpr  curFuncId
        , fAssignExpr curFuncId
        , bAssignExpr curFuncId
        , ouroExpr    curFuncId
        , parenExpr   curFuncId
        , identExpr
        , numExpr
        ]

opL prec curFuncId
 = do   l        <-   opL (prec + 1) curFuncId
        es       <-   some (opR prec curFuncId)
        return   $    case es of 
                           []   ->   l
                           _    ->   chainPartOps (reverse es) l


-- | Parse an opR.
-- |
-- | Note that the function operator must be treated differently from others,
-- |  as it must resolve to a function application in the intermediate language, 
-- |  rather than a binary operator.
opR :: Int -> Id -> Parser Token Exp
opR 6 curFuncId
 = do  XApp i (e1:_:arg_list)   <-   funcop curFuncId
       r                        <-   atom curFuncId
       return                   $    XApp i (e1:r:arg_list)

opR prec curFuncId
 = do  op                       <-   precOp prec
       r                        <-   opL (prec + 1) curFuncId
       return                   $    XOpBin op (XNum (-999)) r


-- | Take a string of binary operations and glue them together.
-- | Each operation is substituted into the LHS of the one following.
-- | The remaining LHS is filled with the expression passed in.  
chainPartOps :: [Exp] -> Exp -> Exp
chainPartOps xps l
 = case xps of
        [XOpBin op _ e2]        ->   XOpBin op l e2
        XOpBin op _ e2 : rest   ->   XOpBin op (chainPartOps rest l) e2
        [XApp f (_:r)]          ->   XApp f (l:r)
        XApp f (_:r) : rest     ->   XApp f (chainPartOps rest l : r)
        _                       ->   XNum (-999)


-- | Parse a func op; a function id followed by a list of args, between square brackets.
funcopsig :: Id -> Parser Token (Id, [Exp])
funcopsig curFuncId
 = do   only KSquareBra
        f                       <- ident
        arg_list                <- alt (commaList (expr curFuncId)) (result [])
        only KSquareKet
        return (f, arg_list)


-- | Parse a function operator; a function id followed by a list of arguments, 
-- |  between square brackets.
funcop :: Id -> Parser Token Exp
funcop curFuncId
 = do   (f, arg_list)           <- funcopsig curFuncId
        return                  $  XApp f $ XNum (-1000) : XNum (-1001) : arg_list


-- Parse operators of the given precedence level.
precOp :: Int -> Parser Token OpBin
precOp prec 
 = case lookup prec binopprecs of
        Just ops -> alts [ do   only  (KOp str)
                                return op
                         | (str, op)  <-  ops ]
        Nothing     -> zero


-- | A map from precedences to lists of operators with that precedence.
-- | Level 1 is the loosest precedence level, with higher numbers binding more tightly.
-- | Level 6 is for the function operator.
binopprecs :: [(Int, [(String, OpBin)])]
binopprecs = [ (1, [ ("|",  OpOr)
                   , ("&",  OpAnd)
                   , ("x|", OpXor) ]
                )
             , (2, [ ("<=", OpLeq)
                   , (">=", OpGeq)
                   , (">",  OpGt)
                   , ("<",  OpLt)
                   , ("==", OpEq)
                   , ("!=", OpNeq) ]
               )
             , (3, [ ("+",  OpAdd)
                   , ("-",  OpSub) ]
               )
             , (4, [ ("*",  OpMul)
                   , ("/",  OpDiv)
                   , ("%",  OpMod) ]
               )
             , (5, [ ("^", OpPow) ]
               )
             ]


-- | Binary operator names.
binops :: [(String, OpBin)]
binops = concatMap snd binopprecs


-- | Unary operator names.
unops :: [(String, OpUn)]
unops = [ ("!",  OpNot)
        , ("-",  OpNeg) ]


-- | Parse a binary operator.
binoper :: Parser Token OpBin
binoper = alts [ do   only    (KOp str)
                      return  op
               | (str, op)   <-  binops ]


-- | Parse a unary operator.
unoper :: Parser Token OpUn
unoper = alts [ do   only    (KOp str)
                     return  op
              | (str, op)   <-  unops]



-- || Atomic Expressions =================================================

-- | Single identifier.
identExpr :: Parser Token Exp
identExpr 
 = do  i        <-  ident
       return   $   XId i


-- | Ouroboros operation.
ouroExpr :: Id -> Parser Token Exp
ouroExpr curFuncId
 = do   only KAt
        arg_list  <-  argList (expr curFuncId)
        return    $   XApp curFuncId arg_list


-- | Function application.
appExpr :: Id -> Parser Token Exp
appExpr curFuncId
 = do   i         <-  ident
        arg_list  <-  argList (expr curFuncId)
        return    $   XApp i arg_list


-- | Numeric literal.
numExpr :: Parser Token Exp
numExpr
 = do  n       <-  num
       return  $   XNum n


-- | Parenthesised expression.
parenExpr :: Id -> Parser Token Exp
parenExpr curFuncId
 = do  only KRoundBra
       e    <- expr curFuncId
       only KRoundKet
       return e


-- | Unary prefix operator.
unaryOpExpr :: Id -> Parser Token Exp
unaryOpExpr curFuncId 
 = do   op      <-  unoper
        e       <-  atom curFuncId
        return  $   XOpUn op e


-- | Assignment expression.
assignExpr :: Id -> Parser Token Exp
assignExpr curFuncId
 = do   i          <- ident
        only KEquals
        e          <- expr curFuncId
        return     $  XAssign i e


-- | FAssign expression.
fAssignExpr :: Id -> Parser Token Exp
fAssignExpr curFuncId
 = do   i          <- ident
        (f, arg_l) <- funcopsig curFuncId
        only KEquals
        e          <- expr curFuncId
        return     $  XAssign i $ XApp f $ XId i : e : arg_l


-- | BAssign expression.
bAssignExpr :: Id -> Parser Token Exp
bAssignExpr curFuncId
 = do   i          <- ident
        o          <- binoper
        only KEquals
        e          <- expr curFuncId
        return     $  XAssign i $ XOpBin o (XId i) e



-- || Statements =========================================================

-- | Assignment, a list of expressions assigned to a list of variable ids.
assignStmt :: Id -> Parser Token Stmt
assignStmt curFuncId
 = do  is         <- commaList ident
       es         <- assignRHS curFuncId
       return     $  SAssign is es


-- | Function assignment.
fassignStmt :: Id -> Parser Token Stmt
fassignStmt curFuncId
 = do  is         <- commaList ident
       (f, arg_l) <- funcopsig curFuncId
       es         <- assignRHS curFuncId
       return     $  SAssign is $ map (\(i, e) -> XApp f (XId i : e : arg_l)) (zip is es)


-- | Operator assignment
bassignStmt :: Id -> Parser Token Stmt
bassignStmt curFuncId
 = do  is         <- commaList ident
       o          <- binoper
       es         <- assignRHS curFuncId
       return     $  SAssign is $ map (\(i, e) -> XOpBin o (XId i) e) (zip is es)


-- | The right side of a multiple assignment: equals sign, expressions, semicolon. 
assignRHS :: Id -> Parser Token [Exp]
assignRHS curFuncId
 = do  only KEquals
       es          <- commaList (expr curFuncId)
       only KSemi
       return es


-- | if-then
ifthenStmt :: Id -> Parser Token Stmt
ifthenStmt curFuncId
 = do  only Kif
       guard      <- expr curFuncId
       alt (only Kthen) (result Kthen)
       br         <- block curFuncId
       return     $  SIf guard br


-- | if-then-else
ifelseStmt :: Id -> Parser Token Stmt
ifelseStmt curFuncId
 = do  SIf guard lbr   <- ifthenStmt curFuncId
       alt (only Kelse) (result Kelse)
       rbr             <- block curFuncId
       return          $  SIfElse guard lbr rbr


-- | while
whileStmt :: Id -> Parser Token Stmt
whileStmt curFuncId
  = do only Kwhile
       cond       <- expr curFuncId
       body       <- block curFuncId
       return     $  SWhile cond body


-- | return
returnStmt :: Id -> Parser Token Stmt
returnStmt curFuncId
 = do  only Kreturn
       i          <- expr curFuncId
       only KSemi
       return     $  SReturn i


-- | print
printStmt :: Id -> Parser Token Stmt
printStmt curFuncId
 = do only Kprint
      es          <- alt (commaList (expr curFuncId)) (result [])
      only KSemi
      return      $  SPrint es


-- | naked expression
exprStmt :: Id -> Parser Token Stmt
exprStmt curFuncId
  = do   e        <- expr curFuncId
         only KSemi 
         return   $  SExp e
