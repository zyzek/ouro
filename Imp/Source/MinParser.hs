
module Imp.Source.MinParser where
import Imp.Source.Exp
import Imp.Source.Tokens
import Imp.Parsec

-- || This is a version of Parser.hs, but with everything that isn't on the spec sheet ripped out.

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
        arg_list   <- idArgs
        var_list   <- vars
        b          <- block
        return     $  Function i arg_list var_list b


-- | Source block: a sequence of statements between curly braces.
block :: Parser Token Block
block
 = do   only KBraceBra
        stmt_list      <- some stmt
        only KBraceKet
        return         $  Block stmt_list


-- | Statement: assignments, conditionals, loops, returns, prints.
-- | Statements themselves contain expressions.
stmt :: Parser Token Stmt
stmt
 = alts  [ assignStmt
         , ifelseStmt
         , ifthenStmt  -- must appear after ifelse, being a prefix
         , whileStmt
         , returnStmt
         ]

-- | Expression: as a series of operations applied to sub-expressions.
-- | We start parsing at the lowest precedence level in order that looser operators
-- |  appear higher in the AST. 
expr :: Parser Token Exp
expr = opL 1



-- || Component Parsers ==================================================

-- | Comma-separated list of ids between parentheses.
idArgs :: Parser Token [Id]
idArgs
 = do   only KRoundBra
        arg_list       <- alt idents $ result []
        only KRoundKet
        return arg_list


-- | Parse arguments as ids separated by commas.
idents :: Parser Token [Id]
idents
 = alt  ( do   i       <- ident
               only KComma
               is      <- idents
               return  $  i : is )

        ( do   i       <- ident
               return  [i] )


-- | Comma-separated list of ids, prefixed by "vars"
vars :: Parser Token [Id]
vars
 = alt  ( do   only Kvars
               idents )

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

-- | Expressions consist of left and right components, opL and opR, themselves expressions. 
-- | Each has a precedence level associated, so let an opL at precedence p be denoted opL(p),
-- |  and similarly for opR.
-- | 
-- | An opL(p) consists of an opL(p+1), followed by  a possibly-empty string of opR(p).
-- | At the highest precedence level, an opL(6) resolves to an atom, which serves as a
-- |  base case.
-- |
-- | An opR(p) consists of a precedence-p operator followed by an opL(p+1).
-- | Each opR only supplies one operand to its operator, so in the calling opL, the string
-- |  of opRs are stitched together, left-to-right, with each expression substituting into the 
-- |  RHS of the previous one.
-- |
-- | Atoms are indivisible units that operators act upon as a whole.
-- | Valid atoms: numeric literals, variable identifiers, function calls, paren expressions. 
-- |
-- | The number 6 here is a bit of a hack: I would prefer a more elegant way than manually
-- |  specifying that the precedence of atoms is maximal.

-- | Parse an atom.
atom :: Parser Token Exp
atom = opL 6


-- | Parse an opL.
opL :: Int -> Parser Token Exp
opL 6
 = alts [ 
          appExpr    -- app must appear before ident
        , parenExpr
        , identExpr
        , numExpr
        ]

opL prec
 = do   l        <-   opL (prec + 1)
        es       <-   some (opR prec)
        return   $    case es of 
                           []   ->   l
                           _    ->   chainPartOps (reverse es) l


-- | Parse an opR.
-- |
-- | Note that the function operator must be treated differently from others,
-- |  as it must resolve to a function application in the intermediate language, 
-- |  rather than a binary operator.
opR :: Int -> Parser Token Exp
opR prec
 = do  op                       <-   precOp prec
       r                        <-   opL (prec + 1)
       return                   $    XOpBin op (XNum (-999)) r


-- | Take a string of binary operations and glue them together.
-- | Each operation is substituted into the LHS of the one following.
-- | The remaining LHS is filled with the expression passed in.  
chainPartOps :: [Exp] -> Exp -> Exp
chainPartOps xps l
 = case xps of
        [XOpBin op _ e2]        ->   XOpBin op l e2
        XOpBin op _ e2 : rest   ->   XOpBin op (chainPartOps rest l) e2
        _                       ->   XNum (-999)


-- Parse operators of the given precedence level.
precOp :: Int -> Parser Token OpBin
precOp prec 
 = case lookup prec opprecs of
        Just opers -> alts [ do   only  (KOp str)
                                  return op
                           | (str, op)  <-  opers ]
        Nothing    -> zero


-- | A map from precedences to lists of operators with that precedence.
-- | Level 1 is the loosest precedence level, with higher numbers binding more tightly.
opprecs :: [(Int, [(String, OpBin)])]
opprecs = [ (1, [ ("|",  OpOr)
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
ops :: [(String, OpBin)]
ops = concatMap snd opprecs


-- | Parse a binary operator.
oper :: Parser Token OpBin
oper = alts [ do   only    (KOp str)
                   return  op
            | (str, op)   <-  ops ]



-- || Atomic Expressions =================================================

-- | Single identifier.
identExpr :: Parser Token Exp
identExpr 
 = do  i        <-  ident
       return   $   XId i


-- | Function application.
appExpr :: Parser Token Exp
appExpr
 = do   i         <-  ident
        arg_list  <-  idArgs
        return    $   XApp i $ map XId arg_list


-- | Numeric literal.
numExpr :: Parser Token Exp
numExpr
 = do  n       <-  num
       return  $   XNum n


-- | Parenthesised expression.
parenExpr :: Parser Token Exp
parenExpr
 = do  only KRoundBra
       e    <- expr
       only KRoundKet
       return e



-- || Statements =========================================================

-- | Assignment, an expression assigned to a variable id.
assignStmt :: Parser Token Stmt
assignStmt
 = do  i         <- ident
       only KEquals
       e          <- expr
       only KSemi
       return     $  SAssign [i] [e]


-- | if-then
ifthenStmt :: Parser Token Stmt
ifthenStmt
 = do  only Kif
       guard      <- expr
       only Kthen
       br         <- block
       return     $  SIf guard br


-- | if-then-else
ifelseStmt :: Parser Token Stmt
ifelseStmt
 = do  SIf guard lbr   <- ifthenStmt
       only Kelse
       rbr             <- block
       return          $  SIfElse guard lbr rbr


-- | while
whileStmt :: Parser Token Stmt
whileStmt
 = do  only Kwhile
       cond       <- expr
       body       <- block
       return     $  SWhile cond body


-- | return
returnStmt :: Parser Token Stmt
returnStmt
 = do  only Kreturn
       i          <- expr
       only KSemi
       return     $  SReturn i

