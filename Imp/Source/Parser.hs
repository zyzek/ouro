
module Imp.Source.Parser where
import Imp.Source.Exp
import Imp.Source.Tokens
import Imp.Parsec
--import Data.Maybe   


-- | Whole program.
program  :: Parser Token Program
program 
 = do   funcs    <- (some function)
        return   $ Program funcs


function  :: Parser Token Function
function
  = do      only Kfun
            i          <- ident
            arg_list   <- args
            var_list   <- vars
            b          <- block
            return     $ Function i arg_list var_list b


args :: Parser Token [Id]
args
 = do   only KRoundBra
        arg_list       <- (alt idents (result []))
        only KRoundKet
        return arg_list

exprArgs :: Parser Token [Exp]
exprArgs
 = do   only KRoundBra
        arg_list       <- (alt exprs (result []))
        only KRoundKet
        return arg_list


vars :: Parser Token [Id]
vars
 = alt  (do  only Kvars
             v_list    <- idents
             return v_list)
        (result [])

block :: Parser Token Block
block 
 = do   only KBraceBra
        stmt_list      <- (some stmt)
        only KBraceKet
        return         $ Block stmt_list


stmt :: Parser Token Stmt
stmt
 = alts 
 [ -- assignment
   do  i          <- ident
       only KEquals
       e          <- expr
       only KSemi
       return     $ SAssign i e
 , do  i          <- ident
       only KRoundBra
       f          <- ident
       only KRoundKet
       only KEquals
       e          <- expr
       only KSemi
       return     $ SFAssign i f e
 , do  i          <- ident
       only KRoundBra
       f          <- binoper
       only KRoundKet
       only KEquals
       e          <- expr
       only KSemi
       return     $ SBAssign i f e

   -- if-then-else
   --  Must appear before if-then; Since if-then is a prefix 
   --  of if-then-else, if the match for the latter fails, 
   --  it might possibly still succeed for if-then.
   --  But if if-then comes first, then if it matches, 
   --  and there's an else block following, the whole parse 
   --  must then fail: we have a dangling else with no 
   --  if-then to precede it.
 , do  only Kif
       guard      <- expr
       only Kthen
       lbr        <- block
       only Kelse
       rbr        <- block
       return     $ SIfElse guard lbr rbr

   -- if-then
 , do  only Kif
       guard      <- expr
       only Kthen
       br         <- block
       return     $ SIf guard br

  -- return
 , do  only Kreturn
       i <- expr
       only KSemi
       return $ SReturn i

  -- while
  , do only Kwhile
       cond       <- expr
       body       <- block
       return     $  SWhile cond body

  -- print
  , do only Kprint
       e          <- expr
       only KSemi
       return     $  SPrint e

  -- naked expression
  , do   e        <- expr
         only KSemi 
         return   $  SExp e
 ]



-- | Parse a number.
num   :: Parser Token Int
num = from takeNum


-- | Parse an identifier.
ident :: Parser Token Id
ident
 = do   i       <- from takeId
        return  $ Id i


-- | Parse arguments as Ids separated by commas.
idents :: Parser Token [Id]
idents
 = alts
 [ do   i       <- ident
        only KComma
        is      <- idents
        return  $ i : is

 , do   i       <- ident
        return  [i]
 ]


-- | Parse arguments as Expressions separated by commas.
exprs :: Parser Token [Exp]
exprs
 = alts
 [ do   i       <- expr
        only KComma
        is      <- exprs
        return  $ i : is

 , do   i       <- expr
        return  [i]
 ]


-- | Parse an operator.
binoper :: Parser Token OpBin
binoper
 = alts
 [ do   only    (KOp str)
        return  op
 | (str, op)    <- binops]

unoper :: Parser Token OpUn
unoper
 = alts
 [ do   only    (KOp str)
        return  op
 | (str, op)    <- unops]


-- | Operator names.
binops :: [(String, OpBin)]
binops
 =      [ ("+",  OpAdd)
        , ("-",  OpSub)
        , ("*",  OpMul)
        , ("/",  OpDiv)
        , ("%",  OpMod)
        , ("^",  OpPow)
        , ("<",  OpLt)
        , (">",  OpGt)
        , ("==", OpEq)
        , (">=", OpGeq)
        , ("<=", OpLeq)
        , ("!=", OpNeq)
        , ("|",  OpOr)
        , ("&",  OpAnd) 
        , ("x|", OpXor)]

unops :: [(String, OpUn)]
unops
 =      [ ("!",  OpNot)
        , ("-",  OpNeg) ]       


binopprecs :: [(Int, [(String, OpBin)])]
binopprecs
 =      [ (1, [ ("|",  OpOr)
              , ("&",  OpAnd)
              , ("x|", OpXor)
              ]
          )
        , (2, [ ("<=", OpLeq)
              , (">=", OpGeq)
              , (">",  OpGt)
              , ("<",  OpLt)
              , ("==", OpEq)
              , ("!=", OpNeq)
              ]
          )
        , (3, [ ("+",  OpAdd)
              , ("-",  OpSub)
              ]
          )
        , (4, [ ("*",  OpMul)
              , ("/",  OpDiv)
              , ("%",  OpMod)
              ]
          )
        , (5, [ ("^", OpPow) 
              ]
          )
        ]

precOp :: Int -> Parser Token OpBin
precOp prec 
 = case lookup prec binopprecs of
    Just ops -> alts
                 [ do   only  (KOp str)
                        return op
                 | (str, op)  <- ops ]
    Nothing     -> zero

expr :: Parser Token Exp
expr = opL 1

-- This is a bit of a hack: prefer a more elegant way than manually
--   specifying that the precedence of atoms is maximal.
opL :: Int -> Parser Token Exp
opL 6
 = alts 
 [
   numExpr
 , parenExpr
 , unaryOp
 , appExpr -- function application must appear before single identifier
 , ouroExpr 
 , identExpr
 ]

opL prec
 =  do   l                   <- opL (prec + 1)
         es                  <- some (opR prec)
         return              $  case es of 
                                 []   -> l
                                 _    -> chainPartOps (reverse es) l
        
opR :: Int -> Parser Token Exp
opR prec
 = do  op                   <- precOp prec
       r                    <- opL (prec + 1)
       return               $  XOpBin op (XNum (-999)) r

-- single identifier
identExpr :: Parser Token Exp
identExpr = do  i        <- ident
                return   $  XId i

-- ouroboros operation
ouroExpr :: Parser Token Exp
ouroExpr = do   only Kouro
                arg_list <- exprArgs
                return   $ XCApp arg_list

-- function application
appExpr :: Parser Token Exp
appExpr = do   i        <- ident
               arg_list <- exprArgs
               return   $ XApp i arg_list

-- numeric literal
numExpr :: Parser Token Exp
numExpr = do  n   <- num
              return (XNum n)

-- parenthesised expression
parenExpr :: Parser Token Exp
parenExpr = do only KRoundBra
               e     <- expr
               only KRoundKet
               return e

-- prefix operator
unaryOp :: Parser Token Exp
unaryOp = do op      <- unoper
             e       <- opL 6
             return $ XOpUn op e

-- Take a string of binary operations and glue them together.
-- Each operation is substituted into the LHS of the one following.
-- The remaining LHS is filled with the expression passed in.  
chainPartOps :: [Exp] -> Exp -> Exp
chainPartOps [XOpBin op _ e2] l = (XOpBin op l e2) 
chainPartOps ((XOpBin op _ e2):rest) l = (XOpBin op (chainPartOps rest l) e2)
chainPartOps _ _ = (XNum (-999))
