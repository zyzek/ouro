
module Imp.Source.Parser where
import Imp.Source.Exp
import Imp.Source.Tokens
import Imp.Parsec


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
            b          <- block i
            return     $ Function i arg_list var_list b


args :: Parser Token [Id]
args
 = do   only KRoundBra
        arg_list       <- (alt idents (result []))
        only KRoundKet
        return arg_list

exprArgs :: Id -> Parser Token [Exp]
exprArgs curFuncId
 = do   only KRoundBra
        arg_list       <- (alt (exprs curFuncId) (result []))
        only KRoundKet
        return arg_list


vars :: Parser Token [Id]
vars
 = alt  (do  only Kvars
             v_list    <- idents
             return v_list)
        (result [])

block :: Id -> Parser Token Block
block curFuncId
 = alts
 [ 
   do   only KBraceBra
        stmt_list      <- (some (stmt curFuncId))
        only KBraceKet
        return         $ Block stmt_list
 , do   only KBraceBra
        stmt_list      <- (some (stmt curFuncId))
        e              <- expr curFuncId

        only KBraceKet
        return         $ Block (stmt_list ++ [SReturn e])
 ]


stmt :: Id -> Parser Token Stmt
stmt curFuncId
 = alts 
 [ -- assignment
   do  i          <- ident
       only KEquals
       e          <- expr curFuncId
       only KSemi
       return     $ SAssign i e
 
   -- multiple assignment
 , do  is         <- idents
       only KEquals
       e          <- exprs curFuncId
       only KSemi
       return     $  SPolyAssign is e

   -- function assignment
 , do  i          <- ident
       only KRoundBra
       f          <- ident
       only KRoundKet
       only KEquals
       e          <- expr curFuncId
       only KSemi
       return     $ SFAssign i f e

   -- operator assignment
 , do  i          <- ident
       only KRoundBra
       f          <- binoper
       only KRoundKet
       only KEquals
       e          <- expr curFuncId
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
       guard      <- expr curFuncId
       only Kthen
       lbr        <- block curFuncId
       only Kelse
       rbr        <- block curFuncId
       return     $ SIfElse guard lbr rbr

   -- if-then
 , do  only Kif
       guard      <- expr curFuncId
       only Kthen
       br         <- block curFuncId
       return     $ SIf guard br

  -- return
 , do  only Kreturn
       i <- expr curFuncId
       only KSemi
       return $ SReturn i

  -- while
  , do only Kwhile
       cond       <- expr curFuncId
       body       <- block curFuncId
       return     $  SWhile cond body

  -- print
  , do only Kprint
       e          <- expr curFuncId
       only KSemi
       return     $  SPrint e

  -- naked expression
  , do   e        <- expr curFuncId
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
exprs :: Id -> Parser Token [Exp]
exprs curFuncId
 = alts
 [ do   i       <- expr curFuncId
        only KComma
        is      <- exprs curFuncId
        return  $ i : is

 , do   i       <- expr curFuncId
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

expr :: Id -> Parser Token Exp
expr = opL 1

-- This is a bit of a hack: prefer a more elegant way than manually
--   specifying that the precedence of atoms is maximal.
opL :: Int -> Id -> Parser Token Exp
opL 6 curFuncId
 = alts 
 [
   numExpr
 , unaryOp curFuncId
 , appExpr curFuncId -- function application must appear before single identifier
 , assignExpr curFuncId
 , fAssignExpr curFuncId
 , bAssignExpr curFuncId
 , ouroExpr curFuncId
 , ternaryExpr curFuncId
 , identExpr
 , parenExpr curFuncId
 ]

opL prec curFuncId
 =  do   l                   <- opL (prec + 1) curFuncId
         es                  <- some (opR prec curFuncId)
         return              $  case es of 
                                 []   -> l
                                 _    -> chainPartOps (reverse es) l
        
opR :: Int -> Id -> Parser Token Exp
opR prec curFuncId
 = do  op                   <- precOp prec
       r                    <- opL (prec + 1) curFuncId
       return               $  XOpBin op (XNum (-999)) r

-- Take a string of binary operations and glue them together.
-- Each operation is substituted into the LHS of the one following.
-- The remaining LHS is filled with the expression passed in.  
chainPartOps :: [Exp] -> Exp -> Exp
chainPartOps [XOpBin op _ e2] l = (XOpBin op l e2) 
chainPartOps ((XOpBin op _ e2):rest) l = (XOpBin op (chainPartOps rest l) e2)
chainPartOps _ _ = (XNum (-999))


-- single identifier
identExpr :: Parser Token Exp
identExpr = do  i        <- ident
                return   $  XId i

-- ouroboros operation
ouroExpr :: Id -> Parser Token Exp
ouroExpr curFuncId = do   only KAt
                          arg_list <- exprArgs curFuncId
                          return   $ XApp curFuncId arg_list

-- function application
appExpr :: Id -> Parser Token Exp
appExpr curFuncId = do   i        <- ident
                         arg_list <- exprArgs curFuncId
                         return   $ XApp i arg_list

-- numeric literal
numExpr :: Parser Token Exp
numExpr = do  n   <- num
              return $ XNum n

-- parenthesised expression
parenExpr :: Id -> Parser Token Exp
parenExpr curFuncId = do only KRoundBra
                         e     <- expr curFuncId
                         only KRoundKet
                         return e

-- prefix operator
unaryOp :: Id -> Parser Token Exp
unaryOp curFuncId = do op      <- unoper
                       e       <- opL 6 curFuncId
                       return $ XOpUn op e

-- Assign expression
assignExpr :: Id -> Parser Token Exp
assignExpr curFuncId
 = do   only KRoundBra
        i        <- ident
        only KEquals
        e          <- expr curFuncId
        only KRoundKet
        return     $ XAssign i e

-- FAssign expression
fAssignExpr :: Id -> Parser Token Exp
fAssignExpr curFuncId
 = do   only KRoundBra
        i          <- ident
        only KRoundBra
        f          <- ident
        only KRoundKet
        only KEquals
        e          <- expr curFuncId
        only KRoundKet
        return     $ XFAssign i f e

-- BAssign expression
bAssignExpr :: Id -> Parser Token Exp
bAssignExpr curFuncId
 = do   only KRoundBra
        i          <- ident
        only KRoundBra
        o          <- binoper
        only KRoundKet
        only KEquals
        e          <- expr curFuncId
        only KRoundKet
        return     $ XBAssign i o e

-- ternary operation
ternaryExpr :: Id -> Parser Token Exp
ternaryExpr curFuncId
 = do   only KQMark
        only KRoundBra
        c          <- expr curFuncId
        only KComma
        e1         <- expr curFuncId
        only KComma
        e2         <- expr curFuncId
        only KRoundKet
        return   $ XTernary c e1 e2
