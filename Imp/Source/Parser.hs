
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

  , do only Kwhile
       cond       <- expr
       body       <- block
       return $ SWhile cond body
 ]


-- | Parse an expression.
expr  :: Parser Token Exp
expr
 = alts 
 [      -- number
   do   n        <- num
        return   $ XNum n

        -- function application
        -- Must appear before single identifier: see above
 , do   i        <- ident
        arg_list <- args
        return   $ XApp i arg_list

        -- single identifier
 , do   i        <- ident
        return   $ XId i

        -- binary operation
 , do   only KRoundBra
        e1       <- expr
        op       <- binoper
        e2       <- expr
        only KRoundKet
        return   $  XOpBin op e1 e2
      
        -- unary operation
 , do   op       <- unoper
        e        <- expr
        return   $  XOpUn op e

 , do   only KRoundBra
        e        <- expr
        only KRoundKet
        return   $  e
 ]


--compL :: Parser Token Exp
--compL  
-- = do    l       <- addL
--         r       <- some compR
--         return SOMETHING

--compR :: Parser Token Exp
--comparative 
-- = do    op      <- compOp
--         r       <- addL
--         return SOMETHING


--addL :: Parser Token Exp
--addL
-- = do    l       <- multL
--         r       <- some addR
--         return SOMETHING

--addR :: Parser Token Exp
--addR 
-- = do    op      <- addOp
--         r       <- multL
--         return SOMETHING


--multL :: Parser Token Exp
--multL 
-- = do    l       <- opBase
--         r       <- some multR
--         return SOMETHING

--multR :: Parser Token Exp
--multR
-- = do    op      <- multOp
--         r       <- opBase
--         return SOMETHING


--opBase :: Parser Token Exp
--opBase = alt num expr         


--chainOp :: Op -> [Exp] -> Maybe Exp
--chainOp _ [] = Nothing
--chainOp op [e] = Just e
--chainOp op e1:rest =  op e1 $ fromJust $ chainOp op rest


-- | Parse a number.
num   :: Parser Token Int
num = from takeNum

--numExpr :: Parser Token Exp
--numExpr = do  n   <- num
--              return (XNum n)



-- | Parse an identifier.
ident :: Parser Token Id
ident
 = do   i       <- from takeId
        return  $ Id i


-- | Parse arguments separated by commas.
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


--addOp :: Parser Token Op
--addOp = alt (only (KOp "+")) (only (KOp "-"))

--multOp :: Parser Token Op
--multOp = alt (only (KOp "*")) (only (KOp "/"))

--compOp :: Parser Token Op
--compOp = alts [ only (KOp "<")
--              , only (KOp ">")
--              , only (KOp "==")
--              , only (KOp "!=") ]

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

