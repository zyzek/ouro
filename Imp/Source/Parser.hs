
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
       guard      <- ident
       only Kthen
       lbr        <- block
       only Kelse
       rbr        <- block
       return     $ SIfElse guard lbr rbr

   -- if-then
 , do  only Kif
       guard      <- ident
       only Kthen
       br         <- block
       return     $ SIf guard br

  -- return
 , do  only Kreturn
       i <- ident
       only KSemi
       return $ SReturn i
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

        -- operation
 , do   only KRoundBra
        e1       <- expr
        op       <- oper
        e2       <- expr
        only KRoundKet
        return   $ XOp op e1 e2
 ]


-- | Parse a number.
num   :: Parser Token Int
num = from takeNum


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
oper :: Parser Token Op
oper
 = alts
 [ do   only    (KOp str)
        return  op
 | (str, op)    <- ops]


-- | Operator names.
ops :: [(String, Op)]
ops
 =      [ ("+",  OpAdd)
        , ("-",  OpSub)
        , ("*",  OpMul)
        , ("/",  OpDiv)
        , ("<",  OpLt)
        , (">",  OpGt)
        , ("==", OpEq) ]       

