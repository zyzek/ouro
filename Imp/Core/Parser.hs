
module Imp.Core.Parser where
import Imp.Parsec
import Imp.Core.Exp
import Imp.Core.Tokens


-- | Whole program; everything at the top level is a function definition.
program :: Parser Token Program
program 
 = do   only KRoundBra
        funcs      <- some function
        only KRoundKet
        return     $  Program funcs

-- | Single function definition: 
-- | keyword, function name, list of arguments, list of variables, then the function body itself.
function :: Parser Token Function
function
 = do   only KRoundBra 
        i          <- ident
        args       <- idArgs        
        blks       <- some block
        only KRoundKet
        return     $  Function i args blks

-- | Source block: a sequence of statements between curly braces.
block :: Parser Token Block
block
 = do   only KRoundBra
        n          <- num
        instrs     <- some instr
        only KRoundKet
        return         $  Block n instrs

-- | Statements themselves contain expressions.
instr :: Parser Token Instr
instr
 = do  only KRoundBra
       i  <- alts  [ constInstr
                   , loadInstr
                   , storeInstr
                   , operInstr
                   , branchInstr
                   , returnInstr
                   , callInstr
                   , printInstr
                   ]
       only KRoundKet
       return i

-- || Component Parsers ==================================================

constInstr :: Parser Token Instr
constInstr
 = do  only KConst
       r    <- register
       i    <- num
       return $ IConst r i


loadInstr :: Parser Token Instr
loadInstr
 = do  only KLoad
       r      <- register
       i      <- ident
       return $ ILoad r i

storeInstr :: Parser Token Instr
storeInstr
 = do  only KStore
       i    <- ident
       r    <- register
       return $ IStore i r

branchInstr :: Parser Token Instr
branchInstr
 = do  only KBranch
       r    <- register
       i1   <- num
       i2   <- num
       return $ IBranch r i1 i2

callInstr :: Parser Token Instr
callInstr
 = do  only KCall
       r    <- register
       i    <- ident 
       args <- some register
       return $ ICall r i args

returnInstr :: Parser Token Instr
returnInstr
 = do  only KReturn
       r <- register
       return $ IReturn r

printInstr :: Parser Token Instr
printInstr
 = do  only KPrint
       regs <- some register
       return $ IPrint regs
       

operInstr :: Parser Token Instr
operInstr
 = do    o  <- alts [ do   only  (KInstr str)
                           return inst
                     | (str, inst)  <-  opers ]
         r1 <- register
         r2 <- register
         r3 <- register
         return $ IArith o r1 r2 r3

opers :: [(String, OpArith)]
opers = [
          ("add",   OpAdd)
        , ("sub",   OpSub)
        , ("mul",   OpMul)
        , ("div",   OpDiv)
        , ("mod",   OpMod)
        , ("pow",   OpPow) 
        , ("lt",     OpLt) 
        , ("gt",     OpGt) 
        , ("eq",     OpEq) 
        , ("neq",   OpNeq)
        , ("geq",   OpGeq)
        , ("leq",   OpLeq)
        , ("or",     OpOr)
        , ("and",   OpAnd)
        , ("xor",   OpXor)
        , ("not",   OpNot)
        , ("neg",   OpNeg)
        ]




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


-- | Parse a number.
num   :: Parser Token Int
num = from takeNum


-- | Parse an identifier.
ident :: Parser Token Id
ident
 = do   i       <- from takeId
        return  $  Id i


register :: Parser Token Reg
register
 = do   n       <- from takeReg
        return  $  Reg n
