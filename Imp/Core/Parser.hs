
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


-- | Single function definition. 
-- | name, arguments, block list.
function :: Parser Token Function
function
 = do   only KRoundBra 
        i          <- ident
        only KRoundBra
        args       <- some ident
        only KRoundKet
        blks       <- some block
        only KRoundKet
        return     $  Function i args blks

-- | Code block: a sequence of instructions with a particular id: a constituent of a function.
block :: Parser Token Block
block
 = do   only KRoundBra
        n          <- num
        instrs     <- some instr
        only KRoundKet
        return         $  Block n instrs


-- | A machine instruction.
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

-- | Number -> Register load instruction.
constInstr :: Parser Token Instr
constInstr
 = do  only KConst
       r    <- register
       i    <- num
       return $ IConst r i


-- | Var -> Reg load instruction.
loadInstr :: Parser Token Instr
loadInstr
 = do  only KLoad
       r      <- register
       i      <- ident
       return $ ILoad r i


-- | Reg -> Var store instruction.
storeInstr :: Parser Token Instr
storeInstr
 = do  only KStore
       i    <- ident
       r    <- register
       return $ IStore i r


-- | Branch instruction.
branchInstr :: Parser Token Instr
branchInstr
 = do  only KBranch
       r    <- register
       i1   <- num
       i2   <- num
       return $ IBranch r i1 i2


-- | Function call instruction.
callInstr :: Parser Token Instr
callInstr
 = do  only KCall
       r    <- register
       i    <- ident 
       args <- some register
       return $ ICall r i args


-- | Return instruction.
returnInstr :: Parser Token Instr
returnInstr
 = do  only KReturn
       r    <- register
       return $ IReturn r


-- | Print instruction.
printInstr :: Parser Token Instr
printInstr
 = do  only KPrint
       regs <- some register
       return $ IPrint regs
       

-- | Arithmetic operation instruction.
operInstr :: Parser Token Instr
operInstr
 = do    o  <- alts [ do   only  (KInstr str)
                           return inst
                     | (str, inst)  <-  opers ]
         r1 <- register
         r2 <- register
         r3 <- register
         return $ IArith o r1 r2 r3


-- | Arithmetic instructions available.
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


-- | Parse a number.
num   :: Parser Token Int
num = from takeNum


-- | Parse an identifier.
ident :: Parser Token Id
ident
 = do   i       <- from takeId
        return  $  Id i


-- | Parse a register.
register :: Parser Token Reg
register
 = do   n       <- from takeReg
        return  $  Reg n
