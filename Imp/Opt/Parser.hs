
module Imp.Opt.Parser where
import Imp.Parsec
import Imp.Opt.Exp
import Imp.Core.Tokens
import Data.List

progCFGs :: Parser Token [CFG]
progCFGs 
 = do   only KRoundBra
        cfgs       <- some funcCFG
        only KRoundKet
        return cfgs


funcCFG :: Parser Token CFG
funcCFG
 = do   only KRoundBra 
        i          <- ident
        only KRoundBra
        some ident
        only KRoundKet
        blks       <- some block
        only KRoundKet
        return     $  setInstrAddrs $ CFG i blks $ concatMap blockBranches blks

-- | Code block: a sequence of instructions with a particular id: a constituent of a function.
block :: Parser Token Block
block
 = do   only KRoundBra
        n          <- num
        instrs     <- some instr
        only KRoundKet
        let instrnodes = map (\i -> InstrNode i (-1, -1) [] []) instrs
        return         $  Block n instrnodes


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



-- | CFG and instruction graph functions.

blockBranches :: Block -> [Edge]
blockBranches (Block o instrnodes)
 = map (Edge o) $ rmDups $ concatMap bDests instrnodes
 where bDests i
        = case i of 
               InstrNode (IBranch _ j k) _ _ _ -> [j, k]
               _             -> []
       rmDups
        = map head . group . sort


setInstrAddrs :: CFG -> CFG
setInstrAddrs (CFG i blks edges)
 = CFG i (map setBlockInstrAddrs blks) edges


setBlockInstrAddrs :: Block -> Block
setBlockInstrAddrs (Block i instrnodes)
 = let aPairs
        = zip (take (length instrnodes) [0,1..]) instrnodes
       setAddr addr (InstrNode inst _ ins outs)
        = InstrNode inst addr ins outs
   in Block i (map (\(n, instrN) -> setAddr (i, n) instrN) aPairs)




