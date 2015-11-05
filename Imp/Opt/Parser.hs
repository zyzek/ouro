
module Imp.Opt.Parser where
import Imp.Parsec
import Imp.Opt.CFG
import Imp.Opt.CFGUtils
import Imp.Core.Tokens
import Imp.Core.Parser hiding (block)

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
        args       <-  some ident
        only KRoundKet
        blks       <- some block
        only KRoundKet
        let brBlks = map blockBranches blks
        return     $  branchUnterminated $ setCFGAddrs $ CFG i args brBlks $ regenBlkEdges brBlks


block :: Parser Token Block
block
 = do   only KRoundBra
        n          <- num
        instrs     <- some instr
        only KRoundKet
        let instrnodes = map (\i -> InstrNode i (InstrAddr (-1) (-1)) [] []) instrs
        return         $  Block n instrnodes (InstrDets [] []) (InstrDets [] []) []

