
module Imp.Source.Convert where
import qualified Imp.Source.Exp         as S
import qualified Imp.Core.Exp           as C
import qualified Imp.Parsec
import Data.List
import qualified Data.Map.Strict        as Map


-- | Convert a program from the source to core languages.
convertProgram :: S.Program -> C.Program
convertProgram (S.Program funcs)
	= C.Program (convertFuncs funcs []) 


-- | Convert a source identifier to a core identifier.
convertId :: S.Id -> C.Id
convertId (S.Id str) = C.Id str

-- | Convert a list of source functions to list of core functions
convertFuncs :: [S.Function] -> [(C.Id, Reg)] -> [C.Function]
convertFuncs [] _
  = []

convertFuncs (f : fs) regMap 
  = (convertFunc f regMap) : (convertFuncs fs regMap)

convertFunc :: S.Function -> C.Function
convertFunc (S.Function sId sArgs sVars sBlocks) regMap
	= 	bind (convertId sId) $ \cId
	->	bind (foreach sArgs convertId) $ \cArgs
	-> 	bind () $ \_
	->	bind (convertBlocks sBlocks 1 []) $ \cBlocks
	-> 	C.Function cId cArgs cBlocks

--convertBlocks :: [S.Block] -> [C.Block] -> [C.Block]
--convertBlocks sBlocks convertedBlocks
--  = case convertBlocks of
--      []  -> 
--        case sBlocks of convertedBlocks
--          []        -> convertedBlocks
--          sb : rest -> 
--      b:bs  ->

convertBlocks :: [S.Block] -> Int -> [(C.Id, Reg)] -> [C.Block]
convertBlock [] _
  = []

convertBlocks (s : ss) blockNo regMap
  = (convertBlock s blockNo regMap) : (convertBlocks ss (blockNo + 1) regMap)

convertBlock :: S.Block -> Int -> [(C.Id, Reg)] -> C.Block
convertBlock (S.Block stmts) blockNo regMap
  = C.Block blockNo (foreach stmts convertStmt)

convertStmts :: [Stmt] -> Int -> (Int, [Instr])
convertStmts [] lastReg
  = (lastReg, [])
convertStmts (s : ss) lastReg
  = bind (convertStmt s lastReg) $ \(stmtReg1, stmtInst1)
  -> bind (convertStmts ss stmtReg1) $ \(stmtReg2, stmtInst2)
  -> (stmtReg2, stmtInst1 ++ stmtInst2)

-- | Convert source statement  to core instruction set
convertStmt :: Stmt -> Int -> (Int, [Instr])
convertStmt stmt lastReg
	= case stmt of
    -- assignment statment
		SAssign sId exp 
      -- id conversiont
      -> bind (convertId sId) $ \cId
      -- expression conversion
      -> bind (convertExp exp) $ \(expReg, expInst)
      -- store instruction
      -> (expReg, [(IStore cId expReg)])
        XNum i    -> 
        Xid xid   -> ILoad (lookup id)

  	SIf id b
      -> 
  	SIfElse id b1 b2
      -> 
    -- return statement requires load and return instructions
  	SReturn id
      -> bind (lastReg + 1) $ \newReg
      -> (lastReg, [(ILoad (Reg newReg) id), (IReturn newReg)])

-- | Convert source expression into instruction list
-- | Also returns last used register id for tracking
convertExp :: S.Exp -> Int -> (Int, [Instr])
convertExp exp lastReg
  = case exp of
      -- integer yields load constant instruction
      XNum i   
        -> bind (lastReg + 1) $ \newReg
        -> (newReg, [lc (Reg newReg) i])
      -- variable id yields load instruction instruction
      XId id      
        -> bind (lastReg + 1) $ \newReg  
        -> (newReg, [ld (Reg newReg) id])
      XApp fId args 
        -> 
      -- operator statement requires recursive expression evaluation
      XOp op e1 e2  
        -- convert e1 and e2 to yield instruction sets and intermediate registers
        -> bind (convertExp e1 lastReg) $ \(intReg1, inst1)
        -> bind (convertExp e2 intReg1) $ \(intReg2, inst2)
        -> bind (intReg2 + 1) $ \intRegOp
        -- concat inst1, inst2 and final operator instruction
        -> (intRegOp, inst1 ++ inst2 ++ (op (Reg intRegOp) intReg1 intReg2) )


