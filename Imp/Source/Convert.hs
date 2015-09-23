
module Imp.Source.Convert where
import Control.Monad
import qualified Imp.Source.Exp         as S
import qualified Imp.Core.Exp           as C
--import Data.List
--import qualified Data.Map.Strict        as Map


-- | Convert a program from the source to core languages.
convertProgram :: S.Program -> C.Program
convertProgram (S.Program funcs)
	= C.Program (convertFuncs funcs) 


-- | Convert a source identifier to a core identifier.
convertId :: S.Id -> C.Id
convertId (S.Id str) = C.Id str

-- | Convert a list of source functions to list of core functions
convertFuncs :: [S.Function] -> [C.Function]
convertFuncs sFuncs
  =   map convertFunc sFuncs

convertFunc :: S.Function -> C.Function
convertFunc (S.Function sId sArgs sVars sBlock)
	= C.Function cId cArgs cBlocks
    where   cId     = (convertId sId)
            cArgs   = (map convertId sArgs)
            (_, _, cBlocks) = (convertBlock sBlock 1 0)
    --do  cId     <- convertId sId
    --      cArgs   <- forEach sArgs convertId
    --      cBlocks <- convertBlocks sBlocks 1 0 []
    --      C.Function cId cArgs cBlocks

--convertBlocks :: S.Block -> Int -> Int -> [C.Block]
--convertBlocks [] _
--  = []
--convertBlocks (b : bs) rNum bNum
--  = b1 ++ (convertBlocks bs rNum1 bNum1) 
--    where (rNum1, bNum1, b1) = convertBlock b rNum bNum


convertBlock :: S.Block -> Int -> Int -> (Int, Int, [C.Block])
convertBlock (S.Block stmts) rNum bNum
  =   let (rNum1, inst, bNum1, sBlocks) = convertStmts stmts rNum (bNum + 1)
      in  (rNum1, bNum1, (C.Block bNum inst) : sBlocks)


convertStmts :: [S.Stmt] -> Int -> Int-> (Int, [C.Instr], Int, [C.Block])
convertStmts [] rNum bNum
  = (rNum, [], bNum, [])

convertStmts (s : ss) rNum bNum
  =   let (rNum1, inst1, bNum1, b1) = convertStmt s rNum bNum
          (rNum2, inst2, bNum2, b2) = convertStmts ss rNum1 bNum1
      in  (rNum2, inst1 ++ inst2, bNum2, b1 ++ b2)

-- | Convert source statement  to core instruction set
-- | Function yields last used register, list of instructions
-- | May also yield new blocks; thus returns last used block num and list of blocks
convertStmt :: S.Stmt -> Int -> Int-> (Int, [C.Instr], Int, [C.Block])
convertStmt stmt rNum bNum
	= case stmt of
      --return statement requires load and return instructions
      S.SReturn id ->
        let   cId = convertId id
              (rNum1, loadInst) = instLoad rNum cId
              (rNum2, retInst) = instRet rNum
        in    (rNum2, [loadInst, retInst], bNum, [])
      -- assignment statment
      S.SAssign sId e ->  
        let   cId = convertId sId 
              (rNum1, eInst) = convertExp e rNum
              (rNum2, storeInst) = instStore cId (rNum1 - 1)
        in    (rNum2, (eInst ++ [storeInst]), bNum, []) 


-- TODO; not sure how deal with if statements
    --S.SIf id b     
   --   -> 
      S.SIfElse id sb1 sb2 ->
        let   cId = convertId id
              (rNum1, ldInst) = instLoad rNum cId
              (rNum2, brInst) = instBranch (rNum1 - 1)  bNum bNum1
              (rNum3, bNum1, cb1) = convertBlock sb1 rNum2 bNum
              (rNum4, bNum2, cb2) = convertBlock sb2 rNum3 bNum1
        in    (rNum4, [ldInst, brInst], bNum2, cb1 ++ cb2)



-- | Convert source expression into instruction list
-- | Also returns last used register id for tracking
convertExp :: S.Exp -> Int -> (Int, [C.Instr])
convertExp e rNum
  = case e of
      -- integer yields load constant instruction
      S.XNum i ->
        let   (rNum1, lcInst) = instConst rNum i
        in    (rNum1, [lcInst] )
      -- variable id yields load instruction instruction
      S.XId id ->
        let   cId = convertId id
              (rNum1, ldInst) = instLoad rNum cId
        in    (rNum1, [ldInst] )
      S.XApp fId args ->
        let   cfId = convertId fId
              cArgs = map convertId args
              (rNum1, ldInst, argRegs) = instLoadMany rNum cArgs 
              (rNum2, callInst) = instCall rNum1 cfId argRegs
        in    (rNum2, ldInst ++ [callInst])

--        -> (0,[])
      -- operator statement requires recursive expression evaluation
      S.XOp sOp e1 e2 ->
        -- convert e1 and e2 to yield instruction sets and intermediate registers
        let cOp = convertOp sOp
            (rNum1, inst1) = convertExp e1 rNum
            (rNum2, inst2) = convertExp e2 rNum1
            (rNum3, opInst) = instOp cOp rNum2 (rNum1 - 1) (rNum2 - 1)
        -- concat inst1, inst2 and final operator instruction
        in  (rNum3, inst1 ++ inst2 ++ [opInst])

convertOp :: S.Op -> C.OpArith
convertOp op
  =   case op of
        S.OpAdd   -> C.OpAdd
        S.OpSub   -> C.OpSub
        S.OpMul   -> C.OpMul
        S.OpDiv   -> C.OpDiv
        S.OpLt    -> C.OpLt
        S.OpGt    -> C.OpGt
        S.OpEq    -> C.OpEq

instConst :: Int -> Int -> (Int, C.Instr)
instConst rNum n = (rNum + 1, C.IConst (C.Reg rNum) n )

instLoad :: Int -> C.Id -> (Int, C.Instr)
instLoad rNum id = (rNum + 1, C.ILoad (C.Reg rNum) id )

instLoadMany :: Int -> [C.Id] -> (Int, [C.Instr], [C.Reg])
instLoadMany rNum [] = (rNum, [], [])
instLoadMany rNum (v : vs)
  =   let   (rNum1, ldInst1) = instLoad rNum v
            (rNum2, instList, argRegs) = instLoadMany rNum1 vs
      in    (rNum2, [ldInst1] ++ instList, (C.Reg rNum) : argRegs)

instStore :: C.Id -> Int -> (Int, C.Instr)
instStore id rNum = (rNum + 1, C.IStore id (C.Reg rNum) )

instOp :: C.OpArith -> Int -> Int -> Int -> (Int, C.Instr)
instOp op rNumRet rNum1 rNum2 
  = (rNumRet + 1, C.IArith op (C.Reg rNumRet) (C.Reg rNum1) (C.Reg rNum2) )

instBranch :: Int -> Int -> Int -> (Int, C.Instr)
instBranch rNum b1 b2 
  = (rNum + 1, C.IBranch (C.Reg rNum) b1 b2 )

instRet :: Int -> (Int, C.Instr)
instRet rNum = (rNum + 1, C.IReturn (C.Reg rNum) )

instCall :: Int -> C.Id -> [C.Reg] -> (Int, C.Instr)
instCall rNum fId args 
  = (rNum + 1, C.ICall (C.Reg rNum) fId args)