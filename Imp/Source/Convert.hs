
module Imp.Source.Convert where
import qualified Imp.Source.Exp         as S
import qualified Imp.Core.Exp           as C
--import Data.List
--import qualified Data.Map.Strict        as Map


-- | Convert a program from the source to core languages.
convertProgram :: S.Program -> C.Program
convertProgram (S.Program funcs)
	= C.Program (map convertFunc funcs) 

-- | Convert source function to core function
convertFunc :: S.Function -> C.Function
convertFunc (S.Function sId sArgs sVars sBlock) -- not sure what to do with vars
  = C.Function cId cArgs cBlocks
          -- convert function id
    where cId     = (convertId sId)
          -- convert arg list
          cArgs   = (map convertId sArgs)
          -- convert block; starting reg 1, block 0
          (_, _, cBlocks) = (convertBlock sBlock 1 0)

-- | Convert source block to list of core blocks (stmts may yield blocks)
convertBlock :: S.Block -> Int -> Int -> (Int, Int, [C.Block])
convertBlock (S.Block stmts) rNum bNum
  =   let (rNum1, inst, bNum1, sBlocks) = convertStmts stmts rNum (bNum + 1)
      in  (rNum1, bNum1, (C.Block bNum inst) : sBlocks)

-- | Convert a list of source stmts to core inst list and block list
-- | args: stmt list, next free reg num, next free block num
-- | returns: next free reg num, inst list
-- | next free block num, converted block list
convertStmts :: [S.Stmt] -> Int -> Int-> (Int, [C.Instr], Int, [C.Block])
convertStmts [] rNum bNum
  = (rNum, [], bNum, [])
convertStmts (s : ss) rNum bNum
          -- base call
  =   let (rNum1, inst1, bNum1, b1) = convertStmt s rNum bNum
          -- recursive call
          (rNum2, inst2, bNum2, b2) = convertStmts ss rNum1 bNum1
      in  (rNum2, inst1 ++ inst2, bNum2, b1 ++ b2)

-- | Convert source stmt to core inst list and block list
-- | args: stmt, next free reg, next free block num
-- | returns: next free reg, inst list,
-- | next free block num, converted block list
convertStmt :: S.Stmt -> Int -> Int-> (Int, [C.Instr], Int, [C.Block])
convertStmt stmt rNum bNum
  = case stmt of
      --return statement
      S.SReturn sId ->
              -- convert return id
        let   cId = convertId sId
              -- load inst
              ( _ , loadInst) = instLoad rNum cId
               -- ret inst
              (rNum2, retInst) = instRet rNum
        in    (rNum2, [loadInst, retInst], bNum, [])
      -- assignment statment
      S.SAssign sId e ->  
              -- convert assignee id
        let   cId = convertId sId 
              (rNum1, eInst) = convertExp e rNum
              -- prepare store inst
              (rNum2, storeInst) = instStore cId (rNum1 - 1)
        in    (rNum2, (eInst ++ [storeInst]), bNum, []) 
      -- if statement
      S.SIf sId sb1 ->
              -- convert condition id
        let   cId = convertId sId 
              -- load inst
              (rNum1, ldInst) = instLoad rNum cId
              -- branch inst
              (rNum2, brInst) = instBranch (rNum1 - 1) bNum bNum1
              -- convert block to exec if condition satisfied
              (rNum3, bNum1, cb1) = convertBlock sb1 rNum2 bNum
              -- create empty second block for if condition unsatisfied
              (rNum4, bNum2, cb2) = (rNum3, bNum1 + 1, [(C.Block bNum1 [])] )
        in    (rNum4, [ldInst, brInst], bNum2, cb1 ++ cb2)
      -- if else statement
      S.SIfElse sId sb1 sb2 ->
              -- convert id
        let   cId = convertId sId
              -- load inst
              (rNum1, ldInst) = instLoad rNum cId
              -- branch inst
              (rNum2, brInst) = instBranch (rNum1 - 1)  bNum bNum1
              -- convert blocks to execute based on condition
              (rNum3, bNum1, cb1) = convertBlock sb1 rNum2 bNum
              (rNum4, bNum2, cb2) = convertBlock sb2 rNum3 bNum1
        in    (rNum4, [ldInst, brInst], bNum2, cb1 ++ cb2)

-- | Convert source expression into instruction list
-- | args: source exp, next free reg
-- | returns: next free register, inst list
convertExp :: S.Exp -> Int -> (Int, [C.Instr])
convertExp e rNum
  = case e of
      -- constant exp
      S.XNum i ->
              -- load const inst
        let   (rNum1, lcInst) = instConst rNum i
        in    (rNum1, [lcInst] )
      -- id exp
      S.XId id ->
        let   cId = convertId id
              -- create load inst
              (rNum1, ldInst) = instLoad rNum cId
        in    (rNum1, [ldInst] )
      -- application exp
      S.XApp fId args ->
        let   cfId = convertId fId
              -- convert arg id's
              cArgs = map convertId args
              -- load inst for all args
              (rNum1, ldInst, argRegs) = instLoadMany rNum cArgs 
              -- call inst
              (rNum2, callInst) = instCall rNum1 cfId argRegs
        in    (rNum2, ldInst ++ [callInst])
      -- operator exp
      S.XOp sOp e1 e2 ->
        let cOp = convertOp sOp
            -- convert e1 and e2 to yield instruction sets and intermediate registers
            (rNum1, inst1) = convertExp e1 rNum
            (rNum2, inst2) = convertExp e2 rNum1
            -- create operator inst
            (rNum3, opInst) = instOp cOp rNum2 (rNum1 - 1) (rNum2 - 1)
            -- concat inst1, inst2 and final operator instruction
        in  (rNum3, inst1 ++ inst2 ++ [opInst])

-- | Convert source operator to core operator
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

-- | Convert a source identifier to a core identifier.
convertId :: S.Id -> C.Id
convertId (S.Id str) = C.Id str

-- | Create load const inst
instConst :: Int -> Int -> (Int, C.Instr)
instConst rNum n = (rNum + 1, C.IConst (C.Reg rNum) n )

-- | Create load inst from var id
instLoad :: Int -> C.Id -> (Int, C.Instr)
instLoad rNum id = (rNum + 1, C.ILoad (C.Reg rNum) id )

-- | Create list of load inst's from var id list
instLoadMany :: Int -> [C.Id] -> (Int, [C.Instr], [C.Reg])
instLoadMany rNum [] = (rNum, [], [])
instLoadMany rNum (v : vs)
            -- base call
  =   let   (rNum1, ldInst1) = instLoad rNum v
            -- recursive call
            (rNum2, instList, argRegs) = instLoadMany rNum1 vs
      in    (rNum2, [ldInst1] ++ instList, (C.Reg rNum) : argRegs)

-- | Create store inst from var id
instStore :: C.Id -> Int -> (Int, C.Instr)
instStore id rNum = (rNum + 1, C.IStore id (C.Reg rNum) )

-- | Create op inst 
instOp :: C.OpArith -> Int -> Int -> Int -> (Int, C.Instr)
instOp op rNumRet rNum1 rNum2 
  = (rNumRet + 1, C.IArith op (C.Reg rNumRet) (C.Reg rNum1) (C.Reg rNum2) )

-- | Create branch inst
instBranch :: Int -> Int -> Int -> (Int, C.Instr)
instBranch rNum b1 b2 
  = (rNum + 1, C.IBranch (C.Reg rNum) b1 b2 )

-- | Create return statement
instRet :: Int -> (Int, C.Instr)
instRet rNum = (rNum + 1, C.IReturn (C.Reg rNum) )

-- | Create call inst
instCall :: Int -> C.Id -> [C.Reg] -> (Int, C.Instr)
instCall rNum fId args 
  = (rNum + 1, C.ICall (C.Reg rNum) fId args)