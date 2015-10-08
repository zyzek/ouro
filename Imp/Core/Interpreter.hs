
module Imp.Core.Interpreter where
import Data.List.Utils
import Imp.Core.Exp
import Data.Maybe
import Data.List


-- | The execution environment, containing the current values of registers and variables.
data Env = Env { eReg :: [(Reg, Int)], eVar :: [(Id, Int)] } deriving Show

-- | A new empty environment.
newEnv :: Env
newEnv = Env [] []


-- | A structure passed into the step function, which contains:
-- |  An environment;
-- |  A list of functions defined in the program, for looking up a function call;
-- |  The current function being executed, used by ouroboroi;
-- |  The current block, for obtaining the next instruction to execute;
-- |  The register in which to store the return value of the current function call.
data StepArgs 
 = StepArgs { 
    sEnv :: Env,
    sFuncs :: [Function],
    sFunc :: Function,
    sBlock :: Block,
    sReg :: Reg
 } deriving Show


-- | Utilities for updating the fields of a StepArgs structure. 
setEnv :: StepArgs -> Env -> StepArgs
setEnv sArgs env = sArgs { sEnv = env }

setFunc :: StepArgs -> Function -> StepArgs
setFunc sArgs func = sArgs { sFunc = func }

setBlock :: StepArgs -> Block -> StepArgs
setBlock sArgs block = sArgs { sBlock = block }


-- | Function field extraction.
fId :: Function -> Id
fId (Function funcId _ _) = funcId

fIds :: Function -> [Id]
fIds (Function _ fArgIds _) = fArgIds

fBlocks :: Function -> [Block]
fBlocks (Function _ _ funcBlocks) = funcBlocks


-- | Block field extraction.
bId :: Block -> Int
bId (Block blkId _) = blkId

bInstrs :: Block -> [Instr]
bInstrs (Block _ blkInstrs) = blkInstrs


-- | Environment-manipulation tools!!
setReg :: Env -> Reg -> Int -> Env
setReg (Env reg var) targetReg val
 = Env (addToAL reg targetReg val) var

setVar :: Env -> Id -> Int -> Env
setVar (Env reg var) targetVar val
 = Env reg (addToAL var targetVar val)

getReg :: Env -> Reg -> Int
getReg (Env reg _) targetReg
 = fromJust $ lookup targetReg reg

getVar :: Env -> Id -> Int
getVar (Env _ var) targetId
 = fromJust $ lookup targetId var

copyReg :: Env -> Reg -> Reg -> Env
copyReg env dst src
 = setReg env dst (getReg env src)

loadVar :: Env -> Reg -> Id -> Env
loadVar env dst varId
 = setReg env dst (getVar env varId)

storeReg :: Env -> Id -> Reg -> Env
storeReg env varId src
 = setVar env varId (getReg env src)


-- | Given an integer, produce that many consecutive registers labeled from n to 1.
makeNRegs :: Int -> [Reg]
makeNRegs
 = unfoldr (\b -> if b == 0 then Nothing else Just (Reg b, b - 1))


-- | Call the main function of the given program with the given arguments. Return overall result.
startProgram :: Program -> [Int] -> IO Int
startProgram (Program funcList) args
 = do let func    = lookupFunc (Id "main") funcList
          regs    = makeNRegs (length args)
          argRegs = zip regs args
      env <- call (Env argRegs []) (Reg 0) funcList func regs
      return $ getReg env (Reg 0)


-- | Call a function. The args are as follows:
-- |  The input environment;
-- |  The register into which the called function should place the result;
-- |  The list of functions defined in the program;
-- |  The function to call;
-- |  The list of registers holding the function arguments.
-- | Return the environment with the result placed in the correct register.
call :: Env -> Reg -> [Function] -> Function -> [Reg] -> IO Env
call env gReg funcs func argRegs
 = do let argVals = map (getReg env) argRegs
      res <- evalFunc funcs func argVals gReg
      return $ setReg env gReg res


-- | Call a function with integer arguments and return the integer result.
evalFunc :: [Function] -> Function -> [Int] -> Reg -> IO Int
evalFunc funcs func@(Function _ argIds (block:_)) argVals gReg
 = do  let args =  zip argIds argVals
       res      <- step $ StepArgs (Env [] args) funcs func block gReg
       return   $  getReg res gReg

evalFunc _ _ _ _ = return 0
    

-- | Advance the program a single execution step, return the resulting environment.
--
-- | Retrieve the next instruction from the current block,
-- |  update the current environment by applying that instruction,
-- |  return the result of calling step on the updated environment, minus the run instruction.
-- |
-- | If the current block is empty, advance to the next block.
step :: StepArgs -> IO Env
step sArgs@(StepArgs _ _ func (Block blockId []) _)
 = step $ setBlock sArgs $ lookupBlock (blockId + 1) (fBlocks func)

step sArgs@(StepArgs env funcs func (Block blockId (i:is)) gReg)
 = do let newArgs = setBlock sArgs $ Block blockId is
      case i of
         IReturn rReg                   -> return $ copyReg env gReg rReg
         ICall dst funcId argsList      -> do newenv <- call env dst funcs 
                                                             (lookupFunc funcId funcs) argsList
                                              step $ setEnv newArgs $ newenv
                                            
         IConst dst val                 -> step $ setEnv newArgs $ setReg env dst val
         ILoad dst varId                -> step $ setEnv newArgs $ loadVar env dst varId
         IStore varId src               -> step $ setEnv newArgs $ storeReg env varId src
         IArith op dst op1 op2          -> step $ setEnv newArgs $ arithOp env op dst op1 op2
         IBranch regCond blk1Id blk2Id  -> 
             if getReg env regCond /= 0
              then step $ setBlock sArgs $ lookupBlock blk1Id $ fBlocks func
              else step $ setBlock sArgs $ lookupBlock blk2Id $ fBlocks func
         IPrint pRegs                   -> do putStrLn $ unwords $ map (show . getReg env) pRegs
                                              step newArgs


-- | Given a list of functions, return the one with the given Id.
lookupFunc :: Id -> [Function] -> Function
lookupFunc wantId funcs 
 = head $ filter (\(Function haveId _ _) -> wantId == haveId) funcs


-- | Given a list of blocks, return the one with the given Int as its id.
lookupBlock :: Int -> [Block] -> Block
lookupBlock wantId bs
 = head $ filter (\(Block haveId _) -> wantId == haveId) bs


-- | Apply an operator to some input.
arithCalc :: OpArith -> Int -> Int -> Int
arithCalc op op1 op2
 = case op of
        OpAdd   -> op1 + op2
        OpSub   -> op1 - op2
        OpMul   -> op1 * op2
        OpDiv   -> op1 `quot` op2
        OpMod   -> op1 `mod` op2
        OpPow   -> op1 ^ op2
        
        OpLt    -> if op1 < op2 then 1 else 0
        OpGt    -> if op1 > op2 then 1 else 0
        OpLeq   -> if op1 <= op2 then 1 else 0
        OpGeq   -> if op1 >= op2 then 1 else 0
        OpEq    -> if op1 == op2 then 1 else 0
        OpNeq   -> if op1 /= op2 then 1 else 0
        
        OpOr    -> if op1 /= 0 then op1 else op2
        OpAnd   -> if op1 /= 0 then op2 else 0
        OpXor   -> if (op1 /= 0) && (op2 == 0)
                    then op1
                    else if (op1 == 0) && (op2 /= 0)
                          then op2
                          else 0
        
        OpNot   -> if op1 /= 0 then 0 else 1
        OpNeg   -> -op1


-- | Apply an operator to the contents of two registers, store the result back in the environment.
arithOp :: Env -> OpArith -> Reg -> Reg -> Reg -> Env
arithOp env op dst op1Reg op2Reg
 = let op1 = getReg env op1Reg
       op2 = getReg env op2Reg
   in  setReg env dst $ arithCalc op op1 op2
