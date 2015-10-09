
module Imp.Core.Interpreter where
import Data.List.Utils
import Imp.Core.Exp
import Data.List
import System.Exit


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

getReg :: Env -> Reg -> IO Int
getReg (Env reg _) targetReg
 = do  let res = lookup targetReg reg
       maybe (do  putStrLn $ "Tried to look up fictitious register " ++ show targetReg ++ "."
                  exitFailure)
             return res

getVar :: Env -> Id -> IO Int
getVar (Env _ var) targetId
 = do  let res = lookup targetId var
       maybe (do  putStrLn $ "Tried to look up fictitious variable \"" ++ strOfId targetId ++ "\"."
                  exitFailure)
             return res

copyReg :: Env -> Reg -> Reg -> IO Env
copyReg env dst src
 = do   res     <- getReg env src
        return  $  setReg env dst res

loadVar :: Env -> Reg -> Id -> IO Env
loadVar env dst varId
 = do   res     <- getVar env varId
        return  $  setReg env dst res

storeReg :: Env -> Id -> Reg -> IO Env
storeReg env varId src
 = do   res     <- getReg env src
        return  $  setVar env varId res


-- | Given an integer, produce that many consecutive registers labeled from n to 1.
makeNRegs :: Int -> [Reg]
makeNRegs
 = unfoldr (\b -> if b == 0 then Nothing else Just (Reg b, b - 1))


-- | Call the main function of the given program with the given arguments. Return overall result.
startProgram :: Program -> [Int] -> IO Int
startProgram (Program funcList) args
 = do func        <- lookupFunc (Id "main") funcList
      let regs    =  makeNRegs (length args)
          argRegs =  zip regs args
      
      if length args < nargs func
       then do putStrLn ("Insufficient input; main takes " ++ show (nargs func) ++ " argument(s).")
               exitFailure
       else do env <- call (Env argRegs []) (Reg 0) funcList func regs
               getReg env (Reg 0)
   where nargs (Function _ funcargs  _) = length funcargs


-- | Call a function. The args are as follows:
-- |  The input environment;
-- |  The register into which the called function should place the result;
-- |  The list of functions defined in the program;
-- |  The function to call;
-- |  The list of registers holding the function arguments.
-- | Return the environment with the result placed in the correct register.
call :: Env -> Reg -> [Function] -> Function -> [Reg] -> IO Env
call env gReg funcs func argRegs
 = do argVals <- mapM (getReg env) argRegs
      res     <- evalFunc funcs func argVals gReg
      return $ setReg env gReg res


-- | Call a function with integer arguments and return the integer result.
evalFunc :: [Function] -> Function -> [Int] -> Reg -> IO Int
evalFunc funcs func@(Function i argIds (block:_)) argVals gReg
 = do  let args =  zip argIds argVals
       if length argIds /= length argVals
         then do  putStrLn ("Provided " ++ show (length argVals)
                            ++ " argument(s) to function \"" ++ strOfId i ++ "\", which takes "
                            ++ show (length argIds) ++ "!")
                  exitFailure
         else do  res      <- step $ StepArgs (Env [] args) funcs func block gReg
                  getReg res gReg

evalFunc _ _ _ _ = return 0
    

-- | Advance the program a single execution step, return the resulting environment.
-- |
-- | Retrieve the next instruction from the current block,
-- |  update the current environment by applying that instruction,
-- |  return the result of calling step on the updated environment, minus the run instruction.
-- |
-- | If the current block is empty, advance to the next block.
step :: StepArgs -> IO Env
step sArgs@(StepArgs _ _ func (Block blockId []) _)
 = do  blk  <- lookupBlock (blockId + 1) (fBlocks func)
       step $  setBlock sArgs blk

step sArgs@(StepArgs env funcs func (Block blockId (i:is)) gReg)
 = do let newArgs = setBlock sArgs $ Block blockId is
      case i of
         IReturn rReg                   ->     copyReg env gReg rReg

         ICall dst funcId argsList      -> do  cfunc  <- lookupFunc funcId funcs 
                                               newenv <- call env dst funcs cfunc argsList
                                               step   $  setEnv newArgs newenv
                                            
         IConst dst val                 ->     step   $  setEnv newArgs $ setReg env dst val

         ILoad dst varId                -> do  ldVar  <- loadVar env dst varId
                                               step   $  setEnv newArgs ldVar

         IStore varId src               -> do  stReg  <- storeReg env varId src
                                               step   $  setEnv newArgs stReg

         IArith op dst op1 op2          -> do  newenv <- arithOp env op dst op1 op2
                                               step   $  setEnv newArgs newenv

         IBranch regCond blk1Id blk2Id  -> do  cond   <- getReg env regCond
                                               if cond /= 0
                                                then do  blk1 <- lookupBlock blk1Id $ fBlocks func
                                                         step $  setBlock sArgs blk1
                                                else do  blk2 <- lookupBlock blk2Id $ fBlocks func
                                                         step $  setBlock sArgs blk2

         IPrint pRegs                   -> do  ns <- mapM (getReg env) pRegs
                                               putStrLn $ unwords $ map show ns
                                               step newArgs


-- | Given a list of functions, return the one with the given Id.
lookupFunc :: Id -> [Function] -> IO Function
lookupFunc wantId funcs 
 = let res = filter (\(Function haveId _ _) -> wantId == haveId) funcs
   in if null res
       then do putStrLn ("No function with the id \"" ++ strOfId wantId ++ "\" exists.")
               exitFailure
       else return $ head res


-- | Given a list of blocks, return the one with the given Int as its id.
lookupBlock :: Int -> [Block] -> IO Block
lookupBlock wantId bs
 = let res = filter (\(Block haveId _) -> wantId == haveId) bs
   in if null res
       then do putStrLn ("No block with the id " ++ show wantId ++ " exists in this function.")
               exitFailure
       else return $ head res


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
        OpXor   |  (op1 /= 0) && (op2 == 0) -> op1
                |  (op1 == 0) && (op2 /= 0) -> op2
                |  otherwise                -> 0
        
        OpNot   -> if op1 /= 0 then 0 else 1
        OpNeg   -> -op1


-- | Apply an operator to the contents of two registers, store the result back in the environment.
arithOp :: Env -> OpArith -> Reg -> Reg -> Reg -> IO Env
arithOp env op dst op1Reg op2Reg
 = do  op1    <- getReg env op1Reg
       op2    <- getReg env op2Reg
       return $ setReg env dst $ arithCalc op op1 op2

