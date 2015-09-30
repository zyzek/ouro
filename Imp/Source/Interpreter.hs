module Imp.Source.Interpreter where
import Data.List.Utils
import Imp.Core.Exp
import Data.Maybe
import Data.List
import Debug.Trace


data Env = Env { eReg :: [(Reg, Int)], eVar :: [(Id, Int)] } deriving Show
newEnv :: Env
newEnv = Env [] []

data StepArgs = StepArgs { 
    sEnv :: Env,
    sFuncs :: [Function],
    sFunc :: Function,
    sBlock :: Block,
    sReg :: Reg
} deriving Show

setEnv :: StepArgs -> Env -> StepArgs
setEnv sArgs env = sArgs { sEnv = env }
setFunc :: StepArgs -> Function -> StepArgs
setFunc sArgs func = sArgs { sFunc = func }
setBlock :: StepArgs -> Block -> StepArgs
setBlock sArgs block = sArgs { sBlock = block }


 -- Tools!!
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


makeNRegs :: Int -> [Reg]
makeNRegs n
 = unfoldr (\b -> if b == 0 then Nothing else Just (Reg b, b - 1)) n

 
startProgram :: Program -> [Int] -> Int
startProgram (Program funcList) args = 
    let func = lookupFunc (Id "main") funcList
        regs = makeNRegs (length args)
        argRegs = zip regs args
        env = call (Env argRegs []) (Reg 0) funcList func regs
    in getReg env (Reg 0)


call :: Env -> Reg -> [Function] -> Function -> [Reg] -> Env
call env gReg funcs func argRegs =
    let argVals = map (\x -> getReg env x) argRegs
    in setReg env gReg (evalFunc funcs func (argVals) gReg)


evalFunc :: [Function] -> Function -> [Int] -> Reg -> Int
evalFunc funcs func@(Function _ argIds (block:_)) argVals gReg = 
    let args = zip argIds argVals
    in getReg (step (StepArgs (Env [] args) funcs func block gReg)) gReg
evalFunc _ _ _ _ = 0
    


step :: StepArgs -> Env
step sArgs@(StepArgs _ _ func (Block blockId []) _) = 
    step $ setBlock sArgs $ lookupBlock (blockId + 1) (fBlocks func)
step sArgs@(StepArgs env funcs func (Block blockId (i:is)) gReg) = 
    let newArgs = setBlock sArgs $ Block blockId is
    in case i of
        IReturn rReg                    -> copyReg env gReg rReg
        ICall dst funcId argsList       -> step $ setEnv newArgs $ 
                                            call env dst funcs (lookupFunc funcId funcs) argsList
        IConst dst val                  -> step $ setEnv newArgs $ setReg env dst val
        ILoad dst varId                 -> step $ setEnv newArgs $ loadVar env dst varId
        IStore varId src                -> step $ setEnv newArgs $ storeReg env varId src
        IArith op dst op1 op2           -> step $ setEnv newArgs $ arithOp env op dst op1 op2
        IBranch regCond blk1Id blk2Id   -> 
            if (getReg env regCond /= 0)
             then step $ setBlock sArgs $ lookupBlock blk1Id $ fBlocks func
             else step $ setBlock sArgs $ lookupBlock blk2Id $ fBlocks func
        IPrint pReg                     -> traceShow (getReg env pReg) (step newArgs)

 
lookupFunc :: Id -> [Function] -> Function
lookupFunc wantId funcs 
 = head $ filter (\(Function haveId _ _) -> wantId == haveId) funcs


lookupBlock :: Int -> [Block] -> Block
lookupBlock wantId bs
 = head $ filter (\(Block haveId _) -> wantId == haveId) bs

arithCalc :: OpArith -> Int -> Int -> Int
arithCalc op op1 op2 = case op of
            OpAdd   -> op1 + op2
            OpSub   -> op1 - op2
            OpMul   -> op1 * op2
            OpDiv   -> quot op1 op2
            OpMod   -> mod op1 op2
            OpPow   -> op1 ^ op2
            OpLt    -> if (op1 < op2) then 1 else 0
            OpGt    -> if (op1 > op2) then 1 else 0
            OpLeq   -> if (op1 <= op2) then 1 else 0
            OpGeq   -> if (op1 >= op2) then 1 else 0
            OpEq    -> if (op1 == op2) then 1 else 0
            OpNeq   -> if (op1 /= op2) then 1 else 0
            OpOr    -> if (op1 /= 0) then op1 else op2
            OpAnd   -> if (op1 /= 0) then op2 else 0
            OpXor   -> if ((op1 /= 0) && (op2 == 0))
                        then op1
                        else 
                         if ((op1 == 0) && (op2 /= 0))
                          then op2
                          else 0
            OpNot   -> if (op1 /= 0) then 0 else 1
            OpNeg   -> -op1

arithOp :: Env -> OpArith -> Reg -> Reg -> Reg -> Env
arithOp env op dst op1Reg op2Reg = 
    let op1 = getReg env op1Reg
        op2 = getReg env op2Reg
    in setReg env dst $ arithCalc op op1 op2

