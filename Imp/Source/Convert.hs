
module Imp.Source.Convert where
import qualified Imp.Source.Exp         as S
import qualified Imp.Core.Exp           as C
import Data.List
import Data.Ord


 -- | Convert a program from the source to core languages.
convertProgram :: S.Program -> C.Program
convertProgram (S.Program funcs)
        = C.Program (map convertFunc funcs)

convertArgs :: (Int, Int, [S.Exp]) -> (Int, Int, [C.Instr], [C.Block], [C.Reg])
convertArgs (reg0, blk0, []) = (reg0, blk0, [], [], [])
convertArgs (reg0, blk0, (x:xs)) = 
    let (reg1, blk1, instrList1, blkList1) = convertExp(reg0, blk0, x)
        (reg2, blk2, instrList2, blkList2, keyRegs) = convertArgs(reg1, blk1, xs)
    in (reg2, blk2, instrList1 ++ instrList2, blkList1 ++ blkList2, [C.Reg (reg1 - 1)] ++ keyRegs)

 -- | - - - - - - - - - - - - -
 -- | 
 -- | Convert Expression
 -- | 
 -- | Converts a source expression into a list of core instructions.
 -- |
 -- | Takes the next available register id, block id, and a source expression.
 -- | Returns the next available register id, block id, and a list of core instructions.
 -- |
 -- | Assumptions that can be made:
 -- |    - If an expression returns a value, that value will be stored in the register 
 -- |       (n - 1) where n is the next available register returned by the convertExp 
 -- |       function.
 -- | 
 -- | Function is of structure:
 -- |   let { Evaluate internal expressions here. }
 -- |   in (
 -- |       {  
 -- |           Return the last register provided in internal expressions, 
 -- |           plus the number of registers consumed in the main expression.
 -- |       },
 -- |       {
 -- |           Return the last block provided in internal expressions,
 -- |           plus the number of blocks consumed in the main expression.
 -- |       },
 -- |       {
 -- |           Return the list of instructions that make up the full expression.
 -- |       }
 -- |   )
 -- |   where { Evaluate any sugaring here. }
 -- |
 -- | - - - - - - - - - - - - -
convertExp :: (Int, Int, S.Exp) -> (Int, Int, [C.Instr], [C.Block])
convertExp (reg0, blk0, (S.XNum val)) = 
    (
        (reg0 + 1),
        (blk0),
        [
            C.IConst (C.Reg reg0) val
        ],
        []
    )
convertExp (reg0, blk0, S.XId varId) = 
    (
        (reg0 + 1),
        (blk0),
        [
            C.ILoad (C.Reg reg0) (convertId varId)
        ],
        []
    )
convertExp (reg0, blk0, S.XApp varId argVals) = 
    let (reg1, blk1, instrList, blkList, regArgs) = convertArgs (reg0, blk0, argVals)
    in (
        (reg1 + 1),
        (blk1),
        -- Append the list of load instructions to the call instruction.
        instrList ++ [C.ICall (C.Reg (reg1)) (convertId varId) regArgs],
        blkList
    )
convertExp (reg0, blk0, (S.XOpBin op exp1 exp2)) = 
    -- Evaluate the two expressions passed into XOpBin
    let (reg1, blk1, instrList1, blkList1) = convertExp (reg0, blk0, exp1)
        (reg2, blk2, instrList2, blkList2) = convertExp (reg1, blk1, exp2)
    in (
        (reg2 + 1),
        (blk2),
        -- Join the two instruction lists and add the arithmetic instruction
        instrList1 ++ instrList2 ++ [
            C.IArith (convertOpBin op) (C.Reg reg2) (C.Reg (reg1 - 1)) (C.Reg (reg2 - 1))
        ],
        blkList1 ++ blkList2
    )
convertExp (reg0, blk0, (S.XOpUn op expr)) =
    -- Evaluate the expression operated on by XOpUn
    let (reg, blk, instrList, blkList) = convertExp (reg0, blk0, expr)
    in (
        (reg + 1),
        (blk),
        instrList ++ [C.IArith (convertOpUn op) (C.Reg reg) (C.Reg (reg - 1)) (C.Reg (reg - 1))],
        blkList
    )
convertExp (reg0, blk0, (S.XTernary cond expr1 expr2)) =
    let (reg1, blk1, instrList, blkList) = convertStmt (reg0, blk0, S.SIfElse cond (S.Block [S.SExp expr1]) (S.Block [S.SExp expr2]))
    in (
        (reg1),
        (blk1),
        instrList,
        blkList
    )
convertExp (reg0, blk0, stmt) =
    let (reg1, blk1, instrList, _) = 
            case stmt of 
                S.XAssign  varId expr        -> convertStmt (reg0, blk0, S.SAssign varId expr)
                S.XFAssign varId funcId expr -> convertStmt (reg0, blk0, S.SFAssign varId funcId expr)
                S.XBAssign varId op     expr -> convertStmt (reg0, blk0, S.SBAssign varId op expr)
                _                            -> convertStmt (reg0, blk0, S.SExp (S.XNum 0))
    in (
        (reg1),
        (blk1),
        instrList,
        []
    )

 -- | - - - - - - - - - - - - -
 -- | 
 -- | Convert Statement
 -- | 
 -- | Converts a source statement into a list of core instructions and core blocks.
 -- |
 -- | Takes the next available register id, block id, and a source statement.
 -- | Returns the next available register id, block id, and a list of core instructions
 -- |   and core blocks.
 -- |
 -- | Assumptions that can be made:
 -- |    - The next block in chronological order after the block that is being "worked on"
 -- |       is the same number that is being returned from the function.
 -- |    - If an expression returns a value, that value will be stored in the register 
 -- |       (n - 1) where n is the next available register returned by the convertExp 
 -- |       function.
 -- | 
 -- | Function is of structure:
 -- |   let { Evaluate internal expressions and internal blocks here. }
 -- |   in (
 -- |       {  
 -- |           Return the last register provided in internal expressions and internal blocks, 
 -- |           plus the number of registers consumed in the main expression.
 -- |       },
 -- |       {
 -- |           Return the last block provided in internal expressions,
 -- |           plus the number of blocks consumed in the main expression.
 -- |       },
 -- |       {
 -- |           Return the list of instructions that make up the main expression.
 -- |       },
 -- |       {
 -- |           Return the list of blocks that make up the main expression.
 -- |       }
 -- |   )
 -- |   where { Evaluate any sugaring here. }
 -- |
 -- | - - - - - - - - - - - - -
convertStmt :: (Int, Int, S.Stmt) -> (Int, Int, [C.Instr], [C.Block])
convertStmt (reg0, blk0, (S.SAssign varId varExp)) =
    let (reg1, blk1, instrList, blkList) = convertExp (reg0, blk0, varExp)
    in (
        (reg1),
        (blk1),
        -- Append the list of instructions generated by the expression to the store instruction.
        instrList ++ [C.IStore (convertId varId) (C.Reg (reg1 - 1))],
        blkList
    )
convertStmt (reg0, blk0, (S.SFAssign varId funcId expr)) =
    let (reg1, blk1, instrList, blkList) = convertExp (reg0, blk0, S.XApp funcId [expr, (S.XId varId)])
    in (
        (reg1),
        (blk1),
        -- Append the list of instructions generated by the expression to the store instruction.
        instrList ++ [C.IStore (convertId varId) (C.Reg (reg1 - 1))],
        blkList
    )
convertStmt (reg0, blk0, (S.SBAssign varId op expr)) =
    let (reg1, blk1, instrList, blkList) = convertExp (reg0, blk0, S.XOpBin op (S.XId varId) expr)
    in (
        (reg1),
        (blk1),
        -- Append the list of instructions generated by the expression to the store instruction.
        instrList ++ [C.IStore (convertId varId) (C.Reg (reg1 - 1))],
        blkList
    )
convertStmt (reg0, blk0, (S.SIf expr blk)) = 
    let (reg1, blk1, condList, blkList) = convertExp (reg0, blk0, expr)
        (reg2, blk2, cBlkList) = convertBlock (reg1 + 1, blk1 + 1, blk)
    in (
        reg2,
        blk2 + 1,
        condList ++ [
            C.IBranch (C.Reg (reg1 - 1)) (blk1 + 1) (blk2 + 1)
        ],
        mergeBlocks
        (
            let blk1Blk = find (\(C.Block bId _) -> bId == blk2) cBlkList
                (C.Block _ ls) = last cBlkList
            in 
                -- Append an unconditional branch to the last block in the block list 
                --  that has been generated by the function, if and only if the block
                --  does not end in a branch or return.
                blkList ++ cBlkList ++ 
                case blk1Blk of
                  Nothing ->  [
                                (C.Block (blk2) 
                                [
                                    C.IConst (C.Reg reg2) 0,
                                    C.IBranch (C.Reg reg2) (blk2 + 1) (blk2 + 1)
                                ])
                              ]
                  _ ->
                      case (last ls) of 
                          (C.IBranch _ _ _)   ->  []
                          (C.IReturn _)       ->  []
                          _                   ->  [
                                  (C.Block (blk2) 
                                  [
                                      C.IConst (C.Reg reg2) 0,
                                      C.IBranch (C.Reg reg2) (blk2 + 1) (blk2 + 1)
                                  ])
                              ]
        )
    )
convertStmt (reg0, blk0, (S.SIfElse expr blkl blkr)) = 
    let (reg1, blk1, condList, blkList) = convertExp (reg0, blk0, expr)
        (reg2, blk2, cBlkListl) = convertBlock (reg1 + 1, blk1 + 1, blkl)
        (reg3, blk3, cBlkListr) = convertBlock (reg2 + 1, blk2 + 1, blkr)
    in (
        reg3,
        blk3 + 1,
        condList ++ [
            C.IBranch (C.Reg (reg1 - 1)) (blk1 + 1) (blk2 + 1)
        ],
        mergeBlocks 
        (
            -- Append an unconditional branch to the last block in each of the block lists 
            --  that have been generated by the function, if and only if the blocks
            --  do not end in a branch or return.
            let blk1Blk = find (\(C.Block bId _) -> bId == blk2) cBlkListl
                blk2Blk = find (\(C.Block bId _) -> bId == blk3) cBlkListr
                (C.Block _ lsl) = last cBlkListl
                (C.Block _ lsr) = last cBlkListr
            in 
                blkList ++ cBlkListl ++ cBlkListr ++ 
                [C.Block blk2 [C.ICopy (C.Reg (reg3 - 1)) (C.Reg (reg2 - 2))]]
                 ++ [C.Block blk3 [C.ICopy (C.Reg (reg3 - 1)) (C.Reg (reg3 - 2))]] ++
                case blk1Blk of
                  Nothing ->  [
                                C.Block (blk2) [
                                  C.IConst (C.Reg reg2) 0,
                                  C.IBranch (C.Reg reg2) (blk3 + 1) (blk3 + 1)
                              ]]
                  _ -> case (last lsl) of 
                        (C.IBranch _ _ _)   ->  []
                        (C.IReturn _)       ->  []
                        _                   ->  [
                            C.Block (blk2) [
                                C.IConst (C.Reg reg2) 0,
                                C.IBranch (C.Reg reg2) (blk3 + 1) (blk3 + 1)
                            ]]
                 ++ 
                case blk2Blk of
                  Nothing ->  [
                            C.Block blk3 [
                              C.IConst (C.Reg reg3) 0,
                              C.IBranch (C.Reg reg3) (blk3 + 1) (blk3 + 1)
                            ]]
                  _ -> case (last lsr) of 
                          (C.IBranch _ _ _)   ->  []
                          (C.IReturn _)       ->  []
                          _                   ->  [
                              C.Block blk3 [
                                  C.IConst (C.Reg reg3) 0,
                                  C.IBranch (C.Reg reg3) (blk3 + 1) (blk3 + 1)
                              ]]
        )
    )
convertStmt (reg0, blk0, (S.SReturn expr)) = 
    let (reg1, blk1, condList, blkList) = convertExp (reg0, blk0, expr)
    in (
        reg1, 
        blk1, 
        condList ++ [C.IReturn (C.Reg (reg1 - 1))],
        blkList
    )
convertStmt (reg0, blk0, (S.SWhile expr blk)) = 
    let (reg1, blk1, cBlkList) = convertBlock (reg0 + 1, blk0 + 1, S.Block [(S.SIf expr blk)])
        -- These three lines are inconsistent with the function pattern, I'm noob plz 4giff
        (C.Block blkId lstBlkInstrs) = last cBlkList
        truncLstBlk = take ((length lstBlkInstrs) - 2) lstBlkInstrs
        newBlkList = (take ((length cBlkList) - 1) cBlkList) ++ [(C.Block blkId truncLstBlk)]
        -- /End inconsistancy
    in (
        reg1 + 1,
        blk1,
        [
            C.IConst (C.Reg reg0) 0,
            C.IBranch (C.Reg reg0) (blk0 + 1) (blk0 + 1)
        ],
        mergeBlocks (
                newBlkList ++ [C.Block (blk1 - 1) [
                    C.IConst (C.Reg reg1) 0,
                    C.IBranch (C.Reg reg1) (blk0 + 1) (blk0 + 1)
            ]]
        ) 
    )
convertStmt (reg, blk, (S.SPrint expr)) =
  let (reg1, blk1, condList, blkList) = convertExp (reg, blk, expr)
  in (
      reg1,
      blk1,
      condList ++ [
          C.IPrint (C.Reg (reg1 - 1))
      ],
      blkList
     )
convertStmt (reg, blk, (S.SExp expr)) =
  let (reg1, blk1, condList, blkList) = convertExp (reg, blk, expr)
  in (
      reg1 + 1,
      blk1,
      condList,
      blkList
     )


-- | Convert a list of source statements to a list of core block properties and core blocks.
convertStmts :: (Int, Int, [S.Stmt]) -> (Int, Int, [(Int, [C.Instr])], [C.Block])
convertStmts (reg0, blk0, []) = (reg0, blk0, [], [])
convertStmts (reg0, blk0, (stmt:stmts)) = 
    let (reg1, blk1, instrList1, blks1) = convertStmt (reg0, blk0, stmt)
        (reg2, blk2, blkInstrList1, blks2) = convertStmts (reg1, blk1, stmts)
    in (reg2, blk2, ((blk0, instrList1) : blkInstrList1), blks1 ++ blks2)

-- | Convert a source block to list of core blocks.
convertBlock :: (Int, Int, S.Block) -> (Int, Int, [C.Block])
convertBlock (reg0, blk0, (S.Block [])) = (reg0, blk0, [])
convertBlock (reg0, blk0, (S.Block sStmts)) = 
    let (reg1, blk1, blkInstrList, blks) = convertStmts (reg0, blk0, sStmts)
        newBlocks = (map (\(blk, instrs) -> C.Block blk instrs) (mergeIBlocks blkInstrList)) ++ blks
        sorted = sortBy (comparing (\(C.Block blkId _) -> blkId)) newBlocks
    in (reg1, blk1, sorted)
    
-- | Merge core block properties that have the same ids together
mergeIBlocks :: [(Int, [C.Instr])] -> [(Int, [C.Instr])]
mergeIBlocks blkInstrs = 
    let a = (\y -> filter (\(blk, _) -> blk == y) blkInstrs)
        maxId = maximumBy (comparing (\(blkId, _) -> blkId)) blkInstrs
        blkInstrList = unfoldr (\b -> if (b < 0) then Nothing else Just ((a b), b - 1)) (fst maxId)
        filteredList = filter (\x -> not (null x)) blkInstrList
    in map (\ls@(l:_) -> ((fst (l)), (foldr (++) [] (map snd ls)))) filteredList

-- | Merge core blocks that have the same block ids together.
mergeBlocks :: [C.Block] -> [C.Block]
mergeBlocks [] = []
mergeBlocks blkInstrs = 
    let a = (\y -> filter (\(C.Block blk _) -> blk == y) blkInstrs)
        (C.Block maxId _) = maximumBy (comparing (\(C.Block blkId _) -> blkId)) blkInstrs
        blkInstrList = unfoldr (\b -> if (b < 0) then Nothing else Just ((a b), b - 1)) maxId
        filteredList = filter (\x -> not (null x)) blkInstrList
    in sortBy (comparing (\(C.Block bId _) -> bId)) (map (\ls@((C.Block bId _):_)
       -> (C.Block bId (foldr (++) [] (map (\(C.Block _ bInstrs) -> bInstrs) ls)))
      ) filteredList)

-- | Convert a source function to a core function.
convertFunc :: S.Function -> C.Function
convertFunc (S.Function fId fArgIds fVarIds fBlk) =
    let (_, _, fBlks) = convertBlock (1, 1, fBlk)
        newBlk = (C.Block 0 ((C.IConst (C.Reg 0) 0)
         : (map (\x -> C.IStore (convertId x) (C.Reg 0)) fVarIds)
         ++ [C.IBranch (C.Reg 0) 1 1]))
    in (C.Function (convertId fId) (map convertId fArgIds) (mergeBlocks ([newBlk] ++ fBlks)))

-- | Convert a source identifier to a core identifier.
convertId :: S.Id -> C.Id
convertId (S.Id str) = C.Id str

-- | Convert a source operation to a core operation.
convertOpBin :: S.OpBin -> C.OpArith
convertOpBin S.OpAdd = C.OpAdd
convertOpBin S.OpSub = C.OpSub
convertOpBin S.OpMul = C.OpMul
convertOpBin S.OpDiv = C.OpDiv
convertOpBin S.OpLt = C.OpLt
convertOpBin S.OpGt = C.OpGt
convertOpBin S.OpEq = C.OpEq
convertOpBin S.OpNeq = C.OpNeq
convertOpBin S.OpGeq = C.OpGeq
convertOpBin S.OpLeq = C.OpLeq
convertOpBin S.OpPow = C.OpPow
convertOpBin S.OpAnd = C.OpAnd
convertOpBin S.OpOr = C.OpOr
convertOpBin S.OpXor = C.OpXor
convertOpBin S.OpMod = C.OpMod

convertOpUn :: S.OpUn -> C.OpArith
convertOpUn S.OpNot = C.OpNot
convertOpUn S.OpNeg = C.OpNeg

