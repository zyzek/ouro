
module Imp.Source.Convert where
import qualified Imp.Source.Exp         as S
import qualified Imp.Core.Exp           as C
import Data.List
import Data.Ord


 -- | Convert a program from the source to core languages.
convertProgram :: S.Program -> C.Program
convertProgram (S.Program funcs)
        = C.Program (map convertFunc funcs)

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
convertExp :: (Int, Int, S.Exp) -> (Int, Int, [C.Instr])
convertExp (reg0, blk0, (S.XNum val)) = 
    (
        (reg0 + 1),
        (blk0),
        [
            C.IConst (C.Reg reg0) val
        ]
    )
convertExp (reg0, blk0, S.XId varId) = 
    (
        (reg0 + 1),
        (blk0),
        [
            C.ILoad (C.Reg reg0) (convertId varId)
        ]
    )
convertExp (reg0, blk0, S.XApp varId argIds) = 
    (
        (reg0 + (length argIds) + 1),
        (blk0),
        -- Append the list of load instructions to the call instruction.
        loadInstrs ++ [C.ICall (C.Reg (reg0 + (length argIds))) (convertId varId) regArgs]
    )
    -- Create a numbered list
    where regIdArgs = unfoldr (\b -> 
            if (b == (length argIds))
             then Nothing
             else Just (reg0 + b, b + 1)) 0
    -- Convert the numbered list to a list of registers
          regArgs = map (\x -> C.Reg x) regIdArgs
    -- Zip the registers with the argument Ids
          idsAndArgs = zip regArgs argIds
    -- Create the list of load instructions
          loadInstrs = map (\(reg, varId2) -> C.ILoad reg (convertId varId2)) idsAndArgs

convertExp (reg0, blk0, (S.XOp op exp1 exp2)) = 
    -- Evaluate the two expressions passed into XOp
    let (reg1, blk1, instrList1) = convertExp (reg0, blk0, exp1)
        (reg2, blk2, instrList2) = convertExp (reg1, blk1, exp2)
    in (
        (reg2 + 1),
        (blk2),
        -- Join the two instruction lists and add the arithmatic instruction
        instrList1 ++ instrList2 ++ [
            C.IArith (convertOp op) (C.Reg reg2) (C.Reg (reg1 - 1)) (C.Reg (reg2 - 1))
        ]
    )

 -- | - - - - - - - - - - - - -
 -- | 
 -- | Convert Expression
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
    let (reg1, blk1, instrList) = convertExp (reg0, blk0, varExp)
    in (
        (reg1),
        (blk1),
        -- Append the list of instructions generated by the expression to the store instruction.
        instrList ++ [C.IStore (convertId varId) (C.Reg (reg1 - 1))],
        []
    )
convertStmt (reg0, blk0, (S.SIf expr blk)) = 
    let (reg1, blk1, condList) = convertExp (reg0, blk0, expr)
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
                cBlkList ++ 
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
    let (reg1, blk1, condList) = convertExp (reg0, blk0, expr)
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
                cBlkListl ++ cBlkListr ++
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
    let (reg1, blk1, condList) = convertExp (reg0, blk0, expr)
    in (
        reg1, 
        blk1, 
        condList ++ [
            C.IReturn (C.Reg (reg1 - 1))
        ],
        []
    )
convertStmt (reg0, blk0, (S.SWhile expr blk)) = 
    let (reg1, blk1, cBlkList) = convertBlock (reg0 + 1, blk0 + 1, S.Block [(S.SIf expr blk)])
        -- These three lines are inconsistant with the function pattern, I'm noob plz 4giff
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
mergeBlocks blkInstrs = 
    let a = (\y -> filter (\(C.Block blk _) -> blk == y) blkInstrs)
        (C.Block maxId _) = maximumBy (comparing (\(C.Block blkId _) -> blkId)) blkInstrs
        blkInstrList = unfoldr (\b -> if (b < 0) then Nothing else Just ((a b), b - 1)) maxId
        filteredList = filter (\x -> not (null x)) blkInstrList
    in map (\ls@((C.Block bId _):_)
       -> (C.Block bId (foldr (++) [] (map (\(C.Block _ bInstrs) -> bInstrs) ls)))
      ) filteredList

-- | Convert a source function to a core function.
convertFunc :: S.Function -> C.Function
convertFunc (S.Function fId fArgIds fVarIds fBlk) =
    let (_, _, fBlks) = convertBlock (1, 1, fBlk)
        newBlk = (C.Block 0 ((C.IConst (C.Reg 0) 0)
         : (map (\x -> C.IStore (convertId x) (C.Reg 0)) fVarIds)
         ++ [C.IBranch (C.Reg 0) 1 1]))
    in (C.Function (convertId fId) (map convertId fArgIds) (newBlk:fBlks))

-- | Convert a source identifier to a core identifier.
convertId :: S.Id -> C.Id
convertId (S.Id str) = C.Id str

-- | Convert a source operation to a core operation.
convertOp :: S.Op -> C.OpArith
convertOp S.OpAdd = C.OpAdd
convertOp S.OpSub = C.OpSub
convertOp S.OpMul = C.OpMul
convertOp S.OpDiv = C.OpDiv
convertOp S.OpLt = C.OpLt
convertOp S.OpGt = C.OpGt
convertOp S.OpEq = C.OpEq
convertOp S.OpNeq = C.OpNeq
convertOp S.OpPow = C.OpPow
convertOp S.OpAnd = C.OpAnd
convertOp S.OpOr = C.OpOr
convertOp S.OpMod = C.OpMod
