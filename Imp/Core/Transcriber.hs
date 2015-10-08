module Imp.Core.Transcriber where
import Imp.Core.Exp
import Data.List


-- | The transcriber converts IR code represented as Haskell data into an output string.


-- | Produce IR program code.
progString :: Program -> String
progString (Program funcs)
 = "(\n" ++ intercalate "\n" (map (funcString "  ") funcs) ++ "\n)" 


-- | The string representation of an IR function.
funcString :: String -> Function -> String
funcString indent (Function i vars blocks)
 = indent ++ 
   "(" ++ strOfId i
       ++ " (" ++ unwords (map strOfId vars) ++ ")\n"
       ++ intercalate "\n" (map (blockString (indent ++ "  ")) blocks)
       ++ "\n" ++ indent ++ ")"


-- | The string representation of an IR Block.
blockString :: String -> Block -> String
blockString indent (Block i instrs)
 = indent ++ 
   "(" ++ show i ++ "\n"
       ++ intercalate "\n" (map (instrString (indent ++ "   ")) instrs)
       ++ " )"


-- | The string representation of an IR instruction.
instrString :: String -> Instr -> String
instrString indent instr
 = indent ++ 
   "(" 
    ++ case instr of
            IConst r n          -> "lc " ++ regString r ++ " " ++ show n
            ILoad  r i          -> "ld " ++ regString r ++ " " ++ strOfId i
            IStore i r          -> "st " ++ strOfId i ++ " " ++ regString r
            IArith o d s1 s2    -> operString o ++ " " ++ regString d ++ " " ++ regString s1 
                                                                      ++ " " ++ regString s2
            IBranch r m n       -> "br " ++ regString r ++ " " ++ show m ++ " " ++ show n
            IReturn r           -> "ret " ++ regString r
            ICall r i args      -> "call " ++ regString r ++ " " ++ strOfId i ++ " "
                                           ++ unwords (map regString args)
            IPrint regs         -> "print " ++ unwords (map regString regs)
    ++ ")"
        

-- | A map from the internal to external names of arithmetic operators.
operString :: OpArith -> String
operString o
 = case o of
        OpAdd -> "add"
        OpSub -> "sub"
        OpMul -> "mul"
        OpDiv -> "div"
        OpMod -> "mod"
        OpPow -> "pow"
        OpLt  -> "lt"
        OpGt  -> "gt"
        OpEq  -> "eq"
        OpNeq -> "neq"
        OpGeq -> "geq"
        OpLeq -> "leq"
        OpOr  -> "or"
        OpAnd -> "and"
        OpXor -> "xor"
        OpNot -> "not"
        OpNeg -> "neg"


-- | Register string representation.
regString :: Reg -> String
regString (Reg n) = "r" ++ show n 

