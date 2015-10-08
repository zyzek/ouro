module Imp.Core.Transcriber where
import Imp.Core.Exp
import Data.List

progString :: Program -> String
progString (Program funcs)
 = "(\n" ++ intercalate "\n" (map (funcString "  ") funcs) 
     ++ "\n)" 


funcString :: String -> Function -> String
funcString indent (Function i vars blocks)
 = indent ++ 
   "(" ++ strOfId i
       ++ " (" ++ intercalate "," (map strOfId vars) ++ ")\n"
       ++ intercalate "\n" (map (blockString (indent ++ "  ")) blocks)
       ++ "\n" ++ indent ++ ")"


blockString :: String -> Block -> String
blockString indent (Block i instrs)
 = indent ++ 
   "(" ++ show i ++ "\n"
       ++ intercalate "\n" (map (instrString (indent ++ "   ")) instrs)
       ++ " )"


instrString :: String -> Instr -> String
instrString indent instr
 = indent ++ 
   "(" 
    ++ case instr of
            IConst r n          -> "lc " ++ regString r ++ " " ++ show n
            ILoad  r i          -> "ld " ++ regString r ++ " " ++ strOfId i
            IStore i r          -> "st " ++ strOfId i ++ " " ++ regString r
            IArith o d s1 s2    -> operString o ++ " " ++ regString d ++ " " ++ regString s1 ++ " "
                                                                             ++ regString s2
            IBranch r m n       -> "br " ++ regString r ++ " " ++ show m ++ " " ++ show n
            IReturn r           -> "ret " ++ regString r
            ICall r i args      -> "call " ++ regString r ++ " " ++ strOfId i
                                           ++ intercalate ", " (map regString args)
            IPrint regs         -> "print " ++ intercalate ", " (map regString regs)
    ++ ")"
        

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


regString :: Reg -> String
regString (Reg n) = "r" ++ show n 

