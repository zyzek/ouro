-- | Input language grammar.
module Imp.Source.Exp where

-- | Source Programs.
data Program
        = Program [Function]
        deriving (Eq, Show)


-- | Source Functions.
data Function
        = Function Id [Id] [Id] Block
        deriving (Eq, Show)


-- | Source Blocks.
data Block
        = Block [Stmt]
        deriving (Eq, Show)


-- | Source Statements.
data Stmt
        = SAssign     Id   Exp
        | SFAssign    Id   Id    Exp
        | SBAssign    Id   OpBin Exp
        | SIf         Exp  Block
        | SIfElse     Exp  Block Block
        | SReturn     Exp
        | SWhile      Exp Block
        | SPrint  Exp
        | SExp    Exp
        deriving (Eq, Show)


-- | Source Expressions.
data Exp
        = XNum     Int
        | XId      Id
        | XApp     Id [Exp]
        | XOpBin   OpBin Exp Exp
        | XOpUn    OpUn Exp
        | XAssign  Id Exp
        | XFAssign Id Id Exp
        | XBAssign Id OpBin Exp
        | XTernary Exp Exp Exp
        deriving (Eq, Show)


-- | Source Identifiers.
data Id
        = Id    String
        deriving (Show, Eq)


-- | Source Operators.
data OpBin
        = OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpMod
        | OpPow
        | OpLt
        | OpGt
        | OpEq
        | OpNeq
        | OpGeq
        | OpLeq
        | OpOr
        | OpAnd
        | OpXor
        deriving (Show, Eq)

data OpUn
        = OpNeg
        | OpNot
        deriving (Show, Eq)


idString :: Id -> String
idString (Id s) = s