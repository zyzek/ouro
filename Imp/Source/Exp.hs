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
        = SAssign Id   Exp
        | SIf     Exp  Block
        | SIfElse Exp  Block Block
        | SReturn Exp
        | SWhile  Exp Block
        deriving (Eq, Show)


-- | Source Expressions.
data Exp
        = XNum    Int
        | XId     Id
        | XApp    Id [Id]
        | XOpBin  OpBin Exp Exp
        | XOpUn   OpUn Exp
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
        | OpOr
        | OpAnd
        deriving (Show, Eq)

data OpUn
        = OpNeg
        | OpNot
        deriving (Show, Eq)


idString :: Id -> String
idString (Id s) = s