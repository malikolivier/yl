module ThreeAddressCode
(
) where

import SemanticAST


data ThreeAddressCode = ThreeAddressCode { labels :: [Label]
                                         , instruction :: Instruction
                                         }
                                         deriving (Show)

data Instruction = GoTo Label
                 | IfTrue (Address, Label)
                 | IfFalse (Address, Label)
                 | BinOpAssigment { bin_op_rhs :: Address
                                  , bin_op_lhs :: (Address, Address)
                                  , bin_op :: BinOperator
                                  }
                 | UnOpAssigment { un_op_rhs :: Address
                                 , un_op_lhs :: Address
                                 , un_op :: UnOperator
                                 }
                 | Copy (Address, Address)
                 | Param Address
                 | Call (Address, Integer)
                 | Noop
                 deriving (Show)

data Label = Label String
           deriving (Show)

data Address = IntConstant Integer
             | FloatConstant Double
             | StringConstant String
             | Name String
             | TempName Integer
             deriving (Show)

data BinOperator = BinOpPlus
                 | BinOpMinus
                 | BinOpMul
                 | BinOpDiv
                 | BinOpMod
                 | BinOpEq
                 | BinOpLt
                 | BinOpLe
                 | BinOpGt
                 | BinOpGe
                 deriving (Show)

data UnOperator = UnOpMinus
                | UnOpNegation
                | UnOpCastToInteger
                | UnOpCastToFloat
                | UnOpCastToString
                deriving (Show)

translate_to_tac :: SemanticAST -> [ThreeAddressCode]
translate_to_tac (ListNode []) = []
