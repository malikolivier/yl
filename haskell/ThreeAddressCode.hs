module ThreeAddressCode
( translate_to_tac
) where

import SemanticAST

data ThreeAddressCode = GoTo Label
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
                      | Call { call_rhs :: Address
                             , call_fn :: Address
                             , call_arg_count :: Integer
                             }
                      | LabelTAC Label
                      | Noop
                      deriving (Show)

data Label = Label Integer
           deriving (Show)

data Address = IntConstant Integer
             | FloatConstant Double
             | StringConstant String
             | Name String
             | TempName Integer
             | ReturnAddress
             | FalseAddress
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

data TranslateContext = TranslateContext { tacs :: [ThreeAddressCode]
                                         , tempCount :: Integer
                                         , labelCount :: Integer
                                         }
                                         deriving (Show)
new_context = TranslateContext { tacs=[], tempCount=0, labelCount=0 }

translate_to_tac :: SemanticAST -> [ThreeAddressCode]
translate_to_tac ast = reverse $ tacs $ translate_to_tac_temp new_context ast


translate_to_tac_temp :: TranslateContext -> SemanticAST -> TranslateContext
translate_to_tac_temp ctx NoopNode =
    append_tac ctx $ return_value FalseAddress
translate_to_tac_temp ctx (IntegerNode i) =
    append_tac ctx $ return_value (IntConstant i)
translate_to_tac_temp ctx (FloatNode f) =
    append_tac ctx $ return_value (FloatConstant f)
translate_to_tac_temp ctx (StringNode str) =
    append_tac ctx $ return_value (StringConstant str)
translate_to_tac_temp ctx (IdNode id_) =
    append_tac ctx $ return_value (mangle_identifier id_)
translate_to_tac_temp ctx (ListNode []) = ctx
translate_to_tac_temp ctx (ListNode (h:next)) =
    let ctx' = translate_to_tac_temp ctx h
    in
    translate_to_tac_temp ctx' (ListNode next)

translate_to_tac_temp ctx (IfNode { if_condition=ifAst
                                  , then_procedure=thenAst
                                  , else_procedure=elseAst
                                  }) =
    let ctx' = translate_to_tac_temp ctx ifAst
        go_to_label = Label (labelCount ctx')
        ctx'' = append_tac ctx' (IfFalse (ReturnAddress, go_to_label))
        ctxThen = translate_to_tac_temp ctx'' thenAst
        ctxThen' = append_label ctxThen go_to_label
        ctxElse = translate_to_tac_temp ctxThen' elseAst
    in
    ctxElse

append_tac :: TranslateContext -> ThreeAddressCode -> TranslateContext
append_tac ctx tac = ctx { tacs=tac:tacs ctx }

append_label :: TranslateContext -> Label -> TranslateContext
append_label ctx label =
    ctx { tacs=(LabelTAC label):tacs ctx, labelCount=labelCount ctx + 1 }

return_value :: Address -> ThreeAddressCode
return_value address = Copy (ReturnAddress, address)

mangle_identifier :: IdentifierNode -> Address
mangle_identifier (IdentifierNode {id_symbol=str, id_count=count}) =
    Name (str ++ "_" ++ show count)
