module ThreeAddressCode
( translate_to_tac
, pretty_tac
) where

import SemanticAST
import           Data.List

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
                      | Call { call_fn :: Address
                             , call_arg_count :: Int
                             }
                      | LabelTAC Label
                      | CreateFunction { fn_name          ::  Address
                                       , fn_params        :: [Address]
                                       , fn_captured_vars :: [Address]
                                       }
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

data FunctionTAC = FunctionTAC { functionAddress :: Address
                               , functionTacs :: [ThreeAddressCode]
                               }
                               deriving (Show)

class Pretty t where
    pretty_tac :: t -> String
    pretty_tac_list :: [t] -> String
    pretty_tac_list list = pretty_tac_list_from_index list 1
    pretty_tac_list_from_index :: [t] -> Int -> String
    pretty_tac_list_from_index [] i = ""
    pretty_tac_list_from_index (h:next) i =
        show i ++ ") " ++ pretty_tac h ++ "\n" ++ pretty_tac_list_from_index next (i+1)

instance Pretty ThreeAddressCode where
    pretty_tac (GoTo l) =
        "goto " ++ pretty_tac l
    pretty_tac (IfTrue (ad, l)) =
        "ifTrue " ++ pretty_tac ad ++ " goto " ++ pretty_tac l
    pretty_tac (IfFalse (ad, l)) =
        "ifFalse " ++ pretty_tac ad ++ " goto " ++ pretty_tac l
    pretty_tac BinOpAssigment {} =
        undefined
    pretty_tac UnOpAssigment {} =
        undefined
    pretty_tac (Copy (lhs, rhs)) =
        pretty_tac lhs ++ " := " ++ pretty_tac rhs
    pretty_tac (Param ad) =
        "param " ++ pretty_tac ad
    pretty_tac (Call {call_fn=ad, call_arg_count=c}) =
        "call " ++ pretty_tac ad ++ " , " ++ show c
    pretty_tac (LabelTAC l) =
        pretty_tac l
    pretty_tac (CreateFunction {fn_name=name, fn_params=params, fn_captured_vars=captured_vars}) =
        let pretty_params = intercalate ", " (map pretty_tac params)
            pretty_capts = intercalate ", " (map pretty_tac captured_vars)
        in
        "makefn " ++ pretty_tac name ++ "(" ++ pretty_params ++ ") " ++
            if length captured_vars == 0 then
                "NO CAPTURES"
            else
                "CAPTURES: [" ++ pretty_capts ++ "]"
    pretty_tac Noop = ""

instance Pretty Label where
    pretty_tac (Label i) = "label_" ++ show i ++ ":"

instance Pretty Address where
    pretty_tac (IntConstant i) = '#': show i
    pretty_tac (FloatConstant d) = '#': show d
    pretty_tac (StringConstant s) = '#': show s
    pretty_tac (Name s) = s
    pretty_tac (TempName i) = 't':show i
    pretty_tac ReturnAddress = "RET"
    pretty_tac FalseAddress = "FALSE"

instance Pretty FunctionTAC where
    pretty_tac FunctionTAC { functionAddress=ad, functionTacs=tacs } =
        "procedure " ++ pretty_tac ad ++ "\n{\n" ++ pretty_tac_list tacs ++ "}"

data TranslateContext = TranslateContext { tacs          :: [ThreeAddressCode]
                                         , functions     :: [FunctionTAC]
                                         , tempCount     :: Integer
                                         , labelCount    :: Integer
                                         , currentFunctionName :: String
                                         }
                                         deriving (Show)
new_context = TranslateContext { tacs=[], functions=[], tempCount=0, labelCount=0, currentFunctionName="main" }

translate_to_tac :: SemanticAST -> [FunctionTAC]
translate_to_tac ast =
    let ctx = translate_to_tac_temp new_context ast
    in
    FunctionTAC { functionAddress=Name "main_0", functionTacs=reverse $ tacs ctx } : functions ctx


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

translate_to_tac_temp ctx (DefFnNode { fn_identifier=id_
                                     , fn_parameters=params
                                     , fn_procedure=procedure
                                     }) =
    let funcName = mangle_identifier id_
        newFunction = CreateFunction { fn_name=funcName
                                     , fn_params=map mangle_identifier params
                                     , fn_captured_vars=map mangle_identifier $ captured_vars id_
                                     }
        ctx' = append_tac ctx newFunction
        fnCtx = translate_to_tac_temp (ctx' {tacs=[], functions=[]}) procedure
        ctx'' = ctx' { functions=(FunctionTAC { functionAddress=funcName
                                              , functionTacs=reverse $ tacs fnCtx
                                              }) : functions fnCtx ++ functions ctx' }
    in
    append_tac ctx'' (Copy (ReturnAddress, funcName))

translate_to_tac_temp ctx (LetNode { let_identifier=id_
                                   , let_rhs=rhs
                                   }) =
    let ctx' = translate_to_tac_temp ctx rhs
    in
    append_tac ctx' (Copy (mangle_identifier id_, ReturnAddress))

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

translate_to_tac_temp ctx (FuncCallNode (id_, argsAst)) =
    let ctx' = set_arguments ctx argsAst
    in
    append_tac ctx' (Call { call_fn=mangle_identifier id_
                          , call_arg_count=length argsAst
                          })
    where
        set_arguments :: TranslateContext -> [SemanticAST] -> TranslateContext
        set_arguments ctx [] = ctx
        set_arguments ctx (h:next) =
            let ctx' = translate_to_tac_temp ctx h
                tmpName = TempName (tempCount ctx)
                ctx'' = copy_to_new_temp_name ctx' ReturnAddress tmpName
                ctx''' = append_tac ctx'' (Param tmpName)
            in
            set_arguments ctx''' next

translate_to_tac_temp ctx LoopNode{} = error("TODO")

append_tac :: TranslateContext -> ThreeAddressCode -> TranslateContext
append_tac ctx tac = ctx { tacs=tac:tacs ctx }

append_label :: TranslateContext -> Label -> TranslateContext
append_label ctx label =
    ctx { tacs=(LabelTAC label):tacs ctx, labelCount=labelCount ctx + 1 }

copy_to_new_temp_name :: TranslateContext -> Address -> Address -> TranslateContext
copy_to_new_temp_name ctx address tmpName =
    case tmpName of
        TempName count ->
            ctx { tacs=(Copy (tmpName, address)):tacs ctx, tempCount=count + 1 }
        _ -> error("Expected a TempName address!")

return_value :: Address -> ThreeAddressCode
return_value address = Copy (ReturnAddress, address)

mangle_identifier :: IdentifierNode -> Address
mangle_identifier (IdentifierNode {id_symbol=str, id_count=count}) =
    Name ("__uvar_" ++ replace str ++ "_" ++ show count)
    where
        replace :: String -> String
        replace [] = []
        replace (h:next) =
            let replacedNext = replace next
                allLetters = "abdefghijklmnopqrstupvwxyzABCDEFGHUIJKLMNOPQRSTUVWXYZ0123456789_"
            in
            if not (elem h allLetters)
                then '_':replacedNext else h:replacedNext
