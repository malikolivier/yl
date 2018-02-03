module CAstHelper
( join_with_dot_op
, cAstAddFunction
, declare_yl_false
, enum_var_type
, var_struct
, Value(..)
, Register(..)
, setRegValue
) where

import CAst

data Value = VAR_TYPE_FALSE
           | VAR_TYPE_INT Integer
           | VAR_TYPE_FLOAT Double
           | VAR_TYPE_STRING String
           | VAR_TYPE_FUNC String
           | VAR_TYPE_IDENTIFIER String
           deriving (Show)

data Register = RET_REGISTER
              | REGISTER_NO Integer

instance Show Register where
    show RET_REGISTER     = "RET"
    show (REGISTER_NO i) = "REG" ++ show i

enum_exp :: Value -> CExp
enum_exp VAR_TYPE_FALSE = CVariableExp "VAR_TYPE_FALSE"
enum_exp (VAR_TYPE_INT _) = CVariableExp "VAR_TYPE_INT"
enum_exp (VAR_TYPE_FLOAT _) = CVariableExp "VAR_TYPE_FLOAT"
enum_exp (VAR_TYPE_STRING _) = CVariableExp "VAR_TYPE_STRING"
enum_exp (VAR_TYPE_FUNC _) = CVariableExp "VAR_TYPE_FUNC"

val_struct_accessor :: Value -> String
val_struct_accessor (VAR_TYPE_INT _) = "i"
val_struct_accessor (VAR_TYPE_FLOAT _) = "f"
val_struct_accessor (VAR_TYPE_STRING _) = "str"
val_struct_accessor (VAR_TYPE_FUNC _) = "fn"

val_exp :: Value -> CExp
val_exp (VAR_TYPE_INT i) = CIntExp i
val_exp (VAR_TYPE_FLOAT f) = CFloatExp f
val_exp (VAR_TYPE_STRING str) = CStringExp str
val_exp (VAR_TYPE_FUNC fn) = CVariableExp fn

enum_var_type = CEnum { enum_name="var_type", enum=["VAR_TYPE_FALSE",
                                                    "VAR_TYPE_INT",
                                                    "VAR_TYPE_FLOAT",
                                                    "VAR_TYPE_STRING",
                                                    "VAR_TYPE_FUNC"] }
var_union = CUnion { union_name="" -- anonymous union
                   , union_vars=[ CVarDeclaration {identifier="i", ctype=CLong}
                                , CVarDeclaration {identifier="f", ctype=CDouble}
                                , CVarDeclaration {identifier="str", ctype=CPointer CChar}
                                , CVarDeclaration {identifier="fn", ctype=CVoidFunc}
                                ]
                   }

var_struct = CStruct { struct_name="var"
                     , members=[ CVarDeclaration { identifier="type"
                                                 , ctype=CTypeEnum enum_var_type
                                                 }
                               , CVarDeclaration { identifier="u"
                                                 , ctype=CTypeUnion var_union
                                                 }
                               ]
                     }


join_with_dot_op :: [String] -> CExp
join_with_dot_op [] = undefined
join_with_dot_op list =
    let rev = reverse list
        join (h:[])   = CVariableExp h
        join (h:next) = CBinaryExp (CBinDot, join next, CVariableExp h)
    in
    join rev

declare_yl :: String -> CVarInitialization -> CVarDeclarationAndInitialization
declare_yl identifier init_var =
    CVarDeclarationAndInitialization {
          declaration=CVarDeclaration {identifier=identifier, ctype=CTypeStruct var_struct}
        , initialization=init_var
    }

declare_yl_false :: String -> CVarDeclarationAndInitialization
declare_yl_false identifier =
    declare_yl identifier $ StructInitialization [("type", IdentifierInitialization "VAR_TYPE_FALSE")]

declare_yl_int :: String -> Integer -> CVarDeclarationAndInitialization
declare_yl_int identifier i =
    declare_yl identifier $ StructInitialization [ ("type", IdentifierInitialization "VAR_TYPE_INT")
                                                 , ("u.i",  IntInitialization i)]

declare_yl_float :: String -> Double -> CVarDeclarationAndInitialization
declare_yl_float identifier f =
    declare_yl identifier $ StructInitialization [ ("type", IdentifierInitialization "VAR_TYPE_FLOAT")
                                                 , ("u.f",  FloatInitialization f)]

declare_yl_string :: String -> String -> CVarDeclarationAndInitialization
declare_yl_string identifier str =
    declare_yl identifier $ StructInitialization [ ("type", IdentifierInitialization "VAR_TYPE_STRING")
                                                 , ("u.str",  StringInitialization str)]

declare_yl_func :: String -> String -> CVarDeclarationAndInitialization
declare_yl_func identifier fn =
    declare_yl identifier $ StructInitialization [ ("type", IdentifierInitialization "VAR_TYPE_FUNC")
                                                 , ("u.fn",  IdentifierInitialization fn)]

cAstAddFunction :: CAst -> CFuncDeclaration -> CAst
cAstAddFunction ast fn =
    let funcs = functions ast in
    ast { functions=(fn:funcs) }

-- Set arbitrary register to arbitrary data
setRegValue :: CFuncDeclaration -> Register -> Value -> CFuncDeclaration
setRegValue fn reg (VAR_TYPE_IDENTIFIER identifier) =
    let procs = func_proc fn
        new_proc = CStatementExp (CBinaryExp (CBinSingleEq, CVariableExp (show reg), CVariableExp identifier)) in
    fn { func_proc=new_proc:procs }
setRegValue fn reg val =
    let procs = func_proc fn
        rhs_type = join_with_dot_op [show reg, "type"]
        lhs_type = enum_exp val
        rhs_val = join_with_dot_op [show reg, "u", val_struct_accessor val]
        lhs_val = val_exp val
        set_type_proc = CStatementExp (CBinaryExp (CBinSingleEq, rhs_type, lhs_type))
        set_val_proc = CStatementExp (CBinaryExp (CBinSingleEq, rhs_val, lhs_val))
    in
    case val of
        VAR_TYPE_FALSE -> fn { func_proc=set_type_proc:procs }
        _              -> fn { func_proc=set_type_proc:set_val_proc:procs }
