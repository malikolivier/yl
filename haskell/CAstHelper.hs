module CAstHelper
( join_with_dot_op
, setReturnRegValueToFalse
, setReturnRegValue
, cAstAddFunction
, declare_yl_false
, enum_var_type
, var_struct
) where

import CAst

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

yl_false_initialization = StructInitialization [("type", IdentifierInitialization "VAR_TYPE_FALSE")]
declare_yl_false :: [Char] -> CVarDeclarationAndInitialization
declare_yl_false identifier = CVarDeclarationAndInitialization {
      declaration=CVarDeclaration {identifier=identifier, ctype=CTypeStruct var_struct}
    , initialization=yl_false_initialization
}


cAstAddFunction :: CAst -> CFuncDeclaration -> CAst
cAstAddFunction ast fn =
    let funcs = functions ast in
    ast { functions=(fn:funcs) }

setReturnRegValueToFalse :: CFuncDeclaration -> CFuncDeclaration
setReturnRegValueToFalse fn =
    setReturnRegValue fn $ CVariableExp "FALSE"

setReturnRegValue :: CFuncDeclaration -> CExp -> CFuncDeclaration
setReturnRegValue fn expr =
    let procs = func_proc fn
        new_proc = CStatementExp (CBinaryExp (CBinSingleEq, CVariableExp "RET", expr)) in
    fn { func_proc=new_proc:procs }
