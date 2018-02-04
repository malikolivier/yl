module CAst
( CAst(..)
, CInclude(..)
, CTypeDeclaration(..)
, CEnum(..)
, CStruct(..)
, CVarDeclaration(..)
, CType(..)
, CUnion(..)
, CVarDeclarationAndInitialization(..)
, CVarDeclarationOrInitialization(..)
, CVarInitialization(..)
, CFuncDeclaration(..)
, CStatement(..)
, CExp(..)
, CBinOperator(..)
, CUnaryOperator(..)
) where

import           Data.List


data CAst = CAst { includes          :: [CInclude]
                 , type_declarations :: [CTypeDeclaration]
                 , global_vars       :: [CVarDeclarationOrInitialization]
                 , functions         :: [CFuncDeclaration]
                 }

data CInclude = CInclude { lib_name :: [Char]
                         , builtin  :: Bool
                         }


data CTypeDeclaration = CTypeDeclarationEnum CEnum
                      | CTypeDeclarationStruct CStruct

data CEnum = CEnum { enum_name :: [Char]
                   , enum      :: [[Char]]
                   }

data CStruct = CStruct { struct_name :: [Char]
                       , members     :: [CVarDeclaration]}

data CVarDeclaration = CVarDeclaration { identifier :: [Char]
                                       , ctype      :: CType
                                       }
data CType = CTypeEnum CEnum
           | CTypeStruct CStruct
           | CTypeUnion CUnion
           | CVoid
           | CInt
           | CLong
           | CDouble
           | CChar
           | CVoidFunc
           | CPointer CType

data CUnion = CUnion { union_name :: [Char]
                     , union_vars :: [CVarDeclaration]
                     }

data CVarDeclarationAndInitialization = CVarDeclarationAndInitialization {
          declaration    :: CVarDeclaration
        , initialization :: CVarInitialization
        }

data CVarDeclarationOrInitialization = CVarDeclOrInit_Decl CVarDeclaration
                                     | CVarDeclOrInit_DeclInit CVarDeclarationAndInitialization

data CVarInitialization = IntInitialization Integer
                        | FloatInitialization Double
                        | StructInitialization [([Char], CVarInitialization)]
                        | StringInitialization String
                        | IdentifierInitialization [Char]

data CFuncDeclaration = CFuncDeclaration { func_name       :: [Char]
                                         , func_parameters :: [CVarDeclaration]
                                         , return_type     :: CType
                                         , func_proc       :: [CStatement]
                                         }

data CStatement = CStatementVarDeclarationAndInitialization CVarDeclarationAndInitialization
                | CStatementVarInitialization CVarInitialization
                | CStatementExp CExp
                | CStatementReturn CExp
                | CStatementBlock [CStatement]
                | CStatementIfElse (CExp, CStatement, CStatement)
                | CStatementIf (CExp, CStatement)
                | CStatementWhile (CExp, CStatement)
                | CStatementSwitch (CExp, [(CExp, [CStatement])], [CStatement])
                | CStatementBreak
                | CStatementEmpty

data CExp = CIntExp Integer
          | CFloatExp Double
          | CStringExp [Char]
          | CVariableExp [Char]
          | CBinaryExp (CBinOperator, CExp, CExp)
          | CUnaryExp (CUnaryOperator, CExp)
          | CCallExp ([Char], [CExp])

data CBinOperator = CBinPlus
                  | CBinMinus
                  | CBinMul
                  | CBinDiv
                  | CBinMod
                  | CBinSingleEq
                  | CBinDoubleEq
                  | CBinGt
                  | CBinGe
                  | CBinLt
                  | CBinLe
                  | CBinDot
                  | CBinArrow

data CUnaryOperator = CUnMinus
                    | CUnDeref
                    | CUnRef
                    | CUnCast CType

instance Show CInclude where
    show CInclude{lib_name=name, builtin=True} =
        "#include <" ++ name ++ ">\n"
    show CInclude{lib_name=name, builtin=False} =
        "#include \"" ++ name ++ "\"\n"

instance Show CEnum where
    show CEnum{enum_name=name, enum=list} =
        "enum " ++ name ++ " {\n" ++ intercalate ",\n\t" list ++ " };\n"

instance Show CStruct where
    show CStruct{struct_name=name, members=list} =
        let content = intercalate "\t" $ map show list in
            "struct " ++ name ++ " {\n" ++ content ++ " };\n"

instance Show CVarDeclaration where
    show CVarDeclaration{identifier=id_, ctype=CVoidFunc} =
        "void (*" ++ id_ ++ ")();\n"
    show CVarDeclaration{identifier=id_, ctype=ctype} =
        show ctype ++ " " ++ id_ ++ ";\n"

instance Show CType where
    show (CTypeEnum e) = "enum " ++ enum_name e
    show (CTypeStruct s) = "struct " ++ struct_name s
    -- Treats anonymous union separately
    show (CTypeUnion (CUnion {union_name="", union_vars=vars})) =
        let content = intercalate "\t" $ map show vars in
            "union {\n" ++ content ++ " }"
    show (CTypeUnion u) = "union " ++ union_name u
    show CVoid = "void"
    show CInt = "int"
    show CLong = "long"
    show CDouble = "double"
    show CChar = "char"
    show CVoidFunc = undefined
    show (CPointer t) = (show t) ++ "*"

instance Show CUnion where
    show CUnion { union_name=name, union_vars=vars } =
        let content = intercalate "\t" $ map show vars in
            "union " ++ name ++ " {\n" ++ content ++ " };\n"

instance Show CAst where
    show CAst { includes=i, type_declarations=types, global_vars=vars, functions=f } =
        let headers = intercalate "" $ map show i
            type_decl = intercalate "" $ map show types
            vars_str = intercalate "" $ map show vars
            fn_prototypes = intercalate "" $ map function_prototype f
            fn_str = intercalate "" $ map show f in
        headers ++ type_decl ++ vars_str ++ fn_prototypes ++ fn_str

instance Show CTypeDeclaration where
    show (CTypeDeclarationEnum enum)     = show enum
    show (CTypeDeclarationStruct struct) = show struct

instance Show CVarDeclarationAndInitialization where
    show CVarDeclarationAndInitialization { declaration=CVarDeclaration { identifier=id_
                                                                        , ctype=ctype}
                                          , initialization=ini} =
        show ctype ++ " " ++ id_ ++ " = " ++ show ini ++ ";\n"

instance Show CVarDeclarationOrInitialization where
    show (CVarDeclOrInit_Decl decl)         = show decl
    show (CVarDeclOrInit_DeclInit decl_ini) = show decl_ini

instance Show CFuncDeclaration where
    show CFuncDeclaration { func_name=name, func_parameters=params, return_type=ret, func_proc=statements} =
        let print_param CVarDeclaration{identifier=id_, ctype=ctype} = show ctype ++ " " ++ id_
            params_str = intercalate ", " $ map print_param params
            procs = intercalate "" $ map show statements in
        show ret ++ "\n" ++ name ++ "(" ++ params_str ++ ")\n{\n" ++ procs ++ "\n}\n"

function_prototype :: CFuncDeclaration -> String
function_prototype CFuncDeclaration { func_name=name, func_parameters=params, return_type=ret, func_proc=_} =
    let print_param CVarDeclaration{identifier=id_, ctype=ctype} = show ctype ++ " " ++ id_
        params_str = intercalate ", " $ map print_param params
    in
    show ret ++ " " ++ name ++ "(" ++ params_str ++ ");\n"

instance Show CVarInitialization where
    show (IntInitialization i) = show i
    show (FloatInitialization f) = show f
    show (IdentifierInitialization id_) = id_
    show (StructInitialization struct) =
        let print_elem (id_, ini) = "." ++ id_ ++ "=" ++ show ini
            content = intercalate "," $ map print_elem struct in
            "{ " ++ content ++ " }"

instance Show CStatement where
    show (CStatementVarDeclarationAndInitialization s) = show s
    show (CStatementVarInitialization s) = show s
    show (CStatementExp expr) = show expr ++ ";\n"
    show (CStatementReturn expr) = "return " ++ show expr ++ ";\n"
    show (CStatementBlock statements) =
        let content = intercalate "" $ map show statements in
        "{\n" ++ content ++ " }\n"
    show (CStatementIfElse (condition, then_statement, else_statement)) =
        show (CStatementIf (condition, then_statement)) ++ " else " ++ show else_statement
    show (CStatementIf (condition, then_statement)) =
        "if (" ++ show condition ++ ") " ++ show then_statement
    show (CStatementWhile (condition, loop_statement)) =
        "while (" ++ show condition ++ ") " ++ show loop_statement
    show (CStatementSwitch (condition, case_list, default_case_statements)) =
        let print_case (expr, statements) =
                "case " ++ show expr ++ ":\n\t" ++ intercalate "" (map show statements)
            content = intercalate "\n" $ map print_case case_list
            default_content = "default: " ++ intercalate "\n" (map show default_case_statements) in
        "switch (" ++ show condition ++ ") { " ++ content ++ default_content ++ "}"
    show CStatementBreak = "break;\n"
    show CStatementEmpty = ";\n"

instance Show CExp where
    show (CIntExp i) = show i
    show (CFloatExp f) = show f
    show (CStringExp s) = show s
    show (CVariableExp v) = v
    show (CBinaryExp (op, left, right)) = "(" ++ show left ++ show op ++ show right ++ ")"
    show (CUnaryExp (op, operand)) = "(" ++ show op ++ show operand ++ ")"
    show (CCallExp (func, exprs)) =
        let args = intercalate ", " $ map show exprs in
        func ++ "(" ++ args ++ ")"

instance Show CBinOperator where
    show CBinPlus     = "+"
    show CBinMinus    = "-"
    show CBinMul      = "+"
    show CBinDiv      = "/"
    show CBinMod      = "%"
    show CBinSingleEq = "="
    show CBinDoubleEq = "=="
    show CBinGt       = ">"
    show CBinGe       = ">="
    show CBinLt       = "<"
    show CBinLe       = "<="
    show CBinDot      = "."
    show CBinArrow    = "->"

instance Show CUnaryOperator where
    show CUnMinus        = "-"
    show CUnDeref        = "*"
    show CUnRef          = "&"
    show (CUnCast ctype) = "( " ++ show ctype ++ ")"
