module Compiler
( compile
, turn_to_code
) where

import           Text.Read

import           CAst
import           Parser



stdlib = CInclude { lib_name="stdlib.h", builtin=True }
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

yl_false_initialization = StructInitialization [("type", IdentifierInitialization "VAR_TYPE_FALSE")]
make_yl_false :: [Char] -> CVarDeclarationAndInitialization
make_yl_false identifier = CVarDeclarationAndInitialization {
      declaration=CVarDeclaration {identifier=identifier, ctype=CTypeStruct var_struct}
    , initialization=yl_false_initialization
}

var_toi_fn_statements =
    [CStatementSwitch ( CBinaryExp (CBinDot, CVariableExp "obj", CVariableExp "type")
                      , [ (CVariableExp "VAR_TYPE_INT"
                          , [CStatementReturn $ join_with_dot_op ["obj", "u", "i"]]
                          )
                        ]
                      , [CStatementReturn $ CIntExp 0]
                      )
    ]
var_toi_fn = CFuncDeclaration { func_name="var_toi"
                              , func_parameters=[ CVarDeclaration { identifier="obj"
                                                                  , ctype=CTypeStruct var_struct
                                                                  }
                                                ]
                              , return_type=CInt
                              , func_proc=var_toi_fn_statements}

__var_main_0000 =
    CFuncDeclaration { func_name="__var_main_0000"
                     , func_parameters=[]
                     , return_type=CVoid
                     , func_proc=[]
                     }
main_fn =
    CFuncDeclaration { func_name="main"
                     , func_parameters=[ CVarDeclaration { identifier="argc"
                                                         , ctype=CInt
                                                         }
                                       , CVarDeclaration { identifier="argv"
                                                         , ctype=CPointer (CPointer CChar)
                                                         }
                                       ]
                     , return_type=CInt
                     , func_proc=[ CStatementExp $ CCallExp ("__var_main_0000", [])
                                 , CStatementReturn $ CCallExp ("var_toi", [CVariableExp "RET"])
                                 ]
                  }
initialCAst = CAst { includes=[stdlib],
                     type_declarations=[ CTypeDeclarationEnum enum_var_type
                                       , CTypeDeclarationStruct var_struct
                                       ],
                     global_vars=[ CVarDeclOrInit_Decl CVarDeclaration{identifier="ARGC", ctype=CInt}
                                 , CVarDeclOrInit_Decl CVarDeclaration{identifier="ARGV", ctype=CPointer (CTypeStruct var_struct)}
                                 , CVarDeclOrInit_DeclInit $ make_yl_false "FALSE"
                                 , CVarDeclOrInit_DeclInit $ make_yl_false "RET"
                                 ],
                     functions = [var_toi_fn, main_fn]}

-- Keep track of current context during compilation
data CompileContext = CompileContext { scope            :: Scope
                                     -- Ultimate output, the cAst
                                     , cAst             :: CAst
                                     -- Indicate whethe ylAst should be treated as a function to be evaluated
                                     , evaluateFunction :: Bool
                                     -- Indicate whether a return value is needed in the current block
                                     , returnValue      :: Bool
                                     -- Count the number of variables defined.
                                     -- This is to assigned them a unique name in C
                                     , varCount         :: Int
                                     -- Store current function before it is completed
                                     , currentFunction  :: CFuncDeclaration
                                     }
                                     deriving (Show)

data Scope = ScopeTopLevel [(String, String)]
           | ChildScope { vars   :: [(String, String)]
                        , parent :: Scope
                        }
           deriving (Show)

startContext = CompileContext { scope=ScopeTopLevel []
                              , cAst=initialCAst
                              , evaluateFunction=True
                              , returnValue=True
                              , varCount=1
                              , currentFunction=__var_main_0000
                              }

compile :: Ast -> CAst
compile ast =
    let ctx = ctxCompile startContext ast in
    cAst ctx

turn_to_code :: CAst -> String
turn_to_code cAst = show cAst

-- Update context
ctxCompile :: CompileContext -> Ast -> CompileContext
ctxCompile ctx (AstList []) = ctxAddReturnFalse ctx
    -- | returnValue ctx =
    -- | otherwise       = ctx
ctxCompile ctx (AstNode string) = compileVal ctx string

compileVal :: CompileContext -> String -> CompileContext
compileVal ctx string =
    ctxAddReturnValue ctx $ ctxParseSymbol ctx string


ctxAddReturnValue :: CompileContext -> CExp -> CompileContext
ctxAddReturnValue ctx expr =
    let fn = setReturnRegValue (currentFunction ctx) expr
        ast = cAst ctx in
    ctx { cAst=cAstAddFunction ast fn}

ctxAddReturnFalse :: CompileContext -> CompileContext
ctxAddReturnFalse ctx =
    let fn = setReturnRegValueToFalse (currentFunction ctx)
        ast = cAst ctx in
    ctx { cAst=cAstAddFunction ast fn}

ctxParseSymbol :: CompileContext -> String -> CExp
ctxParseSymbol ctx symbol =
    let varName = scopeGet (scope ctx) symbol in
    case varName of
        Nothing         -> parseSymbol symbol
        Just identifier -> CVariableExp identifier

parseSymbol :: String -> CExp
parseSymbol symbol =
    case (readMaybe symbol :: Maybe Integer) of
        Just int -> CIntExp int
        Nothing  ->  case (readMaybe symbol :: Maybe Double) of
            Just f  -> CFloatExp f
            Nothing -> CStringExp symbol

scopeGet :: Scope -> String -> Maybe String
scopeGet (ScopeTopLevel vars) identifier = lookup identifier vars
scopeGet (ChildScope {vars=vars, parent=p}) identifier =
    case (lookup identifier vars) of
        Just v  -> Just v
        Nothing -> scopeGet p identifier
