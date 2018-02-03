module Compiler
( compile
, turn_to_code
) where

import           Text.Read

import           CAst
import           CAstHelper
import           Parser



stdlib = CInclude { lib_name="stdlib.h", builtin=True }


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
                                 , CVarDeclOrInit_DeclInit $ declare_yl_false "FALSE"
                                 , CVarDeclOrInit_DeclInit $ declare_yl_false "RET"
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
ctxCompile ctx (AstNode string) = compileVal ctx string
ctxCompile ctx (AstList []) = ctxAddReturnFalse ctx

compileVal :: CompileContext -> String -> CompileContext
compileVal ctx string =
    ctxAddReturnValue ctx $ ctxParseSymbol ctx string


ctxAddReturnValue :: CompileContext -> Value -> CompileContext
ctxAddReturnValue ctx val =
    let fn = setRegValue (currentFunction ctx) RET_REGISTER val
        ast = cAst ctx in
    ctx { cAst=cAstAddFunction ast fn}

ctxAddReturnFalse :: CompileContext -> CompileContext
ctxAddReturnFalse ctx =
    let fn = setReturnRegValueToFalse (currentFunction ctx)
        ast = cAst ctx in
    ctx { cAst=cAstAddFunction ast fn}

ctxParseSymbol :: CompileContext -> String -> Value
ctxParseSymbol ctx symbol =
    let varName = scopeGet (scope ctx) symbol in
    case varName of
        Nothing         -> parseSymbol symbol
        Just identifier -> VAR_TYPE_IDENTIFIER identifier

parseSymbol :: String -> Value
parseSymbol symbol =
    case (readMaybe symbol :: Maybe Integer) of
        Just int -> VAR_TYPE_INT int
        Nothing  ->  case (readMaybe symbol :: Maybe Double) of
            Just f  -> VAR_TYPE_FLOAT f
            Nothing -> VAR_TYPE_STRING symbol

scopeGet :: Scope -> String -> Maybe String
scopeGet (ScopeTopLevel vars) identifier = lookup identifier vars
scopeGet (ChildScope {vars=vars, parent=p}) identifier =
    case (lookup identifier vars) of
        Just v  -> Just v
        Nothing -> scopeGet p identifier
