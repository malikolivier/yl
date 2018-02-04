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

__var_main_0 = newFunction "main" 0
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
                     , func_proc=[ CStatementExp $ CCallExp ("__var_main_0", [])
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
                                     -- , returnValue      :: Bool
                                     -- Count the number of variables defined.
                                     -- This is to assigned them a unique name in C
                                     , varCount         :: Int
                                     -- Store function call stack
                                     , functionStack    :: [CFuncDeclaration]
                                     -- Indicate if we are in the toplevel or not
                                     , topLevel         :: Bool
                                     }
                                     deriving (Show)

currentFunction :: CompileContext -> CFuncDeclaration
currentFunction ctx = head $ functionStack ctx


data Scope = ScopeTopLevel [(String, String)]
           | ChildScope { vars   :: [(String, String)]
                        , parent :: Scope
                        }
           deriving (Show)


startContext = CompileContext { scope=ScopeTopLevel []
                              , cAst=initialCAst
                              , evaluateFunction=False
                              --, returnValue=True
                              , varCount=1
                              , functionStack=[__var_main_0]
                              , topLevel=True
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
ctxCompile ctx (AstList list)
    | topLevel ctx = newCtx { cAst=cAstAddFunction ast topLevelFn }
    | otherwise    = newCtx
        where
            newCtx     = ctxCompileList (ctx { topLevel=False }) list
            ast        = cAst ctx
            topLevelFn = currentFunction newCtx

compileVal :: CompileContext -> String -> CompileContext
compileVal ctx string =
    ctxSetRegister ctx RET_REGISTER $ ctxParseSymbol ctx string

ctxCompileList :: CompileContext -> [Ast] -> CompileContext
ctxCompileList ctx [] = ctxSetRegister ctx RET_REGISTER VAR_TYPE_FALSE
ctxCompileList ctx list
    | evaluateFunction ctx =
        case (head list) of
            AstNode _ ->     ctxCompileFunction ctx list
            AstList _ ->     ctxCompileSimpleList ctx list
    | otherwise            = ctxCompileSimpleList ctx list
    where
        ctxCompileSimpleList ctx (h:[]) =
            ctxCompile (ctx { evaluateFunction=True }) h
        ctxCompileSimpleList ctx (h:next) =
            let newCtx = ctxCompile (ctx { evaluateFunction=True }) h in
                ctxCompileSimpleList newCtx next
        ctxCompileFunction ctx ((AstNode identifier):next) =
            case identifier of
                "def" -> ctxCreateFunction ctx next
                _     -> ctxCallFunction ctx next

-- Create new function
-- Add function to outer scope
-- Declare function arguments, add them to fnScope, extended from outer scope
-- Add new global variables to context cAst
-- Push new function on the function stack
ctxCreateFunction :: CompileContext -> [Ast] -> CompileContext
ctxCreateFunction ctx []     = error "'def' should have at least 2 arguments"
ctxCreateFunction ctx (_:[]) = error "'def' should have at least 2 arguments"
ctxCreateFunction ctx ((AstNode identifier):parameters:expr) =
    let newFn = newFunction identifier (varCount ctx)
        function_name = func_name newFn
        outerScope = scopeSet (scope ctx) identifier function_name
        fnScope = addParametersToFunctionScope (scopeExtend outerScope) parameters (varCount ctx + 1)
        fnCAst = addGlobalVars (cAst ctx) parameters (varCount ctx + 1)
        varCount' = 1 + scopeLength fnScope
        functionStack' = newFn:(functionStack ctx)
        fnCtx = ctx { scope=fnScope, cAst=fnCAst, evaluateFunction=False,
                      varCount=varCount', functionStack=functionStack' }
        fnCompileCompletedCtx = ctxCompile fnCtx (AstList expr)
        fnCompileCompletedCtxCAst = cAstAddFunction (cAst fnCompileCompletedCtx) (currentFunction fnCompileCompletedCtx)
        completedCtx = CompileContext { scope=outerScope
                                      , cAst=fnCompileCompletedCtxCAst
                                      , evaluateFunction=evaluateFunction ctx
                                      , varCount=varCount fnCompileCompletedCtx
                                      , functionStack=functionStack ctx
                                      , topLevel=topLevel ctx
                                      }
    in
        ctxSetRegister completedCtx RET_REGISTER (VAR_TYPE_FUNC function_name)
    where
        addParametersToFunctionScope scope (AstNode str) varCount =
            scopeSet scope str (mangledName str varCount)
        addParametersToFunctionScope scope (AstList []) varCount = scope
        addParametersToFunctionScope scope (AstList (h:next)) varCount =
            let scope' = addParametersToFunctionScope scope (AstList next) (varCount + 1) in
            case h of
                AstNode str ->
                    scopeSet scope' str (mangledName str varCount)
                AstList _   ->
                    error("'def' parameters should be symbols!")
        addGlobalVars cAst (AstNode str) varCount =
            cAstAddGlobalVar cAst str varCount
        addGlobalVars cAst (AstList []) varCount = cAst
        addGlobalVars cAst (AstList (h:next)) varCount =
            let cAst' = addGlobalVars cAst (AstList next) (varCount + 1) in
            case h of
                AstNode str ->
                    cAstAddGlobalVar cAst' str varCount
                AstList _   ->
                    error("'def' parameters should be symbols!")



ctxCallFunction :: CompileContext -> [Ast] -> CompileContext
ctxCallFunction ctx ast =
    ctx -- TODO

ctxSetRegister :: CompileContext -> Register -> Value -> CompileContext
ctxSetRegister ctx reg val =
    let fn = setRegValue (currentFunction ctx) reg val in
    ctx { functionStack=fn:tail (functionStack ctx) }

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

scopeSet :: Scope -> String -> String -> Scope
scopeSet scope yl_identifier c_identifier =
    case scope of
        ScopeTopLevel vars ->
            ScopeTopLevel ((yl_identifier, c_identifier):vars)
        ChildScope { vars=vars, parent=p } ->
            ChildScope { vars=(yl_identifier, c_identifier):vars, parent=p }

scopeExtend :: Scope -> Scope
scopeExtend scope =
    ChildScope { vars=[], parent=scope }

scopeLength :: Scope -> Int
scopeLength scope =
    case scope of
        ScopeTopLevel vars            -> length vars
        ChildScope {vars=v, parent=p} -> length v

incrVarCount :: CompileContext -> CompileContext
incrVarCount ctx =
    ctx { varCount=varCount ctx + 1 }
