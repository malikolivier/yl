module Compiler
( compile
, turn_to_code
) where

import           Text.Read

import           CAst
import           CAstHelper
import           Parser



stdlib = CInclude { lib_name="stdlib.h", builtin=True }


var_toi_fn_statements = -- TODO Add case for VAR_TYPE_FLOAT
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

data ValueInScopeType = Callable [String]
                      | NotCallable
                      deriving (Show)
data ValueInScope = ValueInScope { c_identifier :: String
                                 , value_in_scope_type :: ValueInScopeType
                                 }
                  deriving (Show)

data Scope = ScopeTopLevel [(String, ValueInScope)]
           | ChildScope { vars   :: [(String, ValueInScope)]
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
            ast        = cAst newCtx
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
                _     -> ctxCallFunction ctx identifier next

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
        outerScope = scopeSet (scope ctx) identifier ( ValueInScope { c_identifier=function_name
                                                                    , value_in_scope_type=Callable (getParameterNames parameters (varCount ctx + 1))
                                                                    }
                                                     )
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
        getParameterNames :: Ast -> Int -> [String]
        getParameterNames (AstNode str) varCount =
            [mangledName str varCount]
        getParameterNames (AstList []) varCount = []
        getParameterNames (AstList (h:next)) varCount =
            case h of
                AstNode str ->
                    (mangledName str varCount):getParameterNames (AstList next) (varCount + 1)
                AstList _   ->
                    error("'def' parameters should be symbols!")

        addParametersToFunctionScope :: Scope -> Ast -> Int -> Scope
        addParametersToFunctionScope scope (AstNode str) varCount =
            scopeSetNotCallable scope str (mangledName str varCount)
        addParametersToFunctionScope scope (AstList []) varCount = scope
        addParametersToFunctionScope scope (AstList (h:next)) varCount =
            let scope' = addParametersToFunctionScope scope (AstList next) (varCount + 1) in
            case h of
                AstNode str ->
                    scopeSetNotCallable scope' str (mangledName str varCount)
                AstList _   ->
                    error("'def' parameters should be symbols!")

        addGlobalVars :: CAst -> Ast -> Int -> CAst
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

-- Look in scope for called object (it should exist)
-- Set function parameters to value of provided arguments
-- Call function (the RET register will be set inside the function)
ctxCallFunction :: CompileContext -> String -> [Ast] -> CompileContext
ctxCallFunction ctx identifier arguments =
    let maybeCalledValue = scopeGet (scope ctx) identifier in
    case maybeCalledValue of
        Nothing          -> error("Undefined symbol not callable " ++ identifier)
        Just calledValue ->
            case value_in_scope_type calledValue of
                NotCallable     -> error("Cannot call uncallable object: " ++ identifier)
                Callable params -> call ctx (c_identifier calledValue) params arguments
    where
        call :: CompileContext -> String -> [String] -> [Ast] -> CompileContext
        call ctx c_identifier params arguments =
            let ctx' = setParamValues ctx params arguments in
            ctxAddFunctionCall ctx c_identifier

        setParamValues :: CompileContext -> [String] -> [Ast] -> CompileContext
        setParamValues ctx [] arguments = ctx
        setParamValues ctx (param:next) [] = -- set param to FALSE
            let ctx' = setParamValues ctx next [] in
            ctxSetVarValue ctx' param VAR_TYPE_FALSE
        setParamValues ctx (param:next_param) (argument:next_arg) =
            let ctx'  = ctxCompile (ctx { evaluateFunction=True }) argument
                ctx'' = ctxSetVarValue ctx' param (VAR_TYPE_IDENTIFIER "RET")
            in
            setParamValues ctx'' next_param next_arg

ctxSetRegister :: CompileContext -> Register -> Value -> CompileContext
ctxSetRegister ctx reg val = ctxSetVarValue ctx (show reg) val

ctxSetVarValue :: CompileContext -> String -> Value -> CompileContext
ctxSetVarValue ctx c_identifier val =
    let fn = setVarValue (currentFunction ctx) c_identifier val in
    ctx { functionStack=fn:tail (functionStack ctx) }

ctxAddFunctionCall :: CompileContext -> String -> CompileContext
ctxAddFunctionCall ctx c_identifier =
    let fn = addCallToFunctionPointerNoArg (currentFunction ctx) c_identifier in
    ctx { functionStack=fn:tail (functionStack ctx) }

ctxParseSymbol :: CompileContext -> String -> Value
ctxParseSymbol ctx symbol =
    let varName = scopeGetCIdentifier (scope ctx) symbol in
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

scopeGet :: Scope -> String -> Maybe ValueInScope
scopeGet (ScopeTopLevel vars) identifier = lookup identifier vars
scopeGet (ChildScope {vars=vars, parent=p}) identifier =
    case (lookup identifier vars) of
        Just v  -> Just v
        Nothing -> scopeGet p identifier

scopeGetCIdentifier :: Scope -> String -> Maybe String
scopeGetCIdentifier scope identifier =
    case (scopeGet scope identifier) of
        Just v  -> Just (c_identifier v)
        Nothing -> Nothing

scopeSet :: Scope -> String -> ValueInScope -> Scope
scopeSet scope yl_identifier value =
    case (scopeGet scope yl_identifier) of
        Just _ -> error(yl_identifier ++ " is already defined in scope!")
        Nothing ->
            case scope of
                ScopeTopLevel vars ->
                    ScopeTopLevel ((yl_identifier, value):vars)
                ChildScope { vars=vars, parent=p } ->
                    ChildScope { vars=(yl_identifier, value):vars, parent=p }

-- Will have to change an uncallable value to callable value and vice-versa
-- during assignment, especially when setting scope before calling a function or
-- using let
scopeSetNotCallable :: Scope -> String -> String -> Scope
scopeSetNotCallable scope yl_identifier c_identifier =
    scopeSet scope yl_identifier (ValueInScope { c_identifier=c_identifier, value_in_scope_type=NotCallable})

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
