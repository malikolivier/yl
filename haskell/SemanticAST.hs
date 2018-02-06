module SemanticAST
( semantic_parse
, SemanticAST(..)
, IdentifierNode(..)
) where

import Text.Read

import Parser

-- Analysis AST and turn it into a SemanticAST

data SemanticAST = IntegerNode Integer
                 | FloatNode Double
                 | StringNode String
                 | IdNode IdentifierNode
                 | DefFnNode { fn_identifier    :: IdentifierNode
                             , fn_parameters    :: [IdentifierNode]
                             , fn_procedure     :: SemanticAST
                             }
                 | LetNode { let_identifier :: IdentifierNode
                           , let_rhs :: SemanticAST }
                 | IfNode { if_condition :: SemanticAST
                          , else_procedure :: SemanticAST
                          , then_procedure :: SemanticAST
                          }
                 | LoopNode { loop_identifier :: IdentifierNode
                            , loop_values :: LoopValues
                            , loop_procedure :: SemanticAST
                            }
                 | FuncCallNode (IdentifierNode, [SemanticAST])
                 | ListNode [SemanticAST]
                 | NoopNode
                 deriving (Show)

data LoopValues = LoopValuesList [SemanticAST]
                | LoopRangeInt (Integer, Integer)
                | LoopRangeFloat (Double, Double)
                deriving (Show)

data IdentifierNode = IdentifierNode { id_symbol     :: String
                                     , id_count      :: Integer
                                     , captured_vars :: [IdentifierNode]
                                     }
                                     deriving (Show)

data SemanticParseContext = SemanticParseContext { scope  :: Scope
                                                 , semAst :: SemanticAST
                                                 }

data Scope = TopLevel [IdentifierNode]
           | ChildScope { scope_parent :: Scope
                        , scope_vars :: [IdentifierNode]
                        }

new_parse_context = SemanticParseContext { scope=TopLevel [], semAst=NoopNode }

semantic_parse :: Ast -> SemanticAST
semantic_parse ast = semAst $ semantic_parse_temp new_parse_context ast False

semantic_parse_temp :: SemanticParseContext -> Ast -> Bool -> SemanticParseContext
semantic_parse_temp ctx (AstNode str) _ = semantic_parse_val ctx str
semantic_parse_temp ctx (AstList list) evaluateFunction =
    semantic_parse_list ctx list evaluateFunction

semantic_parse_val :: SemanticParseContext -> String -> SemanticParseContext
semantic_parse_val ctx str =
    let val = scope_get (scope ctx) str
    in
    case val of
        Nothing -> ctx { semAst=parse_string str }
        Just v  -> ctx { semAst=IdNode v }

semantic_parse_list :: SemanticParseContext -> [Ast] -> Bool -> SemanticParseContext
semantic_parse_list ctx [] _ = ctx { semAst=NoopNode }
semantic_parse_list ctx (h:[]) False =
    let ctx' = semantic_parse_temp ctx h True in
    ctx' { semAst=ListNode [semAst ctx'] }
semantic_parse_list ctx (h:next) False =
    let
        ctx' = semantic_parse_temp ctx h True
        ctx'' = semantic_parse_list ctx' next False
        semAst'' = semAst ctx''
    in
    case semAst'' of
        ListNode l -> ctx'' { semAst=ListNode (semAst ctx':l) }
        _          -> error("Expected semAst'' to be a ListNode")
semantic_parse_list ctx all@(h:next) True =
    case h of
        AstList list    -> semantic_parse_list ctx all False
        AstNode "let"   -> undefined
        AstNode "def"   -> undefined
        AstNode "if"    -> undefined
        AstNode "loop"  -> undefined
        AstNode identifier ->
            let var = scope_get (scope ctx) identifier in
            case var of
                Nothing  -> semantic_parse_list ctx all False
                Just id_ -> sementic_parse_call ctx id_ next

sementic_parse_call :: SemanticParseContext -> IdentifierNode -> [Ast] -> SemanticParseContext
sementic_parse_call ctx id_ args =
    let (ctx', semArgs) = getArgs ctx args
        newCall = FuncCallNode (id_, semArgs)
    in
    ctx' { semAst=newCall }
    where
        getArgs :: SemanticParseContext -> [Ast] -> (SemanticParseContext, [SemanticAST])
        getArgs ctx [] = (ctx, [])
        getArgs ctx (h:next) =
            let ctx' = semantic_parse_temp ctx h True
                (ctx'', nextArgs) = getArgs ctx' next
            in
            (ctx'', semAst ctx' : nextArgs)


scope_get :: Scope -> String -> Maybe IdentifierNode
scope_get scope id_ =
    case scope of
        TopLevel vars -> find vars id_
        ChildScope {scope_parent=p, scope_vars=vars } ->
            case find vars id_ of
                Just idNode  -> Just idNode
                Nothing      -> scope_get p id_
    where
        find :: [IdentifierNode] -> String -> Maybe IdentifierNode
        find [] _ = Nothing
        find (h:next) id_ =
            if id_ == id_symbol h then Just h else find next id_

parse_string :: String -> SemanticAST
parse_string string =
    case (readMaybe string :: Maybe Integer) of
        Just int -> IntegerNode int
        Nothing  -> case (readMaybe string :: Maybe Double) of
            Just num -> FloatNode num
            Nothing  -> StringNode string
