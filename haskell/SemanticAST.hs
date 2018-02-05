module SemanticAST
( semantic_parse
, SemanticAST(..)
, IdentifierNode(..)
) where

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
                 | FuncCallNode (SemanticAST, [SemanticAST])
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

semantic_parse :: Ast -> SemanticAST
semantic_parse ast = ListNode []
