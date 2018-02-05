module SemanticAST
( semantic_parse
, SemanticAST(..)
) where

import Parser

-- Analysis AST and turn it into a SemanticAST

data SemanticAST = IntegerNode Integer
                 | FloatNode Double
                 | StringNode String
                 | IdNode IdentifierNode
                 | DefFnNode { fn_identifier :: IdentifierNode
                             , fn_parameters :: [String]
                             , fn_procedure  :: SemanticAST
                             -- Stores all the symbols defined in the upper scope
                             , fn_symbol_table :: [IdentifierNode]
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
                 | ListNode [SemanticAST]
                 deriving (Show)

data LoopValues = LoopValuesList [SemanticAST]
                | LoopRangeInt (Integer, Integer)
                | LoopRangeFloat (Double, Double)
                deriving (Show)

data IdentifierNode = IdentifierNode { id_symbol :: String
                                     , id_count  :: Integer
                                     }
                                     deriving (Show)

semantic_parse :: Ast -> SemanticAST
semantic_parse ast = ListNode []
