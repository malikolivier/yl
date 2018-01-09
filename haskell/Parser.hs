module Parser
( parse
) where

data Ast = AstNode [Char]
         | AstList [Ast]
         deriving (Show)

parse :: [Char] -> Ast
parse program = AstList []
