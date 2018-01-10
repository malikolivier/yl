module Parser
( parse
, Ast(..)
) where

data Ast = AstNode [Char]
         | AstList [Ast]
         deriving (Show)

data Token = Open
           | Close
           | Symbol [Char]
           deriving (Show)

tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize (c:next)
  | c `elem` " \n\t" = tokenize next
tokenize (';':next) = tokenize $ skipComment next
tokenize ('"':next) = readString next
tokenize ('(':next) = Open : tokenize next
tokenize (')':next) = Close : tokenize next
tokenize all@(c:next) = readSymbol all

skipComment :: [Char] -> [Char]
skipComment [] = []
skipComment ('\n':next) = next
skipComment (c:next) = skipComment next

readString :: [Char] -> [Token]
readString code = let (string, next) = readStringTokenValue "" code in
    Symbol string : tokenize next

readStringTokenValue :: [Char] -> [Char] -> ([Char], [Char])
readStringTokenValue result [] = (result, "")
readStringTokenValue result ('"':next) = (result, next)
readStringTokenValue result ('\\':'"':next) = readStringTokenValue (result ++ ['"']) next
readStringTokenValue result (c:next) = readStringTokenValue (result ++ [c]) next

readSymbol :: [Char] -> [Token]
readSymbol code = let (string, next) = readSymbolTokenValue "" code in
    Symbol string : tokenize next

readSymbolTokenValue :: [Char] -> [Char] -> ([Char], [Char])
readSymbolTokenValue result [] = (result, "")
readSymbolTokenValue result all@(c:next)
    | c `elem` " \n\t()" = (result, all)
    | otherwise = readSymbolTokenValue (result ++ [c]) next

_parse :: [Token] -> Ast
_parse tokens = AstList $ _parseInner tokens

_parseInner :: [Token] -> [Ast]
_parseInner [] = []
_parseInner (Symbol string:next) = AstNode string : _parseInner next
-- Tokens after closing parenthesis are handled in (Open:next) pattern
_parseInner (Close:_) = []
_parseInner (Open:next) = let (context, after) = splitAt (countUntilCorrespondingClose next) next in
    (AstList (_parseInner context)) : _parseInner after

countUntilCorrespondingClose :: [Token] -> Int
countUntilCorrespondingClose = countUntilCorrespondingCloseIn 1

countUntilCorrespondingCloseIn :: Int -> [Token] -> Int
countUntilCorrespondingCloseIn 0 _ = 0
countUntilCorrespondingCloseIn depthCounter [] = 0
countUntilCorrespondingCloseIn depthCounter (Open:next) = 1 + countUntilCorrespondingCloseIn (depthCounter + 1) next
countUntilCorrespondingCloseIn depthCounter (Close:next) = 1 + countUntilCorrespondingCloseIn (depthCounter - 1) next
countUntilCorrespondingCloseIn depthCounter (Symbol _:next) = 1 + countUntilCorrespondingCloseIn depthCounter next


parse :: [Char] -> Ast
parse = _parse . tokenize
