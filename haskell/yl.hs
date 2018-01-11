import Control.Monad

import System.Environment
import System.IO

import Interpreter
import Parser

main = do
    args <- getArgs
    code <- getCode args
    runProgram $ createProgram code args

getCode :: [[Char]] -> IO [Char]
-- Interactive mode
getCode [] = do
    hSetBuffering stdout NoBuffering
    interativeMode globalScope
    error "Interactive mode is a loop!"
getCode ("-e":program:_) = return program
getCode (file:_) = do
    handle <- openFile file ReadMode
    hGetContents handle

createProgram :: [Char] -> [[Char]] -> Ast
createProgram code ("-e":_:ylArgs) = addArgvProgram ylArgs $ parse code
createProgram code (file:ylArgs) = addArgvProgram ylArgs $ parse code

addArgvProgram :: [[Char]] -> Ast -> Ast
addArgvProgram ylArgs (AstList list) = AstList $ (createArgvProgram ylArgs):list

createArgvProgram :: [[Char]] -> Ast
createArgvProgram args = AstList ([ AstNode "def"
                                  , AstNode "argv"
                                  , AstList [AstNode "n"]
                                  ]
                                  ++ createArgsDefinition args 0
                                  ++ [createArgsBranches args 0])

createArgsDefinition :: [[Char]] -> Int -> [Ast]
createArgsDefinition [] _ = []
createArgsDefinition (arg:next) counter =
    let counterStr = show counter in
        (AstList [ AstNode "let"
                 , AstNode ("__arg" ++ counterStr ++ "__")
                 , AstNode arg
                 ]) : (createArgsDefinition next (counter + 1))

createArgsBranches ::  [[Char]] -> Int -> Ast
createArgsBranches [] _ = AstList []
createArgsBranches (arg:next) counter =
    let counterStr = show counter in
        AstList [ AstNode "if"
                , AstList [ AstNode "="
                          , AstNode "n"
                          , AstNode counterStr
                          ]
                , AstList [ AstNode ("__arg" ++ counterStr ++ "__") ]
                , AstList [createArgsBranches next (counter + 1)]
                ]

runProgram :: Ast -> IO ()
runProgram program = let Context {var=_, io=io, scope=_} = evaluateGlobal program in
    io

interativeMode :: Scope -> IO ()
interativeMode scope = do
    putStr "> "
    program <- getLine
    let Context {var=output, io=io, scope=newScope} = evaluate (parse program) scope False in
        do
            io
            print output
            interativeMode newScope
