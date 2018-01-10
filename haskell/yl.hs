import Control.Monad

import System.Environment
import System.IO

import Interpreter
import Parser

main = do
    args <- getArgs
    program <- getProgram args
    runProgram program

-- Interactive mode
getProgram [] = do
    hSetBuffering stdout NoBuffering
    interativeMode globalScope
    error "Interactive mode is a loop!"
getProgram ("-e":program:[]) = return program
getProgram (file:[]) = do
    handle <- openFile file ReadMode
    hGetContents handle

runProgram :: [Char] -> IO ()
runProgram program = let Context {var=_, io=io, scope=_} = evaluateGlobal $ parse program in
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
