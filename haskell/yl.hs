import Control.Monad

import System.Environment
import System.IO

import Interpreter
import Parser

main = do
    args <- getArgs
    program <- getProgram args
    putStr $ runProgram program

-- Interactive mode
getProgram [] = forever $ do
  hSetBuffering stdout NoBuffering
  putStr "> "
  program <- getLine
  putStrLn $ runProgram program
getProgram ("-e":program:[]) = return program
getProgram (file:[]) = do
    handle <- openFile file ReadMode
    hGetContents handle

runProgram = evaluate . parse
