import System.Environment
import System.IO

import Compiler
import Parser

main = do
        args <- getArgs
        code <- getCode args
        let yl_ast = parse code
            c_ast = compile yl_ast
            c_code = turn_to_code c_ast in
                print c_code

getCode :: [[Char]] -> IO [Char]
getCode [] = error "No input"
getCode (file:_) = do
        handle <- openFile file ReadMode
        hGetContents handle
