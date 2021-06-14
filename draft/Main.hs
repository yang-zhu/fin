module Main where

import Lexer(tokenize)
import Parser(parseProgram)
import FCompiler(translateProgram)
import MF(Value, HeapCell(VAL), StackCell(HeapAddr), MachineState(..), runMF)

test :: String -> Value
test s = let
    ms = runMF $ translateProgram $ parseProgram $ tokenize s
    HeapAddr hCell = head $ stack ms
    VAL res = heap ms !! hCell
    in res

main = do
    input <- getLine 
    print (test input)