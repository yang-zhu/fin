module Main where

import Lexer(tokenize)
import Parser(parseProgram)
import FCompiler(translateProgram)
import MF(Value, HeapCell(VAL), StackCell(HeapAddr), MachineState(..), runMF)
import Data.Sequence ( index )

test :: String -> Value
test s = let
    ms@MachineState{stack=stack, heap=heap} = runMF $ translateProgram $ parseProgram $ tokenize s
    HeapAddr hCell = head $ stack
    VAL res = heap `index` hCell
    in res

main = do
    input <- getLine 
    print (test input)