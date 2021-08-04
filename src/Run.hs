module Run where

import Lexer(tokenize)
import Parser(parseProgram)
import FCompiler(translateProgram)
import MF(Value, HeapCell(VAL), StackCell(HeapAddr), MachineState(..), runMF)
import Data.Sequence (index)

test :: String -> Value
test s = case parseProgram (tokenize s) of
  Right program ->
    let MachineState {stack = stack, heap = heap} = runMF $ translateProgram program
        HeapAddr hCell = head stack
        VAL res = heap `index` hCell
     in res
  Left message -> error message

main :: IO ()
main = do
  input <- getLine
  print (test input)