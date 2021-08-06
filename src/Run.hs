module Run where

import Lexer(tokenize)
import Parser(parseProgram)
import FCompiler(translateProgram)
import MF(Value, HeapCell(VAL), StackCell(HeapAddr), MachineState(..), runMF)
import Data.Sequence (index)

test :: String -> Value
test s = case parseProgram (tokenize s) of
  Right program -> case runMF $ translateProgram program of
    Right MachineState {stack = stack, heap = heap} ->
      let HeapAddr hCell = head stack
          VAL res = heap `index` hCell
       in res
    Left message -> error message
  Left message -> error message

main :: IO ()
main = do
  input <- getLine
  print (test input)