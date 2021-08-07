module Run where

import Data.Sequence (index)
import FCompiler (translateProgram)
import Lexer (tokenize)
import MF (HeapCell (VAL), MachineState (..), StackCell (HeapAddr), Value, runMF)
import Parser (parseProgram)

test :: String -> Value
test s = case parseProgram (tokenize s) of
  Right program -> case runMF $ translateProgram program of
    Right MachineState {stack = stack, heap = heap} ->
      let HeapAddr hCell = head stack
          VAL res = heap `index` hCell
       in res
    Left message -> error $ "Runtime error: " ++ message
  Left message -> error $ "Syntax error: " ++ message

-- Take in multi-line input until empty line
multiline :: IO String
multiline =
  do
    s <- getLine
    case s of
      "" -> return s
      _ -> fmap (s ++) multiline

main :: IO ()
main =
  do
    input <- multiline
    putStrLn $ "==================\n" ++ "Result: " ++ show (test input)