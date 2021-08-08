module Run where

import Data.Sequence (index)
import FCompiler (translateProgram)
import Lexer (tokenize)
import MF (HeapCell (VAL), MachineState (..), StackCell (HeapAddr), Value, runMF)
import Parser (parseProgram)

run :: String -> Value
run s = case parseProgram (tokenize s) of
  Right program -> case runMF $ translateProgram program of
    Right machinestates ->
      let 
        MachineState {stack, heap} = last machinestates
        HeapAddr hCell = head stack
        VAL res = heap `index` hCell
       in res
    Left err -> error $ "Runtime error: " ++ err
  Left err -> error $ "Syntax error: " ++ err

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
    putStrLn $ "==================\n" ++ "Result: " ++ show (run input)