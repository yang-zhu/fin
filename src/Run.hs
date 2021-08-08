module Run where

import Data.Sequence (index)
import FCompiler (translateProgram)
import Lexer (tokenize)
import MF (HeapCell (VAL), MachineState (..), StackCell (HeapAddr), Value, runMF, Instruction)
import Parser (parseProgram)
import System.Environment (getArgs)
import Data.List (intercalate)
import Control.Monad (when)

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

showCode :: [Instruction] -> String
showCode instructions = intercalate "\n" (map (\(i, c) -> "c" ++ show i ++ ": " ++ c) codeWithIndices)
  where
    codeWithIndices :: [(Int, String)]
    codeWithIndices = zip [0..] (map show instructions)

traceMF :: [MachineState] -> String
traceMF [] = ""
traceMF [_] = ""
traceMF (m : ms) = ("I: " ++ show (code m !! pc m) ++ "\n" ++ show (head ms)) ++ traceMF ms

main :: IO ()
main =
  do
    args <- getArgs
    input <- multiline
    case parseProgram (tokenize input) of
      Right program ->
        do
          let ms = translateProgram program
          -- when the flag "-code" is enabled
          when ("-code" `elem` args) (putStrLn $ showCode (code ms))
          case runMF ms of
            Right machinestates ->
              do
                -- when the flag "-trace" is enabled
                when ("-trace" `elem` args) (putStr $ traceMF machinestates)
                let MachineState {stack, heap} = last machinestates
                let HeapAddr hCell = head stack
                let VAL res = heap `index` hCell
                putStrLn $ ">>> Result: " ++ show res
            Left err -> error $ "Runtime error: " ++ err
      Left err -> error $ "Syntax error: " ++ err