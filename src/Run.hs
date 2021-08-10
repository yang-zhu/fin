module Run where

import Data.Sequence (index)
import FCompiler (translateProgram)
import Lexer (tokenize)
import MF (HeapCell (VAL), MachineState (..), StackCell (HeapAddr), Value, showCode, showStack, showHeap, mergeBlocks, runMF)
import Parser (parseProgram)
import System.Environment (getArgs)
import Data.List (intercalate)
import Control.Monad (when)

run :: String -> Value
run s = 
  case tokenize s of
    Right tokens -> case parseProgram tokens of
      Right program -> case runMF $ translateProgram program of
        Right machinestates ->
          let 
            MachineState {stack, heap} = last machinestates
            HeapAddr hCell = head stack
            VAL res = heap `index` hCell
          in res
        Left (err, _) -> error $ "Runtime error: " ++ err
      Left err -> error $ "Syntax error: " ++ err
    Left err -> error $ "Lexical error: " ++ err

-- Take in multi-line input until empty line
multiline :: IO String
multiline =
  do
    s <- getLine
    case s of
      "" -> return s
      _ -> fmap (s ++) multiline

traceMF :: [MachineState] -> String
traceMF [] = ""
traceMF [_] = ""
traceMF (m1 : m2 : ms) =
  let
    mergeSH = mergeBlocks (showStack m2) (showHeap m2)
    mergeAll = mergeBlocks ["I: " ++ show (code m1 !! pc m1), "P: c" ++ show (pc m2)] mergeSH
  in intercalate "\n" mergeAll ++ "\n\n" ++ traceMF (m2 : ms)

titleStyling :: String -> String
titleStyling s = "+" ++ replicate (length s + 2) '-' ++ "+\n" ++
                 "| " ++ s ++ " |\n" ++
                 "+" ++ replicate (length s + 2) '-' ++ "+\n"

main :: IO ()
main =
  do
    args <- getArgs
    input <- multiline
    case tokenize input of
      Right tokens -> 
        do
          when ("-lex" `elem` args) (putStrLn $ titleStyling "Tokens" ++ intercalate "\n" (map show tokens) ++ "\n")
          case parseProgram tokens of
            Right program ->
              do
                -- when the flag "-parse" is enabled
                when ("-parse" `elem` args) (putStrLn $ titleStyling "Parse Result" ++ intercalate "\n" (map show program) ++ "\n")
                let ms = translateProgram program
                -- when the flag "-code" is enabled
                when ("-code" `elem` args) (putStrLn $ titleStyling "Instructions" ++ showCode (code ms) ++ "\n")
                case runMF ms of
                  Right machinestates ->
                    do
                      -- when the flag "-trace" is enabled
                      when ("-trace" `elem` args) (putStr $ titleStyling "Execution Trace" ++ traceMF machinestates)
                      let MachineState {stack, heap} = last machinestates
                      let HeapAddr hCell = head stack
                      let VAL res = heap `index` hCell
                      putStrLn $ ">>> Result: " ++ show res
                  Left (err, machinestates) -> 
                    do
                      -- still print the trace when there is an error
                      when ("-trace" `elem` args) (putStr $ traceMF machinestates)
                      putStrLn $ "Runtime error: " ++ err
            Left err -> putStrLn $ "Syntax error: " ++ err
      Left err -> putStrLn $ "Lexical error: " ++ err