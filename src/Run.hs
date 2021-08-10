module Run where

import qualified Data.Map.Strict as Map
import Data.Sequence (index)
import Data.Foldable (toList)
import Data.Tuple (swap)
import System.Environment (getArgs)
import Data.List (intercalate)
import Control.Monad (when)
import Lexer
import Parser
import FCompiler
import MF

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

showCode :: [Instruction] -> String
showCode instructions = intercalate "\n" (map (\(i, c) -> padString 6 ("c" ++ show i ++ ":") ++ show c) codeWithAddrs)
  where
    codeWithAddrs :: [(CodeAddr, Instruction)]
    codeWithAddrs = zip [0..] instructions

showStack :: MachineState -> [String]
showStack MachineState {stack, codeRange} = map showStackCell cellWithAddrs
  where
    cellWithAddrs :: [(Int, StackCell)]
    cellWithAddrs = zip [0..] stack

    showStackCell :: (Int, StackCell) -> String
    showStackCell (i, CodeAddr ca) = case tracebackFunc ca codeRange of
      Just f -> padString 5 ("s" ++ show i ++ ":") ++ show (CodeAddr ca)  ++ " (" ++ f ++ ")"
      Nothing -> padString 5 ("s" ++ show i ++ ":") ++ show (CodeAddr ca)
    showStackCell (i, cell) = padString 5 ("s" ++ show i ++ ":") ++ show cell

reverseMap :: (Ord a, Ord b) => Map.Map a b -> Map.Map b a
reverseMap m = Map.fromList $ map swap (Map.toList m)

showHeap :: MachineState -> [String]
showHeap MachineState {heap, global} = map showHeapCell cellWithAddrs
  where
    cellWithAddrs :: [(HeapAddr, HeapCell)]
    cellWithAddrs = zip [0..] (toList heap)

    reversedGlobal = reverseMap global
    showHeapCell :: (HeapAddr, HeapCell) -> String
    showHeapCell (ha, cell) = case Map.lookup ha reversedGlobal of
      Just f -> padString 5 ("h" ++ show ha ++ ":") ++ show cell ++ " <-- " ++ f
      Nothing -> padString 5 ("h" ++ show ha ++ ":") ++ show cell

padBlock :: ([String], [String]) -> ([String], [String])
padBlock (block1, block2) =
  ( block1 ++ replicate (longerBlock - length block1) "",
    block2 ++ replicate (longerBlock - length block2) ""
  )
  where
    longerBlock = max (length block1) (length block2)

padString :: Int -> String -> String
padString len s = s ++ replicate (len - length s) ' '

padStrings :: Int -> [String] -> [String]
padStrings minLen strings = map (padString len) strings
  where
    len = max (maximum $ map length strings) minLen

mergeBlocks :: [String] -> [String] -> [String]
mergeBlocks block1 block2 = zipWith (\s1 s2 -> s1 ++ " " ++ s2) paddedBlock1 paddedBlock2
  where
    (block1', block2') = padBlock (block1, block2)
    paddedBlock1 = padStrings 25 block1'
    paddedBlock2 = padStrings 25 block2'

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