module Run where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Foldable (toList)
import Data.Tuple (swap)
import System.Environment (getArgs)
import Control.Monad (when)
import System.Console.Pretty (Color(..), Style(..), color, style)
import System.Exit (exitFailure)
import Lexer
import Parser
import FCompiler
import MF

data Options =
  Options {
    lexOpt :: Bool,
    parseOpt :: Bool,
    codeOpt :: Bool,
    stepOpt :: Bool,
    traceOpt :: Bool
  }

run :: String -> Value
run s = case tokenize s of
  Right tokens -> case parseProgram tokens of
    Right program -> case runMF $ translateProgram program of
      Right machinestates ->
        let MachineState {stack, heap} = last machinestates
            HeapAddr hCell = head stack
            VAL res = heap `Seq.index` hCell
         in res
      Left (err, _) -> error $ "Runtime error: " ++ err
    Left err -> error $ "Syntax error: " ++ err
  Left err -> error $ "Lexical error: " ++ err

-- Take in multi-line input until empty line
multiline :: IO String
multiline = do
  s <- getLine
  case s of
    "" -> return s
    _ -> ((s ++ "\n") ++) <$> multiline

-- Display MF code
showCode :: MachineState -> String
showCode MachineState {code, codeRange} = List.intercalate "\n" (map showInstruction codeWithAddrs)
  where
    codeWithAddrs :: [(CodeAddr, Instruction)]
    codeWithAddrs = zip [0 ..] (toList code)

    -- mapping from the index of the first instruction of a function to the name of the function
    codeBeginToFunc :: Map.Map CodeAddr String
    codeBeginToFunc = Map.fromList ([(begin, f) | ((begin, _), f) <- codeRange] ++ [(4, "binary operator"), (13, "if"), (21, "unary operator")])

    -- insert new lines between functions and add the function names
    showInstruction :: (CodeAddr, Instruction) -> String
    showInstruction (ca, instruction) = case Map.lookup ca codeBeginToFunc of
      Just f -> "\n* " ++ f ++ " *\n" ++ padString 6 ("c" ++ show ca ++ ":") ++ show instruction
      Nothing -> padString 6 ("c" ++ show ca ++ ":") ++ show instruction

-- Display stack cells
showStack :: MachineState -> [String]
showStack MachineState {stack, codeRange} = map showStackCell cellWithAddrs
  where
    cellWithAddrs :: [(Int, StackCell)]
    cellWithAddrs = zip [0 ..] stack

    -- add function names to stack cells that contain code addresses
    showStackCell :: (Int, StackCell) -> String
    showStackCell (i, CodeAddr ca) = case tracebackFunc ca codeRange of
      Just f -> padString 5 ("s" ++ show i ++ ":") ++ show (CodeAddr ca) ++ " (" ++ f ++ ")"
      Nothing -> padString 5 ("s" ++ show i ++ ":") ++ show (CodeAddr ca)
    showStackCell (i, cell) = padString 5 ("s" ++ show i ++ ":") ++ show cell

-- Reverse key and value in a map
reverseMap :: (Ord a, Ord b) => Map.Map a b -> Map.Map b a
reverseMap = Map.fromList . map swap . Map.toList

-- Display heap cells
showHeap :: MachineState -> [String]
showHeap MachineState {heap, global} = map showHeapCell cellWithAddrs
  where
    cellWithAddrs :: [(HeapAddr, HeapCell)]
    cellWithAddrs = zip [0 ..] (toList heap)

    reversedGlobal = reverseMap global  -- (heap address : function)
    -- add function names to heap cells that the global environment points to
    showHeapCell :: (HeapAddr, HeapCell) -> String
    showHeapCell (ha, cell) = case Map.lookup ha reversedGlobal of
      Just f -> padString 5 ("h" ++ show ha ++ ":") ++ show cell ++ " <-- " ++ f
      Nothing -> padString 5 ("h" ++ show ha ++ ":") ++ show cell

-- Pad the shorter block with empty lines, so the two blocks have the same length
padBlock :: ([String], [String]) -> ([String], [String])
padBlock (block1, block2) =
  ( block1 ++ replicate (longerBlock - length block1) "",
    block2 ++ replicate (longerBlock - length block2) ""
  )
  where
    longerBlock = max (length block1) (length block2)

padString :: Int -> String -> String
padString len s = s ++ replicate (len - length s) ' '

-- Pad the shorter strings with spaces, so all the lines have the same length, which is max(minLen, the length of the longest line)
padStrings :: Int -> [String] -> [String]
padStrings minLen strings = map (padString len) strings
  where
    len = max (maximum $ map length strings) minLen

-- Merge two blocks into one rectangular block
mergeBlocks :: [String] -> [String] -> [String]
mergeBlocks block1 block2 = zipWith (\s1 s2 -> s1 ++ " " ++ s2) paddedBlock1 paddedBlock2
  where
    (block1', block2') = padBlock (block1, block2)
    paddedBlock1 = padStrings 25 block1'
    paddedBlock2 = padStrings 25 block2'

-- Display the execution trace as in the script
traceMF :: [MachineState] -> String
traceMF [] = ""
traceMF [_] = ""
traceMF (m1 : m2 : ms) =
  let mergeSH = mergeBlocks (showStack m2) (showHeap m2)
      mergeAll = mergeBlocks ["I: " ++ show (code m1 `Seq.index` pc m1), "P: c" ++ show (pc m2)] mergeSH
   in List.intercalate "\n" mergeAll ++ "\n\n" ++ traceMF (m2 : ms)

-- Make a title bold and add a frame around it
titleStyling :: String -> String
titleStyling s = "+" ++ replicate (length s + 2) '-' ++ "+\n" ++
                 "| " ++ style Bold s ++ " |\n" ++
                 "+" ++ replicate (length s + 2) '-' ++ "+\n"

-- Check if the command line arguments are valid. If not, give help information.
checkArgs :: [String] -> IO ()
checkArgs [] = return ()
checkArgs (arg : args)
  | arg `elem` flags = checkArgs args
  | otherwise = do
    putStrLn $ color Red ("Invalid option " ++ show arg) ++ "\n"
               ++ "Possible options: " ++ List.intercalate ", " flags
    exitFailure
    where
      flags = ["-lex", "-parse", "-code", "-step", "-trace"]

-- The logo "Fin" in ASCII art
asciiLogo :: String
asciiLogo = "       _____  _\n" ++
            "      |  ___|(_) _ __\n" ++
            "      | |_   | || '_ \\\n" ++
            "      |  _|  | || | | |\n" ++
            color Blue "~~~~~~" ++ "|_|" ++ color Blue "~~~~" ++ "|_||_| |_|" ++ color Blue "~~~~~~" ++ "\n" ++
            color Blue "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

runFin :: Options -> String -> String
runFin Options {lexOpt, parseOpt, codeOpt, stepOpt, traceOpt} input =
  case tokenize input of
    Right tokens ->
      (if lexOpt then titleStyling "Tokens" ++ List.intercalate "\n" (map show tokens) ++ "\n\n" else "")
        ++ case parseProgram tokens of
          Right program ->
            (if parseOpt then titleStyling "Parse Result" ++ List.intercalate "\n" (map show program) ++ "\n\n" else "")
              ++ let ms = translateProgram program
                  in (if codeOpt then titleStyling "Instructions" ++ showCode ms ++ "\n\n" else "")
                       ++ case runMF ms of
                         Right machinestates ->
                           (if stepOpt then titleStyling "Step Count" ++ "Number of execution steps: " ++ show (length machinestates) ++ "\n\n" else "") ++ (if traceOpt then titleStyling "Execution Trace" ++ traceMF machinestates else "")
                             ++ let MachineState {stack, heap} = last machinestates
                                    HeapAddr hCell = head stack
                                    VAL res = value hCell heap
                                 in ">>> Result: " ++ show res
                         Left (err, machinestates) -> (if traceOpt then traceMF machinestates else "") ++ color Red "Runtime error: " ++ err
          Left err -> color Red "Syntax error: " ++ err
    Left err -> color Red "Lexical error: " ++ err

main :: IO ()
main =
  do
    args <- getArgs
    checkArgs args
    putStrLn asciiLogo
    putStrLn $ style Bold "Please enter the F program here" ++ " (end with an empty line)" ++ style Bold ":"
    input <- multiline
    putStrLn $ runFin Options{lexOpt="-lex" `elem` args,
                              parseOpt="-parse" `elem` args,
                              codeOpt="-code" `elem` args,
                              stepOpt="-step" `elem` args,
                              traceOpt="-trace" `elem` args} input
