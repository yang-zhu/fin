module Run where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Foldable (toList)
import Data.Tuple (swap)
import Lexer
import Parser
import FCompiler
import MF

data Options = Options
  { lexOpt :: Bool,
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
            hCell = head stack
            VAL res = heap `Seq.index` hCell
         in res
      Left (err, _) -> error $ "Runtime error: " ++ err
    Left err -> error $ "Syntax error: " ++ err
  Left err -> error $ "Lexical error: " ++ err

showCode :: MachineState -> String
showCode MachineState {code, codeRange} = List.intercalate "\n" (map showInstruction codeWithAddrs)
  where
    codeWithAddrs :: [(CodeAddr, Instruction)]
    codeWithAddrs = zip [0 ..] (toList code)

    codeBeginToFunc :: Map.Map CodeAddr String
    codeBeginToFunc = Map.fromList ([(begin, f) | ((begin, _), f) <- codeRange] ++ [(4, "binary operator"), (13, "if"), (21, "unary operator")])

    showInstruction :: (CodeAddr, Instruction) -> String
    showInstruction (ca, instruction) = case Map.lookup ca codeBeginToFunc of
      Just f -> "\n* " ++ f ++ " *\n" ++ padString 6 ("c" ++ show ca ++ ":") ++ show instruction
      Nothing -> padString 6 ("c" ++ show ca ++ ":") ++ show instruction

showStack :: MachineState -> [String]
showStack MachineState {stack} = map showStackCell cellWithAddrs
  where
    cellWithAddrs :: [(Int, HeapAddr)]
    cellWithAddrs = zip [0 ..] stack

    showStackCell :: (Int, HeapAddr) -> String
    showStackCell (i, cell) = padString 5 ("s" ++ show i ++ ":") ++ "h" ++ show cell
    -- showStackCell (i, ha) = padString 5 ("s" ++ show i ++ ":") ++ "h" ++ show ha ++ if ha < Seq.length heap then " (" ++ show (value ha heap) ++ ")" else "(INVALID ADDRESS)"

showReturnStack :: MachineState -> [String]
showReturnStack MachineState {returnStack, codeRange} = map showReturnStackCell returnStack
  where
    showReturnStackCell :: CodeAddr -> String
    showReturnStackCell ca = case tracebackFunc ca codeRange of
      Just f -> "c" ++ show ca ++ " (" ++ f ++ ")"
      Nothing -> "c" ++ show ca 

reverseMap :: (Ord a, Ord b) => Map.Map a b -> Map.Map b a
reverseMap = Map.fromList . map swap . Map.toList

showHeap :: MachineState -> [String]
showHeap MachineState {heap, global} = map showHeapCell cellWithAddrs
  where
    cellWithAddrs :: [(HeapAddr, HeapCell)]
    cellWithAddrs = zip [0 ..] (toList heap)

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
mergeBlocks block1 block2 = zipWith (\s1 s2 -> s1 ++ "    " ++ s2) paddedBlock1 paddedBlock2
  where
    (block1', block2') = padBlock (block1, block2)
    paddedBlock1 = padStrings 25 block1'
    paddedBlock2 = padStrings 25 block2'

traceMF :: [MachineState] -> String
traceMF [] = ""
traceMF [_] = ""
traceMF (m1 : m2 : ms) =
  let mergeStacks = mergeBlocks (showStack m2) (showReturnStack m2)
      mergeSH = mergeBlocks mergeStacks (showHeap m2)
      mergeAll = mergeBlocks ["I: " ++ show (code m1 `Seq.index` pc m1), "P: c" ++ show (pc m2)] mergeSH
   in List.intercalate "\n" mergeAll ++ "\n\n" ++ traceMF (m2 : ms)
   -- "\nFree List: " ++ show (freeList m2) ++ "\nReachable: " ++ show (reachable m2) ++

titleStyling :: String -> String
titleStyling s = "+" ++ replicate (length s + 2) '-' ++ "+\n" ++
                 "| " ++ s ++ " |\n" ++
                 "+" ++ replicate (length s + 2) '-' ++ "+\n"

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
                                    hCell = head stack
                                    VAL res = heap `Seq.index` hCell
                                 in ">>> Result: " ++ show res
                         Left (err, machinestates) -> (if traceOpt then traceMF machinestates else "") ++ "Runtime error: " ++ err
          Left err -> "Syntax error: " ++ err
    Left err -> "Lexical error: " ++ err