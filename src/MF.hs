module MF (Value (..), Instruction (..), CodeAddr, HeapAddr, StackCell (..), Operator (..), HeapCell (..), MachineState (..), runMF) where

import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, index, update, (|>))
import Data.List (find, intercalate)
import Debug.Trace (trace)
import Parser (BinaryOp (..), UnaryOp (..))
import Data.Maybe (catMaybes)

instance Show Value where
  show (IntValue x) = show x
  show (BoolValue b) = show b

data Value = IntValue Integer | BoolValue Bool deriving Eq

data Instruction = Reset | Pushfun String | Pushval Value | Pushparam Int | Makeapp | Slide Int | Unwind | Call | Return | Pushpre Operator | Operator1 | Operator2 | OperatorIf | UpdateFun Int | UpdateOp | UpdateLet Int | Alloc | SlideLet Int | Halt deriving (Eq, Show)

type CodeAddr = Int

type HeapAddr = Int

data StackCell = CodeAddr CodeAddr | HeapAddr HeapAddr deriving (Show)

data Operator = UnaryOperator UnaryOp | BinaryOperator BinaryOp | IfOperator deriving (Eq, Show)

data HeapCell = DEF CodeAddr | VAL Value | APP HeapAddr HeapAddr | PRE Operator | IND HeapAddr | UNINIT deriving (Show)

data MachineState = MachineState {pc :: Int, code :: [Instruction], stack :: [StackCell], heap :: Seq HeapCell, global :: Map.Map String HeapAddr, codeRange :: [((Int, Int), String)]} deriving Show

type MFError = String

-- Extract all the stack cells with code addresses
extractCodeAddr :: [StackCell] -> [CodeAddr]
extractCodeAddr [] = []
extractCodeAddr (cell : cells) = case cell of
  CodeAddr c -> c : extractCodeAddr cells
  _ -> extractCodeAddr cells

-- Trace back the function, to which a code address belongs
tracebackFunc :: CodeAddr -> [((Int, Int), String)] -> Maybe String
tracebackFunc ca codeRange = 
  do
    (_, f) <- find (\((start, end), _) -> ca >= start && ca <= end) codeRange
    return f

-- Trace back all the functions in the current stack
tracebackFuncs :: MachineState -> [String]
tracebackFuncs MachineState {pc, stack, codeRange} =
  let
    cas = pc : extractCodeAddr stack
    funcs = [ tracebackFunc ca codeRange | ca <- cas]
   in catMaybes funcs

runMF :: MachineState -> Either MFError MachineState
runMF ms@MachineState {pc, code} =
  let i = code !! pc
   in if i == Halt
        then return ms
        else case execInstruction i ms {pc = pc + 1} of
          Right ms -> runMF ms
          Left err ->
            do
              let funcs = tracebackFuncs ms
              Left $ err ++ "\nTraceback (most recent call first): " ++ intercalate ", " funcs
        -- for debugging: else runMF $ trace (show (stack ms) ++ "\n" ++ show (heap ms) ++ "\n" ++ show i) (execInstruction i ms{pc=p+1})

-- Follow the IND cell to find the actual heap cell
value :: HeapAddr -> Seq HeapCell -> HeapCell
value addr1 heap = case heap `index` addr1 of
  IND addr2 -> value addr2 heap
  anything -> anything

-- Specify how each instruction is executed
execInstruction :: Instruction -> MachineState -> Either MFError MachineState
execInstruction Reset ms = return ms {stack = []}
-- find the heap address of the function in the global environment and push the address onto the stack
execInstruction (Pushfun f) ms@MachineState {stack, global} =
  case Map.lookup f global of
    Just funAddr -> return ms {stack = HeapAddr funAddr : stack}
    Nothing -> Left $ "Function " ++ f ++ " not found."
execInstruction (Pushval v) ms@MachineState {stack, heap} =
  return
    ms
      { -- (length h) is exactly the index of the newly added heap cell
        stack = HeapAddr (length heap) : stack,
        heap = heap |> VAL v
      }
execInstruction (Pushparam n) ms@MachineState {stack, heap} =
  case stack !! (n + 1) of
    HeapAddr appAddr -> case value appAddr heap of
      -- a parameter of a function is the right child of an APP-node
      APP _ arg2 -> return ms {stack = HeapAddr arg2 : stack}
      cell -> Left $ "Pushparam expected an APP-cell, but found " ++ show cell ++ "."
    CodeAddr ca -> Left $ "Pushparam expected a heap address, but found code address " ++ show ca ++ "."
execInstruction Makeapp ms@MachineState {stack = HeapAddr first : HeapAddr second : cells, heap} =
  return
    ms
      { stack = HeapAddr (length heap) : cells,
        heap = heap |> APP first second
      }
-- clean up the stack by removing the slots produced by Unwind, only the unevaluated body and the return address are kept
execInstruction (Slide n) ms@MachineState {stack = funBody : ra : cells} = return ms {stack = funBody : ra : drop n cells}
-- unfold the function graph until it hits a non-APP-node
execInstruction Unwind ms@MachineState {pc, stack = stack@(HeapAddr top : _), heap} =
  case value top heap of
    APP addr1 _ -> return ms {pc = pc - 1, stack = HeapAddr addr1 : stack}
    _ -> return ms
execInstruction Call ms@MachineState {pc, stack = stack@(HeapAddr top : _), heap} =
  case value top heap of
    DEF addr -> return ms {pc = addr, stack = CodeAddr pc : stack}
    PRE (UnaryOperator _) -> return ms {pc = 21, stack = CodeAddr pc : stack}
    PRE (BinaryOperator _) -> return ms {pc = 4, stack = CodeAddr pc : stack}
    PRE IfOperator -> return ms {pc = 13, stack = CodeAddr pc : stack}
    _ -> return ms
execInstruction Return ms@MachineState {stack = res : CodeAddr ra : cells} =
  return
    ms
      { pc = ra,
        stack = res : cells
      }
execInstruction (Pushpre op) ms@MachineState {stack, heap} =
  return
    ms
      { stack = HeapAddr (length heap) : stack,
        heap = heap |> PRE op
      }
execInstruction Operator1 ms@MachineState {stack = HeapAddr operand : ra : HeapAddr opAddr : cells, heap} =
  do
    v <- case value operand heap of
      VAL v -> Right v
      x -> Left $ "Expected a value, but found " ++ show x ++ "."
    evalOpExpr <- case (op, v) of
      (Not, BoolValue b) -> Right $ BoolValue (not b)
      (Neg, IntValue x) -> Right $ IntValue (negate x)
      (Not, _) -> Left $ "Expected a truth value, but found " ++ show v ++ "."
      (Neg, _) -> Left $ "Expected a number, but found " ++ show v ++ "."
    return
      ms
        { -- the top APP-node of the operator graph is kept temporarily for later UpdateOp, therefore only the PRE-cell is removed
          stack = HeapAddr (length heap) : ra : cells,
          heap = heap |> VAL evalOpExpr
        }
  where
    PRE (UnaryOperator op) = value opAddr heap
execInstruction Operator2 ms@MachineState {stack = HeapAddr sndOperand : HeapAddr fstOperand : ra : HeapAddr opAddr : cells, heap} =
  do
    -- v1 and v2 could either be intergers or truth values
    v1 <- case value fstOperand heap of
      VAL v1 -> Right v1
      x -> Left $ "Expected a value, but found " ++ show x ++ "."
    v2 <- case value sndOperand heap of
      VAL v2 -> Right v2
      x -> Left $ "Expected a value, but found " ++ show x ++ "."
    evalOpExpr <- case (op, v1, v2) of
      (Equal, IntValue x1, IntValue x2) -> Right $ BoolValue (x1 == x2)
      (Equal, BoolValue b1, BoolValue b2) -> Right $ BoolValue (b1 == b2)
      (Equal, _, _) -> Right $ BoolValue False -- allow equality check between values of different types
      (Smaller, IntValue x1, IntValue x2) -> Right $ BoolValue (x1 < x2)
      (Smaller, _, _) -> Left $ "Expected two numbers, but found " ++ show v1 ++ " < " ++ show v2 ++ "."
      (Plus, IntValue x1, IntValue x2) -> Right $ IntValue (x1 + x2)
      (Plus, _, _) -> Left $ "Expected two numbers, but found " ++ show v1 ++ " + " ++ show v2 ++ "."
      (Minus, IntValue x1, IntValue x2) -> Right $ IntValue (x1 - x2)
      (Minus, _, _) -> Left $ "Expected two numbers, but found " ++ show v1 ++ " - " ++ show v2 ++ "."
      (Times, IntValue x1, IntValue x2) -> Right $ IntValue (x1 * x2)
      (Times, _, _) -> Left $ "Expected two numbers, but found " ++ show v1 ++ " * " ++ show v2 ++ "."
      (Divide, IntValue x1, IntValue x2) -> Right $ IntValue (x1 `div` x2)
      (Divide, _, _) -> Left $ "Expected two numbers, but found " ++ show v1 ++ " / " ++ show v2 ++ "."
    return
      ms
        { -- the top APP-node of the operator graph is kept temporarily for later UpdateOp, therefore only the PRE-cell and the lower APP-node are removed
          stack = HeapAddr (length heap) : ra : drop 1 cells,
          heap = heap |> VAL evalOpExpr
        }
  where
    PRE (BinaryOperator op) = value opAddr heap
execInstruction OperatorIf ms@MachineState {stack = HeapAddr condition : ra : _ : _ : HeapAddr thenAddr : HeapAddr elseAddr : cells, heap} =
  do
    cond <- case value condition heap of
      VAL (BoolValue cond) -> Right cond
      x -> Left $ "Expected a truth value, but found " ++ show x ++ "."
    return
      ms
        { -- the top APP-node of the operator graph is kept temporarily for later UpdateOp, therefore only the PRE-cell and the lower two APP-nodes are removed
          stack =
            if cond
              then HeapAddr thenBranch : ra : HeapAddr elseAddr : cells
              else HeapAddr elseBranch : ra : HeapAddr elseAddr : cells
        }
  where
    APP _ thenBranch = value thenAddr heap
    APP _ elseBranch = value elseAddr heap
-- convert the heap cell that contains the top APP-node of a function application graph into an IND-cell that points at the the result of the function application -> lazy evaluation
execInstruction (UpdateFun n) ms@MachineState {stack = HeapAddr funBody : stack, heap} =
  case stack !! (n + 1) of -- the heap cell of the top APP-node
    HeapAddr replaced ->
      return
        ms
          { heap = update replaced (IND funBody) heap
          }
    CodeAddr ca -> Left $ "Expected a heap address, but found code address " ++ show ca ++ "."
-- replace the heap cell that contains the top APP-node with the result of the operator, top of the stack points to the updated APP-node heap cell
-- attention: lower app nodes are already cleaned up by the Operator instruction, different from normal functions that rely on Slide to clean up
execInstruction UpdateOp ms@MachineState {stack = HeapAddr res : ra : HeapAddr replaced : cells, heap} =
  return
    ms
      { stack = HeapAddr replaced : ra : cells,
        heap = update replaced (heap `index` res) heap
      }
-- replace the right child of the dummy APP-node with its expression graph and pop the expression node from the stack
execInstruction (UpdateLet n) ms@MachineState {stack = HeapAddr res : cells, heap} =
  do
    appAddr <- case cells !! n of -- the heap cell of the right hand side expression
      HeapAddr appAddr -> Right appAddr
      CodeAddr ca -> Left $ "Expected a heap address, but found code address " ++ show ca ++ "."
    replaced <- case value appAddr heap of
      APP _ replaced -> Right replaced
      cell -> Left $ "Expected an APP-cell, but found " ++ show cell ++ "."
    return
      ms
        { stack = cells,
          heap = update replaced (IND res) heap
        }
-- allocate UNINIT-cell in the heap and push its address onto the stack
execInstruction Alloc ms@MachineState {stack, heap} =
  return
    ms
      { stack = HeapAddr (length heap) : stack,
        heap = heap |> UNINIT
      }
-- clean up dummy APP-nodes for local definitions on the stack
execInstruction (SlideLet n) ms@MachineState {stack = res : cells} =
  return ms {stack = res : drop n cells}
execInstruction i _ = Left $ "Cannot execute " ++ show i ++ "."