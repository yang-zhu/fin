module MF where

import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Parser

(!?) :: [a] -> Int -> Maybe a
l !? i
  | i < length l = Just (l !! i)
  | otherwise = Nothing

data Value
  = IntValue Integer
  | BoolValue Bool
  deriving (Eq)

data Instruction
  = Reset
  | Pushfun String
  | Pushval Value
  | Pushparam Int
  | Makeapp
  | Slide Int
  | Unwind
  | Call
  | Return
  | Pushpre Operator
  | Operator1
  | Operator2
  | OperatorIf
  | Update
  | UpdateLet Int
  | Alloc
  | Halt
  deriving (Eq, Show)

type CodeAddr = Int

type HeapAddr = Int

data Operator
  = UnOp UnaryOp
  | BinOp BinaryOp
  | IfOp
  deriving (Eq, Show)

data HeapCell
  = DEF CodeAddr
  | VAL Value
  | APP HeapAddr HeapAddr
  | PRE Operator
  | UNINIT

data MachineState = MachineState
  { pc :: CodeAddr,
    code :: Seq Instruction,
    stack :: [HeapAddr],
    returnStack :: [CodeAddr],
    heap :: Seq HeapCell,
    global :: Map.Map String HeapAddr,
    codeRange :: [((Int, Int), String)],
    freeList :: [HeapAddr],
    gcInterval :: Int
  }

type MFError = String

instance Show Value where
  show (IntValue x) = show x
  show (BoolValue b) = show b

instance Show HeapCell where
  show (DEF ca) = "DEF c" ++ show ca
  show (VAL (IntValue x)) = "VAL Integer " ++ show x
  show (VAL (BoolValue b)) = "VAL Bool " ++ show b
  show (APP ha1 ha2) = "APP h" ++ show ha1 ++ " h" ++ show ha2
  show (PRE op) = "PRE (" ++ show op ++ ")"
  show UNINIT = "UNINITIALIZED"

tracebackFunc :: CodeAddr -> [((Int, Int), String)] -> Maybe String
tracebackFunc ca codeRange = do
  (_, f) <- List.find (\((start, end), _) -> ca >= start && ca <= end) codeRange
  return f

-- Trace back all the functions in the current stack
tracebackFuncs :: MachineState -> [String]
tracebackFuncs MachineState {pc, returnStack, codeRange} =
  let funcs = [tracebackFunc ca codeRange | ca <- pc:returnStack]
   in catMaybes funcs

followPointer :: HeapAddr -> Seq HeapCell -> Set HeapAddr -> Set HeapAddr
followPointer ha heap reachable =
  if Set.notMember ha reachable
    then case heap `Seq.index` ha of
      (APP h1 h2) -> followPointer h2 heap (followPointer h1 heap (Set.insert ha reachable))
      _ -> Set.insert ha reachable
    else reachable

reachableCells :: [HeapAddr] -> Seq HeapCell -> Set HeapAddr -> Set HeapAddr
reachableCells [] _ reachable = reachable
reachableCells (ha : has) heap reachable = reachableCells has heap (followPointer ha heap reachable)

collectGarbage :: MachineState -> MachineState
collectGarbage ms@MachineState {global, stack, heap, gcInterval} =
  let reachable = reachableCells (stack ++ Map.elems global) heap Set.empty
      freeList = filter (`Set.notMember` reachable) [0 .. Seq.length heap - 1]
   in ms {freeList = freeList, gcInterval = gcInterval * 2}

runMF :: MachineState -> Either (MFError, [MachineState]) [MachineState]
runMF ms@MachineState {pc, code} =
  let i = code `Seq.index` pc
   in if i == Halt
        then return [ms]
        else case execInstruction i ms {pc = pc + 1} of
          Right ms' -> case runMF ms' of
            Right mss -> Right (ms : mss)
            Left (err, mss) -> Left (err, ms : mss)
          Left err -> do
            let funcs = tracebackFuncs ms
            Left (err ++ "\nTraceback (most recent call first): " ++ List.intercalate ", " funcs, [ms])

allocate :: MachineState -> HeapCell -> MachineState
allocate ms@MachineState {stack, heap, freeList, gcInterval} cell =
  if null freeList
    then if length heap == gcInterval
      then allocate (collectGarbage ms) cell
      else ms {stack = length heap : stack, heap = heap |> cell}
    else ms {stack = head freeList : stack, heap = Seq.update (head freeList) cell heap, freeList = tail freeList}

-- Specify how each instruction is executed
execInstruction :: Instruction -> MachineState -> Either MFError MachineState
execInstruction Reset ms = return ms {stack = []}
-- find the heap address of the function in the global environment and push the address onto the stack
execInstruction (Pushfun f) ms@MachineState {stack, global} =
  case Map.lookup f global of
    Just funAddr -> return ms {stack = funAddr : stack}
    Nothing -> Left $ "Function " ++ f ++ " not found."
execInstruction (Pushval v) ms = return $ allocate ms (VAL v)
execInstruction (Pushparam n) ms@MachineState {stack, heap} =
  case stack !? n of
    Just appAddr -> case heap `Seq.index` appAddr of
      -- a parameter of a function is the right child of an APP-node
      APP _ arg2 -> return ms {stack = arg2 : stack}
      cell -> Left $ "Pushparam expected an APP-cell, but found " ++ show cell ++ "."
    Nothing -> Left "Stack access out of bounds."
execInstruction Makeapp ms@MachineState {stack = first : second : _} =
  let
    -- !!! First allocate, then delete the first and second cells, otherwise the heap cells at first and second and the cells they point to won't be taken into account by the garbarge collector. 
    ms' = allocate ms (APP first second)
    (appCell : cells) = stack ms'
   in return ms' {stack = appCell : drop 2 cells}
-- clean up the stack by removing the slots produced by Unwind
execInstruction (Slide n) ms@MachineState {stack = funBody : cells} =
  return ms {stack = funBody : drop n cells}
-- unfold the function graph until it hits a non-APP-node
execInstruction Unwind ms@MachineState {pc, stack = stack@(top : _), heap} =
  case heap `Seq.index` top of
    APP addr1 _ -> return ms {pc = pc - 1, stack = addr1 : stack}
    _ -> return ms
execInstruction Call ms@MachineState {pc, stack = top : _, returnStack, heap} =
  case heap `Seq.index` top of
    DEF addr -> return ms {pc = addr, returnStack = pc : returnStack}
    PRE (UnOp _) -> return ms {pc = 4, returnStack = pc : returnStack}
    PRE (BinOp _) -> return ms {pc = 11, returnStack = pc : returnStack}
    PRE IfOp -> return ms {pc = 21, returnStack = pc : returnStack}
    _ -> return ms
execInstruction Return ms@MachineState {returnStack} =
  return ms
    { pc = head returnStack,
      returnStack = tail returnStack
    }
execInstruction (Pushpre op) ms = return $ allocate ms (PRE op)
execInstruction Operator1 ms@MachineState {stack = operand : opAddr : cells, heap} = do
  v <- case heap `Seq.index` operand of
    VAL v -> Right v
    x -> Left $ "Expected a value, but found " ++ show x ++ "."
  evalOpExpr <- case (op, v) of
    (Not, BoolValue b) -> Right $ BoolValue (not b)
    (Neg, IntValue x) -> Right $ IntValue (negate x)
    (Not, _) -> Left $ "Expected a truth value, but found " ++ show v ++ "."
    (Neg, _) -> Left $ "Expected a number, but found " ++ show v ++ "."
  return $ allocate ms {stack = opAddr : cells} (VAL evalOpExpr)
  where
    PRE (UnOp op) = heap `Seq.index` opAddr
execInstruction Operator2 ms@MachineState {stack = sndOperand : fstOperand : opAddr : cells, heap} = do
  -- v1 and v2 could either be intergers or truth values
  v1 <- case heap `Seq.index` fstOperand of
    VAL v1 -> Right v1
    x -> Left $ "Expected a value, but found " ++ show x ++ "."
  v2 <- case heap `Seq.index` sndOperand of
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
    (Divide, IntValue x1, IntValue x2) ->
      if x2 /= 0
        then Right $ IntValue (x1 `div` x2)
        else Left "Division by 0."
    (Divide, _, _) -> Left $ "Expected two numbers, but found " ++ show v1 ++ " / " ++ show v2 ++ "."
  return $ allocate ms {stack = opAddr : cells} (VAL evalOpExpr)
  where
    PRE (BinOp op) = heap `Seq.index` opAddr
execInstruction OperatorIf ms@MachineState {stack = condition : cells@(_ : _ : thenAddr : elseAddr : _), heap} = do
  cond <- case heap `Seq.index` condition of
    VAL (BoolValue cond) -> Right cond
    x -> Left $ "Expected a truth value, but found " ++ show x ++ "."
  return ms
    { stack =
        if cond
          then thenBranch : cells
          else elseBranch : cells
    }
  where
    APP _ thenBranch = heap `Seq.index` thenAddr
    APP _ elseBranch = heap `Seq.index` elseAddr
-- replace the heap cell that contains the top APP-node of a graph with the cell that contains the result -> lazy evaluation
execInstruction Update ms@MachineState {stack = res : cells@(topApp : _), heap} =
  return ms
    { stack = cells,
      heap = Seq.update topApp (heap `Seq.index` res) heap
    }
-- replace the right child of the dummy APP-node with its expression graph and pop the expression node from the stack
execInstruction (UpdateLet n) ms@MachineState {stack = res : cells, heap} = do
  -- cells !? n: the heap cell of the right hand side expression
  replaced <- case cells !? n of
    Just rightChild -> case heap `Seq.index` rightChild of
      APP _ replaced -> Right replaced
      cell -> Left $ "Expected an APP-cell, but found " ++ show cell ++ "."
    Nothing -> Left "Stack access out of bounds."
  return ms
    { stack = cells,
      heap = Seq.update replaced (heap `Seq.index` res) heap
    }
-- allocate UNINIT-cell in the heap and push its address onto the stack
execInstruction Alloc ms = return $ allocate ms UNINIT
execInstruction i _ = Left $ "Cannot execute " ++ show i ++ "."