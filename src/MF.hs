module MF (Value (..), Instruction (..), CodeAddr, HeapAddr, StackCell (..), Operator (..), HeapCell (..), MachineState (..), runMF) where

import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, index, update, (|>))
import Debug.Trace (trace)
import Parser (BinaryOp (..), UnaryOp (..))

data Value = IntValue Integer | BoolValue Bool deriving (Eq, Show)

data Instruction = Reset | Pushfun String | Pushval Value | Pushparam Int | Makeapp | Slide Int | Unwind | Call | Return | Pushpre Operator | Operator1 | Operator2 | OperatorIf | UpdateFun Int | UpdateOp | UpdateLet Int | Alloc | SlideLet Int | Halt deriving (Eq, Show)

type CodeAddr = Int

type HeapAddr = Int

data StackCell = CodeAddr CodeAddr | HeapAddr HeapAddr deriving (Show)

data Operator = UnaryOperator UnaryOp | BinaryOperator BinaryOp | IfOperator deriving (Eq, Show)

data HeapCell = DEF CodeAddr | VAL Value | APP HeapAddr HeapAddr | PRE Operator | IND HeapAddr | UNINIT deriving (Show)

data MachineState = MachineState {pc :: Int, code :: [Instruction], stack :: [StackCell], heap :: Seq HeapCell, global :: Map.Map String HeapAddr} deriving Show

runMF :: MachineState -> MachineState
runMF ms@MachineState {pc, code} =
  let i = code !! pc
   in if i == Halt
        then ms
        else runMF $ execInstruction i ms {pc = pc + 1}
        -- for debugging: else runMF $ trace (show (stack ms) ++ "\n" ++ show (heap ms) ++ "\n" ++ show i) (execInstruction i ms{pc=p+1})

-- Follow the IND cell to find the actual heap cell
value :: HeapAddr -> Seq HeapCell -> HeapCell
value addr1 heap = case heap `index` addr1 of
  IND addr2 -> value addr2 heap
  anything -> anything

-- Specify how each instruction is executed
execInstruction :: Instruction -> MachineState -> MachineState
execInstruction Reset ms = ms {stack = []}
-- find the heap address of the function in the global environment and push the address onto the stack
execInstruction (Pushfun f) ms@MachineState {stack, global} =
  ms {stack = HeapAddr funAddr : stack}
  where
    Just funAddr = Map.lookup f global
execInstruction (Pushval v) ms@MachineState {stack, heap} =
  ms
    { -- (length h) is exactly the index of the newly added heap cell
      stack = HeapAddr (length heap) : stack,
      heap = heap |> VAL v
    }
execInstruction (Pushparam n) ms@MachineState {stack, heap} =
  ms {stack = HeapAddr arg2 : stack}
  where
    -- a parameter of a function is the right child of an APP-node
    HeapAddr appAddr = stack !! (n + 1)
    APP _ arg2 = value appAddr heap
execInstruction Makeapp ms@MachineState {stack = HeapAddr fst : HeapAddr snd : cells, heap} =
  ms
    { stack = HeapAddr (length heap) : cells,
      heap = heap |> APP fst snd
    }
-- clean up the stack by removing the slots produced by Unwind, only the unevaluated body and the return address are kept
execInstruction (Slide n) ms@MachineState {stack = funBody : ra : cells} = 
    ms {stack = funBody : ra : drop n cells}
-- unfold the function graph until it hits a non-APP-node
execInstruction Unwind ms@MachineState {pc, stack = stack@(HeapAddr top : cells), heap} =
  case value top heap of
    APP addr1 _ -> ms {pc = pc - 1, stack = HeapAddr addr1 : stack}
    _ -> ms
execInstruction Call ms@MachineState {pc, stack = stack@(HeapAddr top : cells), heap} =
  case value top heap of
    DEF addr -> ms {pc = addr, stack = CodeAddr pc : stack}
    PRE (UnaryOperator _) -> ms {pc = 21, stack = CodeAddr pc : stack}
    PRE (BinaryOperator _) -> ms {pc = 4, stack = CodeAddr pc : stack}
    PRE IfOperator -> ms {pc = 13, stack = CodeAddr pc : stack}
    _ -> ms
execInstruction Return ms@MachineState {stack = res : CodeAddr ra : cells} =
  ms
    { pc = ra,
      stack = res : cells
    }
execInstruction (Pushpre op) ms@MachineState {stack, heap} =
  ms
    { stack = HeapAddr (length heap) : stack,
      heap = heap |> PRE op
    }
execInstruction Operator1 ms@MachineState {stack = HeapAddr operand : ra : HeapAddr opAddr : cells, heap} =
  ms
    { -- the top APP-node of the operator graph is kept temporarily for later UpdateOp, therefore only the PRE-cell is removed
      stack = HeapAddr (length heap) : ra : cells,
      heap = heap |> VAL evalOpExpr
    }
  where
    VAL v = value operand heap
    PRE (UnaryOperator op) = value opAddr heap
    evalOpExpr = case (op, v) of
      (Not, BoolValue b) -> BoolValue $ not b
      (Neg, IntValue x) -> IntValue $ negate x
execInstruction Operator2 ms@MachineState {stack = HeapAddr sndOperand : HeapAddr fstOperand : ra : HeapAddr opAddr : cells, heap} =
  ms
    { -- the top APP-node of the operator graph is kept temporarily for later UpdateOp, therefore only the PRE-cell and the lower APP-node are removed
      stack = HeapAddr (length heap) : ra : drop 1 cells,
      heap = heap |> VAL evalOpExpr
    }
  where
    VAL v1 = value fstOperand heap
    VAL v2 = value sndOperand heap  -- v1 and v2 could be either intergers or truth values
    PRE (BinaryOperator op) = value opAddr heap
    evalOpExpr = case (op, v1, v2) of
      (Equal, IntValue x1, IntValue x2) -> BoolValue $ x1 == x2
      (Equal, BoolValue b1, BoolValue b2) -> BoolValue $ b1 == b2
      (Equal, _, _) -> BoolValue False  -- allow equality check between values of different types
      (Smaller, IntValue x1, IntValue x2) -> BoolValue $ x1 < x2
      (Plus, IntValue x1, IntValue x2) -> IntValue $ x1 + x2
      (Minus, IntValue x1, IntValue x2) -> IntValue $ x1 - x2
      (Times, IntValue x1, IntValue x2) -> IntValue $ x1 * x2
      (Divide, IntValue x1, IntValue x2) -> IntValue $ x1 `div` x2
execInstruction OperatorIf ms@MachineState {stack = HeapAddr condition : ra : _ : _ : HeapAddr thenAddr : HeapAddr elseAddr : cells, heap} =
  ms
    { -- the top APP-node of the operator graph is kept temporarily for later UpdateOp, therefore only the PRE-cell and the lower two APP-nodes are removed
      stack =
        if cond
          then HeapAddr thenBranch : ra : HeapAddr elseAddr : cells
          else HeapAddr elseBranch : ra : HeapAddr elseAddr : cells
    }
  where
    VAL (BoolValue cond) = value condition heap
    APP _ thenBranch = value thenAddr heap
    APP _ elseBranch = value elseAddr heap
-- convert the heap cell that contains the top APP-node of a function application graph into an IND-cell that points at the the result of the function application -> lazy evaluation
execInstruction (UpdateFun n) ms@MachineState {stack = HeapAddr funBody : stack, heap} =
  ms
    { heap = update replaced (IND funBody) heap
    }
  where
    HeapAddr replaced = stack !! (n + 1)  -- the heap cell of the top APP-node
-- replace the heap cell that contains the top APP-node with the result of the operator, top of the stack points to the updated APP-node heap cell
-- attention: lower app nodes are already cleaned up by the Operator instruction, different from normal functions that rely on Slide to clean up
execInstruction UpdateOp ms@MachineState {stack = HeapAddr res : ra : HeapAddr replaced : cells, heap} =
  ms
    { stack = HeapAddr replaced : ra : cells,
      heap = update replaced (heap `index` res) heap
    }
-- replace the right child of the dummy APP-node with its expression graph and pop the expression node from the stack
execInstruction (UpdateLet n) ms@MachineState {stack = HeapAddr res : cells, heap} =
  ms
    { stack = cells,
      heap = update replaced (IND res) heap
    }
  where
    HeapAddr appAddr = cells !! n  -- the heap cell of the right hand side expression 
    APP _ replaced = value appAddr heap
-- allocate UNINIT-cell in the heap and push its address onto the stack
execInstruction Alloc ms@MachineState {stack, heap} =
  ms
    { stack = HeapAddr (length heap) : stack,
      heap = heap |> UNINIT
    }
-- clean up dummy APP-nodes for local definitions on the stack
execInstruction (SlideLet n) ms@MachineState {stack = res : cells} =
    ms {stack = res : drop n cells}