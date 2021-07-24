module MF (Value (..), Instruction (..), CodeAddr, HeapAddr, StackCell (..), Operator (..), HeapCell (..), MachineState (..), runMF) where

import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, index, update, (|>))
import qualified Data.Sequence as Sequence
import Debug.Trace (trace)
import Parser (BinaryOp (..), UnaryOp (..))

data Value = IntValue Integer | BoolValue Bool deriving (Eq, Show)

data Instruction = Reset | Pushfun String | Pushval Value | Pushparam Int | Makeapp | Slide Int | Unwind | Call | Return | Pushpre Operator | Operator1 | Operator2 | OperatorIf | UpdateFun Int | UpdateOp | UpdateLet Int | Alloc | SlideLet Int | Halt deriving (Eq, Show)

type CodeAddr = Int

type HeapAddr = Int

data StackCell = CodeAddr CodeAddr | HeapAddr HeapAddr | Operator Operator deriving (Show)

data Operator = UnaryOperator UnaryOp | BinaryOperator BinaryOp | IfOperator deriving (Eq, Show)

data HeapCell = DEF CodeAddr | VAL Value | APP HeapAddr HeapAddr | PRE Operator | IND HeapAddr | UNINIT deriving (Show)

data MachineState = MachineState {pc :: Int, code :: [Instruction], stack :: [StackCell], heap :: Seq HeapCell, global :: Map.Map String HeapAddr}

runMF :: MachineState -> MachineState
runMF ms@MachineState {pc = p, code = c, global = g} =
  let i = c !! p
   in if i == Halt
        then ms
        else runMF $ execInstruction i ms {pc = p + 1}
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
execInstruction (Pushfun f) ms@MachineState {stack = s, heap = h, global = g} =
  ms {stack = HeapAddr funAddr : s}
  where
    Just funAddr = Map.lookup f g
execInstruction (Pushval v) ms@MachineState {stack = s, heap = h} =
  ms
    { -- (length h) is exactly the index of the newly added heap cell
      stack = HeapAddr (length h) : s,
      heap = h |> VAL v
    }
execInstruction (Pushparam n) ms@MachineState {stack = s, heap = h} =
  ms {stack = HeapAddr arg2 : s}
  where
    -- a parameter of a function is the right child of an APP-node
    HeapAddr appAddr = s !! (n + 1)
    APP _ arg2 = value appAddr h
execInstruction Makeapp ms@MachineState {stack = HeapAddr fst : HeapAddr snd : cells, heap = h} =
  ms
    { stack = HeapAddr (length h) : cells,
      heap = h |> APP fst snd
    }
-- clean up the stack by removing the slots produced by Unwind, only the unevaluated body and the return address are kept
execInstruction (Slide n) ms@MachineState {stack = funBody : ra : cells} = 
    ms {stack = funBody : ra : drop n cells}
-- unfold the function graph until it hits a non-APP-node
execInstruction Unwind ms@MachineState {pc = p, stack = s@(HeapAddr top : cells), heap = h} =
  case value top h of
    APP addr1 _ -> ms {pc = p - 1, stack = HeapAddr addr1 : s}
    _ -> ms
execInstruction Call ms@MachineState {pc = p, stack = s@(HeapAddr top : cells), heap = h} =
  case value top h of
    DEF addr -> ms {pc = addr, stack = CodeAddr p : s}
    PRE uop@(UnaryOperator op) -> ms {pc = 21, stack = CodeAddr p : s}
    PRE bop@(BinaryOperator op) -> ms {pc = 4, stack = CodeAddr p : s}
    PRE IfOperator -> ms {pc = 13, stack = CodeAddr p : s}
    _ -> ms
execInstruction Return ms@MachineState {stack = res : CodeAddr ra : cells} =
  ms
    { pc = ra,
      stack = res : cells
    }
execInstruction (Pushpre op) ms@MachineState {stack = s, heap = h} =
  ms
    { stack = HeapAddr (length h) : s,
      heap = h |> PRE op
    }
execInstruction Operator1 ms@MachineState {stack = HeapAddr operand : ra : HeapAddr opAddr : cells, heap = h} =
  ms
    { -- the top APP-node of the operator graph is kept temporarily for later UpdateOp, therefore only the PRE-cell is removed
      stack = HeapAddr (length h) : ra : cells,
      heap = h |> VAL evalOpExpr
    }
  where
    VAL v = value operand h
    PRE (UnaryOperator op) = value opAddr h
    evalOpExpr = case op of
      Not -> let BoolValue b = v in BoolValue $ not b
      Neg -> let IntValue x = v in IntValue $ negate x
execInstruction Operator2 ms@MachineState {stack = HeapAddr sndOperand : HeapAddr fstOperand : ra : HeapAddr opAddr : cells, heap = h} =
  ms
    { -- the top APP-node of the operator graph is kept temporarily for later UpdateOp, therefore only the PRE-cell and the lower APP-node are removed
      stack = HeapAddr (length h) : ra : drop 1 cells,
      heap = h |> VAL evalOpExpr
    }
  where
    VAL v1 = value fstOperand h
    VAL v2 = value sndOperand h  -- v1 and v2 could be either intergers or truth values
    PRE (BinaryOperator op) = value opAddr h
    evalOpExpr = case op of
      Equal -> case (v1, v2) of
        (IntValue b1, IntValue b2) -> BoolValue $ b1 == b2
        (BoolValue b1, BoolValue b2) -> BoolValue $ b1 == b2
        _ -> BoolValue False  -- allow equality check between values of different types
      Smaller ->
        let IntValue b1 = v1
            IntValue b2 = v2
         in BoolValue $ b1 < b2
      Plus ->
        let IntValue b1 = v1
            IntValue b2 = v2
         in IntValue $ b1 + b2
      Minus ->
        let IntValue b1 = v1
            IntValue b2 = v2
         in IntValue $ b1 - b2
      Times ->
        let IntValue b1 = v1
            IntValue b2 = v2
         in IntValue $ b1 * b2
      Divide ->
        let IntValue b1 = v1
            IntValue b2 = v2
         in IntValue $ b1 `div` b2
execInstruction OperatorIf ms@MachineState {stack = HeapAddr condition : ra : _ : _ : HeapAddr thenAddr : HeapAddr elseAddr : cells, heap = h} =
  ms
    { -- the top APP-node of the operator graph is kept temporarily for later UpdateOp, therefore only the PRE-cell and the lower two APP-nodes are removed
      stack =
        if cond
          then HeapAddr thenBranch : ra : HeapAddr elseAddr : cells
          else HeapAddr elseBranch : ra : HeapAddr elseAddr : cells
    }
  where
    VAL (BoolValue cond) = value condition h
    APP _ thenBranch = value thenAddr h
    APP _ elseBranch = value elseAddr h
-- convert the heap cell that contains the top APP-node of a function application graph into an IND-cell that points at the the result of the function application -> lazy evaluation
execInstruction (UpdateFun n) ms@MachineState {stack = HeapAddr funBody : s, heap = h} =
  ms
    { heap = update replaced (IND funBody) h
    }
  where
    HeapAddr replaced = s !! (n + 1)  -- the heap cell of the top APP-node
-- replace the heap cell that contains the top APP-node with the result of the operator, top of the stack points to the updated APP-node heap cell
-- attention: lower app nodes are already cleaned up by the Operator instruction, different from normal functions that rely on Slide to clean up
execInstruction UpdateOp ms@MachineState {stack = HeapAddr res : ra : HeapAddr replaced : cells, heap = h} =
  ms
    { stack = HeapAddr replaced : ra : cells,
      heap = update replaced (h `index` res) h
    }
-- replace the right child of the dummy APP-node with its expression graph and pop the expression node from the stack
execInstruction (UpdateLet n) ms@MachineState {stack = HeapAddr res : cells, heap = h} =
  ms
    { stack = cells,
      heap = update replaced (IND res) h
    }
  where
    HeapAddr appAddr = cells !! n  -- the heap cell of the right hand side expression 
    APP _ replaced = value appAddr h
-- allocate UNINIT-cell in the heap and push its address onto the stack
execInstruction Alloc ms@MachineState {stack = s, heap = h} =
  ms
    { stack = HeapAddr (length h) : s,
      heap = h |> UNINIT
    }
-- clean up dummy APP-nodes for local definitions on the stack
execInstruction (SlideLet n) ms@MachineState {stack = res : cells} =
    ms {stack = res : drop n cells}