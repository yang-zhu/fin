module MF (Value(..), Instruction(..), CodeAddr, HeapAddr, StackCell(..), Operator(..), HeapCell(..), MachineState(..), runMF) where

import Parser ( UnaryOp(..), BinaryOp(..) )
import Data.List ( findIndex )
import Debug.Trace ( trace )


data Value = IntValue Integer | BoolValue Bool deriving (Eq, Show)
data Instruction = Reset | Pushfun String | Pushval Value | Pushparam Int | Makeapp | Slide Int | Unwind | Call | Return | Pushpre Operator | Operator1 | Operator2 | OperatorIf | UpdateFun Int | UpdateOp | UpdateLet Int | Alloc | SlideLet Int | Halt deriving (Eq, Show)
type CodeAddr = Int
type HeapAddr = Int
data StackCell = CodeAddr CodeAddr | HeapAddr HeapAddr | Operator Operator deriving Show
data Operator = UnaryOperator UnaryOp | BinaryOperator BinaryOp | IfOperator deriving (Eq, Show)
data HeapCell = DEF Int CodeAddr | VAL Value | APP HeapAddr HeapAddr | PRE Operator | IND HeapAddr | UNINIT deriving Show
type GlobalFunction = (String, HeapAddr)
-- We model the stack as a list where the first element is the top of the stack
data MachineState = MachineState {pc :: Int, code :: [Instruction], stack :: [StackCell], heap :: [HeapCell], global :: [GlobalFunction]}


runMF :: MachineState -> MachineState
runMF ms@MachineState{pc=p, code=c} = let
    i = c!!p
    in if i == Halt
        then ms
        else runMF $ execInstruction i ms{pc=p+1}
        -- for debugging: else runMF $ trace (show (stack ms) ++ "\n" ++ show (heap ms) ++ "\n" ++ show i) (execInstruction i ms{pc=p+1})


setAt :: [HeapCell] -> HeapAddr -> HeapCell -> [HeapCell]
setAt (fst:cells) 0 newCell = newCell : cells
setAt (fst:cells) i newCell = fst : setAt cells (i-1) newCell

value :: HeapAddr -> [HeapCell] -> HeapCell
value addr1 heap = case heap!!addr1 of
    IND addr2 -> value addr2 heap
    anything -> anything

execInstruction :: Instruction -> MachineState -> MachineState
execInstruction Reset ms = ms{stack=[]}
execInstruction (Pushfun f) ms@MachineState{stack=s, heap=h, global=g} = ms{stack=let
    Just hAddr = lookup f g
    in HeapAddr hAddr : s}
execInstruction (Pushval v) ms@MachineState{stack=s, heap=h} = ms{stack=HeapAddr (length h) : s, heap=h ++ [VAL v]}
execInstruction (Pushparam n) ms@MachineState{stack=s, heap=h} = ms{stack=let
    HeapAddr hCellInd = s!!(n+1)
    APP _ arg2 = value hCellInd h
    in HeapAddr arg2 : s} 
execInstruction Makeapp ms@MachineState{stack=HeapAddr fst : HeapAddr snd : cells, heap=h} = ms{stack=HeapAddr (length h) : cells, heap=h ++ [APP fst snd]}
execInstruction (Slide n) ms@MachineState{stack=res: ra: cells} = ms{stack=res : ra : drop n cells}
execInstruction Unwind ms@MachineState{pc=p, stack=s@(HeapAddr appCell : cells), heap=h} = case value appCell h of
    APP addr1 _ -> ms{pc=p-1, stack=HeapAddr addr1 : s}
    _ -> ms
execInstruction Call ms@MachineState{pc=p, stack=s@(HeapAddr top : cells), heap=h} = case value top h of
    DEF _ addr -> ms{pc=addr, stack=CodeAddr p : s}
    PRE uop@(UnaryOperator op) -> ms{pc=21, stack=Operator uop : CodeAddr p : s}
    PRE bop@(BinaryOperator op) -> ms{pc=4, stack=Operator bop : CodeAddr p : s}
    PRE IfOperator -> ms{pc=13, stack=CodeAddr p : s}
    _-> ms
execInstruction Return ms@MachineState{stack=res : CodeAddr ra: cells} = ms{pc=ra, stack=res:cells}
execInstruction (Pushpre op) ms@MachineState{stack=s, heap=h} = ms{stack=HeapAddr (length h) : s, heap=h ++ [PRE op]}
execInstruction Operator1 ms@MachineState{stack=HeapAddr resCell : (Operator (UnaryOperator op)) : ra : cells, heap=h} = ms{stack=HeapAddr (length h) : ra : drop 1 cells, heap=let
    VAL v = value resCell h
    evalOpExpr = case op of
        Not -> let BoolValue b = v in BoolValue $ not b
        Neg -> let IntValue x = v in IntValue $ negate x
    in h ++ [VAL evalOpExpr]}
execInstruction Operator2 ms@MachineState{stack=HeapAddr sndCell : HeapAddr fstCell : (Operator (BinaryOperator op)) : ra : cells, heap=h} = ms{stack=HeapAddr (length h) : ra : drop 2 cells, heap=let
    VAL v1 = value fstCell h
    VAL v2 = value sndCell h
    evalOpExpr = case op of
        Equal -> case (v1, v2) of
            (IntValue b1, IntValue b2) -> BoolValue $ b1 == b2
            (BoolValue b1, BoolValue b2) -> BoolValue $ b1 == b2
            _ -> BoolValue False
        Smaller -> let
            IntValue b1 = v1
            IntValue b2 = v2 
            in BoolValue $ b1 < b2
        Plus -> let
            IntValue b1 = v1
            IntValue b2 = v2 
            in IntValue $ b1 + b2
        Minus -> let
            IntValue b1 = v1
            IntValue b2 = v2 
            in IntValue $ b1 - b2
        Times -> let
            IntValue b1 = v1
            IntValue b2 = v2 
            in IntValue $ b1 * b2
        Divide -> let
            IntValue b1 = v1
            IntValue b2 = v2 
            in IntValue $ b1 `div` b2
        in h ++ [VAL evalOpExpr]}
execInstruction OperatorIf ms@MachineState{stack=HeapAddr condition : ra : _ : _ : HeapAddr thenCell : HeapAddr elseCell : cells, heap=h} = ms{stack=let
    VAL (BoolValue cond) = value condition h
    APP _ thenBranch = value thenCell h
    APP _ elseBranch = value elseCell h
    in if cond 
        then HeapAddr thenBranch : ra : HeapAddr elseCell : cells
        else HeapAddr elseBranch : ra : HeapAddr elseCell : cells}
execInstruction (UpdateFun n) ms@MachineState{stack=HeapAddr top : s, heap=h} = ms{heap=let
    HeapAddr replaced = s!!(n+1)
    in setAt h replaced (IND top)}
execInstruction UpdateOp ms@MachineState{stack=HeapAddr res : ra : HeapAddr replaced : cells, heap=h} = ms{stack=HeapAddr replaced : ra : cells , heap=setAt h replaced (h!!res)}
-- replaces the right child of the dummy APP-node with its expression graph and pops the expression node from the stack
execInstruction (UpdateLet n) ms@MachineState{stack=HeapAddr res : cells, heap=h} = ms{stack=cells, heap=let
    HeapAddr appAddr = cells!!n
    APP _ replaced = value appAddr h
    in setAt h replaced (IND res)}
execInstruction Alloc ms@MachineState{stack=s, heap=h} = ms{stack=HeapAddr (length h) : s, heap=h ++ [UNINIT]}
execInstruction (SlideLet n) ms@MachineState{stack=resCell : cells} = ms{stack=resCell : drop n cells}