module FCompiler (translateProgram) where

import qualified Data.Map.Strict as Map
import Data.Sequence ((|>))
import qualified Data.Sequence as Sequence
import MF
  ( HeapCell (DEF),
    Instruction (..),
    MachineState (..),
    Operator (BinaryOperator, IfOperator, UnaryOperator),
    Value (BoolValue, IntValue),
  )
import Parser
  ( Definition (..),
    Expression (..),
    LocalDefinition (..),
    Program,
    Variable,
  )

-- Translate function definitions
translateDef :: Definition -> [Instruction]
translateDef (Definition _ args body) =
  translateExpr body pos
    ++ [UpdateFun (length args), Slide (length args + 1), Unwind, Call, Return]
  where
    -- pos is a list of pairs (variable, variable's position in the stack)
    pos = zip args [1 ..]

-- Translate local definitions
translateLocalDefs :: [LocalDefinition] -> [(Variable, Int)] -> [Instruction]
translateLocalDefs [] _ = []
translateLocalDefs (LocalDef _ e : lDefs) pos =
  translateExpr e pos
    ++ [UpdateLet (length lDefs)]
    ++ translateLocalDefs lDefs pos

-- Translate expressions
translateExpr :: Expression -> [(Variable, Int)] -> [Instruction]
translateExpr (Let localDefs e) pos =
  -- first reserve space in the heap and stack for all the bindings
  -- all the local definitions are accessible from the beginning
  concat (replicate (length localDefs) [Alloc, Alloc, Makeapp])
    ++ translateLocalDefs localDefs pos'
    ++ translateExpr e pos'
    ++ [SlideLet (length localDefs)]
  where
    variables = [v | LocalDef v _ <- localDefs]
    varPos = zip (reverse variables) [-1 ..]
    numVars = length variables
    pos' = varPos ++ map (\(x, y) -> (x, y + numVars)) pos
    -- recursive let in Haskell does not allow pos = varPos ++ map (\(x,y) -> (x, y + numVars)) pos, because the pos in the body refers to the newly defined pos
translateExpr (FuncApp e1 e2) pos =
  translateExpr e2 pos
    ++ translateExpr e1 (map (\(x, y) -> (x, y + 1)) pos)
    ++ [Makeapp]
translateExpr (Unary op e) pos =
  translateExpr e pos
    ++ [Pushpre (UnaryOperator op), Makeapp]
translateExpr (Binary e1 op e2) pos =
  translateExpr e2 pos
    ++ translateExpr e1 (map (\(x, y) -> (x, y + 1)) pos)
    ++ [Pushpre (BinaryOperator op), Makeapp, Makeapp]
translateExpr (If e1 e2 e3) pos =
  translateExpr e3 pos
    ++ translateExpr e2 (map (\(x, y) -> (x, y + 1)) pos)
    ++ translateExpr e1 (map (\(x, y) -> (x, y + 2)) pos)
    ++ [Pushpre IfOperator, Makeapp, Makeapp, Makeapp]
translateExpr (Number i) _ = [Pushval (IntValue i)]
translateExpr (TruthValue b) _ = [Pushval (BoolValue b)]
translateExpr (Var v) pos =
  case lookup v pos of
    Just i -> [Pushparam i]
    Nothing -> [Pushfun v]

-- Add one definition to the initial machine state
add1Definition :: MachineState -> Definition -> MachineState
add1Definition ms@MachineState {code, heap, global, codeRange} d@(Definition f _ _) =
  ms
    { code = code',
      heap = heap |> DEF (length code),
      global = Map.insert f (length heap) global,
      codeRange = ((length code, length code' - 1), f) : codeRange
    }
  where
    code' = code ++ translateDef d

-- Translate the program (multiple function definitions)
translateProgram :: Program -> MachineState
translateProgram =
  foldl
    add1Definition
    MachineState
      { pc = 0,
        code =
          [ -- starting point
            Reset,
            Pushfun "main",
            Call,
            Halt,
            -- binary operators
            Pushparam 1,
            Unwind,
            Call,
            Pushparam 3,
            Unwind,
            Call,
            Operator2,
            UpdateOp,
            Return,
            -- if-then-else
            Pushparam 1,
            Unwind,
            Call,
            OperatorIf,
            Unwind,
            Call,
            UpdateOp,
            Return,
            -- unary operators
            Pushparam 1,
            Unwind,
            Call,
            Operator1,
            UpdateOp,
            Return
          ],
        -- the stack ist modelled as a list where the first element is the top of the stack
        stack = [],
        -- the heap is implemented with a finger tree
        heap = Sequence.empty,
        -- the global environment stores all the functions in a map (function name: heap address)
        global = Map.empty,
        -- codeRange is used to map a code address to the function it belongs to (to give more informative error messages)
        codeRange = []
      }