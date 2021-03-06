module FCompiler where

import qualified Data.Map.Strict as Map
import Data.Sequence ((|>), (><))
import qualified Data.Sequence as Seq
import MF
import Parser

-- Translate function definitions
translateDef :: Definition -> [Instruction]
translateDef (Definition _ args body) =
  translateExpr body pos
    ++ [Slide (length args), Update, Unwind, Call, Return]
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
    ++ [Slide (length localDefs)]
  where
    variables = [v | LocalDef v _ <- localDefs]
    varPos = zip (reverse variables) [0 ..]
    numVars = length variables
    pos' = varPos ++ map (\(x, y) -> (x, y + numVars)) pos
    -- recursive let in Haskell does not allow pos = varPos ++ map (\(x,y) -> (x, y + numVars)) pos, because the pos in the body refers to the newly defined pos
translateExpr (FuncApp e1 e2) pos =
  translateExpr e2 pos
    ++ translateExpr e1 (map (\(x, y) -> (x, y + 1)) pos)
    ++ [Makeapp]
translateExpr (Unary op e) pos =
  translateExpr e pos
    ++ [Pushpre (UnOp op), Makeapp]
translateExpr (Binary e1 op e2) pos =
  translateExpr e2 pos
    ++ translateExpr e1 (map (\(x, y) -> (x, y + 1)) pos)
    ++ [Pushpre (BinOp op), Makeapp, Makeapp]
translateExpr (If e1 e2 e3) pos =
  translateExpr e3 pos
    ++ translateExpr e2 (map (\(x, y) -> (x, y + 1)) pos)
    ++ translateExpr e1 (map (\(x, y) -> (x, y + 2)) pos)
    ++ [Pushpre IfOp, Makeapp, Makeapp, Makeapp]
translateExpr (Number i) _ = [Pushval (IntValue i)]
translateExpr (TruthValue b) _ = [Pushval (BoolValue b)]
translateExpr (Var v) pos =
  case lookup v pos of
    Just i -> [Pushparam i]
    Nothing -> [Pushfun v]
translateExpr _ _ = []  -- unreachable, just to silence the warning 

-- Add one definition to the initial machine state
add1Definition :: MachineState -> Definition -> MachineState
add1Definition ms@MachineState {code, heap, global, codeRange} d@(Definition f _ _) =
  ms
    { code = code',
      heap = heap |> DEF (Seq.length code),
      global = Map.insert f (length heap) global,
      codeRange = ((Seq.length code, Seq.length code' - 1), f) : codeRange
    }
  where
    code' = code >< Seq.fromList (translateDef d)

-- Translate the program (multiple function definitions)
translateProgram :: Program -> MachineState
translateProgram =
  foldl
    add1Definition
    MachineState
      { pc = 0,
        code = 
          Seq.fromList 
          [ -- starting point
            Reset,
            Pushfun "main",
            Call,
            Halt,
            -- unary operators
            Pushparam 1,
            Unwind,
            Call,
            Operator1,
            Slide 1,
            Update,
            Return,
            -- binary operators
            Pushparam 1,
            Unwind,
            Call,
            Pushparam 3,
            Unwind,
            Call,
            Operator2,
            Slide 2,
            Update,
            Return,
            -- if-then-else
            Pushparam 1,
            Unwind,
            Call,
            OperatorIf,
            Slide 3,
            Update,
            Unwind,
            Call,            
            Return
          ],
        -- the stack is modelled as a list where the first element is the top of the stack
        stack = [],
        returnStack = [],
        -- the heap is implemented with a finger tree
        heap = Seq.empty,
        -- the global environment stores all the functions in a map (function name: heap address)
        global = Map.empty,
        -- codeRange is used to map a code address to the function it belongs to (to give more informative error messages)
        codeRange = [],
        freeList = [],
        gcInterval = 10
      }