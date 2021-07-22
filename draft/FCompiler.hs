module FCompiler (translateProgram) where

import Parser
    ( Variable,
      Program,
      AtomicExpression(Var, Number, TruthValue),
      Expression(..),
      LocalDefinition(..),
      Definition(..) )
import MF
    ( MachineState(..),
      HeapCell(DEF),
      Operator(IfOperator, UnaryOperator, BinaryOperator),
      Instruction(..),
      Value(BoolValue, IntValue) )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Sequence
import Data.Sequence ( (|>) )


translateDef :: Definition -> [Instruction]
translateDef (Definition f args body) = translateKons body pos ++ [UpdateFun (length args), Slide (length args + 1), Unwind, Call, Return] where
    pos = zip args [1..]


translateLocalDefinitions :: [LocalDefinition] -> [(Variable, Int)] -> [Instruction]
translateLocalDefinitions [] pos = []
translateLocalDefinitions (LocalDef v e : lDefs) pos = translateKons e pos ++ [UpdateLet (length lDefs)] ++ translateLocalDefinitions lDefs pos

translateKons :: Expression -> [(Variable, Int)] -> [Instruction]
translateKons (Let localDefs expr) pos = concat (replicate (length localDefs) [Alloc, Alloc, Makeapp]) ++ translateLocalDefinitions localDefs pos' ++ translateKons expr pos' ++ [SlideLet (length localDefs)] where
    variables = [ v | LocalDef v _ <- localDefs]
    varPos = zip (reverse variables) [-1..]
    numVars = length variables
    pos' = varPos ++ map (\(x,y) -> (x, y + numVars)) pos
    -- recursive let in Haskell does not allow pos = varPos ++ map (\(x,y) -> (x, y + numVars)) pos, because the pos in the body refers to the newly defined pos
translateKons (FuncApp e1 e2) pos = translateKons e2 pos ++ translateKons e1 (map (\(x,y) -> (x, y+1)) pos) ++ [Makeapp]
translateKons (Unary op e) pos = translateKons e pos ++ [Pushpre (UnaryOperator op), Makeapp]
translateKons (Binary e1 op e2) pos = translateKons e2 pos ++ translateKons e1 (map (\(x,y) -> (x, y+1)) pos) ++ [Pushpre (BinaryOperator op), Makeapp, Makeapp]
translateKons (If e1 e2 e3) pos = translateKons e3 pos ++ translateKons e2 (map (\(x,y) -> (x, y+1)) pos) ++ translateKons e1 (map (\(x,y) -> (x, y+2)) pos) ++ [Pushpre IfOperator, Makeapp, Makeapp, Makeapp]
translateKons (AtomicExpr (Number i)) pos = [Pushval (IntValue i)]
translateKons (AtomicExpr (TruthValue b)) pos = [Pushval (BoolValue b)]
translateKons (AtomicExpr (Var v)) pos = case lookup v pos of
    Just i -> [Pushparam i]
    Nothing -> [Pushfun v]


add1Definition :: MachineState -> Definition -> MachineState
add1Definition ms@MachineState{code=c, heap=h, global=g} d@(Definition f args _) = ms{code=c ++ translateDef d, heap=h |> DEF (length c), global=Map.insert f (length h) g}

translateProgram :: Program -> MachineState
translateProgram = foldl add1Definition MachineState{pc=0, code=[
    Reset, Pushfun "main", Call, Halt,
    Pushparam 2, Unwind, Call, Pushparam 4, Unwind, Call, Operator2, UpdateOp, Return,
    Pushparam 1, Unwind, Call, OperatorIf, Unwind, Call, UpdateOp, Return,
    Pushparam 2, Unwind, Call, Operator1, UpdateOp, Return], stack=[], heap=Sequence.empty, global=Map.empty}