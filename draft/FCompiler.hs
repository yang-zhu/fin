module FCompiler (translateProgram) where

import Parser
    ( Variable,
      Program,
      AtomicExpression(Var, Number, TruthValue),
      Expression(..),
      Definition(..) )
import MF
    ( MachineState(..),
      HeapCell(DEF),
      Operator(IfOperator, UnaryOperator, BinaryOperator),
      Instruction(..),
      Value(BoolValue, IntValue) )


translateDef :: Definition -> [Instruction]
translateDef (Definition f args body) = translateKons body pos ++ [UpdateFun (length args), Slide (length args + 1), Unwind, Call, Return] where
    pos = zip args [1..]


translateKons :: Expression -> [(Variable, Int)] -> [Instruction]
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
add1Definition ms@MachineState{code=c, heap=h} d@(Definition f args _) = ms{code=c ++ translateDef d, heap=h ++ [DEF f (length args) (length c)]}

translateProgram :: Program -> MachineState
translateProgram = foldl add1Definition MachineState{pc=0, code=[
    Reset, Pushfun "main", Call, Halt,
    Pushparam 2, Unwind, Call, Pushparam 4, Unwind, Call, Operator2, UpdateOp, Return,
    Pushparam 1, Unwind, Call, OperatorIf, Unwind, Call, UpdateOp, Return,
    Pushparam 2, Unwind, Call, Operator1, UpdateOp, Return], stack=[], heap=[]}