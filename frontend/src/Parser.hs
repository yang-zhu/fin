module Parser where

import Data.Set (Set)
import qualified Data.Set as Set
import Lexer

type Program = [Definition]

data Definition = Definition Variable [Variable] Expression deriving Show

data LocalDefinition = LocalDef Variable Expression deriving Show

data Expression
  = Let [LocalDefinition] Expression
  | If Expression Expression Expression
  | Binary Expression BinaryOp Expression
  | Unary UnaryOp Expression
  | FuncApp Expression Expression
  | Lambda [Variable] Expression
  | Var Variable
  | Number Integer
  | TruthValue Bool
  deriving (Show)

data BinaryOp
  = Equal
  | Smaller
  | Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Show)

data UnaryOp
  = Not
  | Neg
  deriving (Eq, Show)

type Variable = String

type ParseError = String

-- Grammar:
-- LocalDefinitions ::= LocalDefinition {; LocalDefinition}
-- LocalDefinition ::= Variable = Expression1
-- AtomicExpression ::= Variable | Literal | ( Expression1 )
--                    | \ {Variable} . Expression1
--                    | if Expression0 then Expression1 else Expression1
--                    | let LocalDefinitions in Expression1
-- Expression8 ::= AtomicExpression {AtomicExpression}
-- Expression7 ::= [-] Expression8
-- Expression6 ::= Expression7 RestExpression6
-- RestExpression6 ::= / Expression7 | {* Expression7}
-- Expression5 ::= Expression6 RestExpression5
-- RestExpression5 ::= - Expression6 | {+ Expression6}
-- Expression4 ::= Expression5 [== Expression5] | Expression5 [< Expression5]
-- Expression3 ::= [not] Expression4
-- Expression2 ::= Expression3 [& Expression2]
-- Expression1 ::= Expression2 [| Expression1]

-- Definition ::= Variable {Variable} = Expression1
-- Program ::= Definition ; {Definition ;} 

-- Check if the head of the token list matches the expected token. If it matches, consume the token; Otherwise output an error message
matchKeywordToken :: String -> [Token] -> Either ParseError [Token]
matchKeywordToken t (KeywordToken ln col t' : ts) =
  if t == t'
    then return ts
    else Left $ "Expected token " ++ show t ++ ", but found token " ++ show t' ++ " at position " ++ show (ln, col) ++ "."
matchKeywordToken t (t' : _) = Left $ "Expected token " ++ show t ++ ", but found token " ++ tokenToStr t' ++ " at position " ++ show (getTokenPos t') ++ "."
matchKeywordToken t [] = Left $ "Expected token " ++ show t ++ ", but found end of input."

-- Check if a token is NameToken
isNameToken :: Token -> Bool
isNameToken (NameToken _ _ _) = True
isNameToken _ = False

-- Parse local definitions
parseLocalDefinitions :: [Token] -> Either ParseError ([LocalDefinition], [Token])
parseLocalDefinitions (NameToken _ _ t : ts1) = do
  let (vs, ts2) = span isNameToken ts1
  ts3 <- matchKeywordToken "=" ts2
  (e, ts4) <- parseExpr1 ts3
  let lDef = if null vs then LocalDef t e else LocalDef t (Lambda [v | NameToken _ _ v <- vs] e)
  case ts4 of
    KeywordToken _ _ ";" : ts5 -> do
      (lDefs, rest) <- parseLocalDefinitions ts5
      return (lDef : lDefs, rest)
    ts4 -> return ([lDef], ts4)
parseLocalDefinitions (t : _) = Left $ "Expected local definition, but found token " ++ tokenToStr t ++ " at position " ++ show (getTokenPos t) ++ "."
parseLocalDefinitions [] = Left "Expected local definition, but found end of input."

-- Parse atomic expressions
parseAtomicExpr :: [Token] -> Either ParseError (Expression, [Token])
parseAtomicExpr (NumberToken _ _ t : ts) = return (Number t, ts)
parseAtomicExpr (NameToken _ _ t : ts) = return (Var t, ts)
parseAtomicExpr (KeywordToken _ _ "true" : ts) = return (TruthValue True, ts)
parseAtomicExpr (KeywordToken _ _ "false" : ts) = return (TruthValue False, ts)
parseAtomicExpr (KeywordToken _ _ "(" : ts) = do
  (e, ts') <- parseExpr1 ts
  rest <- matchKeywordToken ")" ts'
  return (e, rest)
parseAtomicExpr (KeywordToken _ _ "\\" : ts1) = do
  let (vs, ts2) = span isNameToken ts1
  ts3 <- matchKeywordToken "." ts2
  (e, rest) <- parseExpr1 ts3
  return (Lambda [v | NameToken _ _ v <- vs] e, rest)
parseAtomicExpr (KeywordToken _ _ "if" : ts1) = do
  (e1, ts2) <- parseExpr1 ts1
  ts2' <- matchKeywordToken "then" ts2
  (e2, ts3) <- parseExpr1 ts2'
  ts3' <- matchKeywordToken "else" ts3
  (e3, rest) <- parseExpr1 ts3'
  return (If e1 e2 e3, rest)
parseAtomicExpr (KeywordToken _ _ "let" : ts1) = do
  (localDefs, ts2) <- parseLocalDefinitions ts1
  ts2' <- matchKeywordToken "in" ts2
  (expr, rest) <- parseExpr1 ts2'
  return (Let localDefs expr, rest)
parseAtomicExpr (KeywordToken ln col t : _) = Left $ "Expected expression, but found token " ++ show t ++ " at position " ++ show (ln, col) ++ "."
parseAtomicExpr [] = error "unreachable case"  -- parseManyAtomicExpr [] makes it unreachable

startSymbols :: [String]
startSymbols = ["(", "true", "false", "\\", "if", "let"]

isExprStart :: Token -> Bool
isExprStart (KeywordToken _ _ t) = t `elem` startSymbols
isExprStart (NumberToken _ _ _) = True
isExprStart (NameToken _ _ _) = True

parseManyAtomicExpr :: [Token] -> Either ParseError ([Expression], [Token])
parseManyAtomicExpr [] = return ([], [])
parseManyAtomicExpr ts@(t : _) =
  if isExprStart t
    then do
      (e, ts') <- parseAtomicExpr ts
      (es, rest) <- parseManyAtomicExpr ts'
      return (e : es, rest)
    else return ([], ts)

-- Parse function applications
parseExpr8 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr8 ts = do
  (es, ts') <- parseManyAtomicExpr ts
  case (es, ts') of
    (e : es, ts') -> return (foldl FuncApp e es, ts')
    ([], []) -> Left "Expected expression, but found end of input."
    ([], t : _) -> Left $ "Expected expression, but found token " ++ tokenToStr t ++ " at position " ++ show (getTokenPos t) ++ "."

-- Parse unary minus
parseExpr7 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr7 (KeywordToken _ _ "-" : ts) = do
  (e, rest) <- parseExpr8 ts
  return (Unary Neg e, rest)
parseExpr7 ts = parseExpr8 ts

parseMultiplications :: [Token] -> Either ParseError ([Expression], [Token])
parseMultiplications (KeywordToken _ _ "*" : ts) = do
  (e, ts') <- parseExpr7 ts
  (es, rest) <- parseMultiplications ts'
  return (e : es, rest)
parseMultiplications ts = return ([], ts)

-- Parse * and / operators
parseExpr6 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr6 ts = do
  (e, ts') <- parseExpr7 ts
  case ts' of
    (KeywordToken _ _ "/" : ts') -> do
      (divisor, rest) <- parseExpr7 ts'
      return (Binary e Divide divisor, rest)
    _ -> do
      (es, rest) <- parseMultiplications ts'
      return (foldl (\x y -> Binary x Times y) e es, rest)

parseAdditions :: [Token] -> Either ParseError ([Expression], [Token])
parseAdditions (KeywordToken _ _ "+" : ts) = do
  (e, ts') <- parseExpr6 ts
  (es, rest) <- parseAdditions ts'
  return (e : es, rest)
parseAdditions ts = return ([], ts)

-- Parse + and - operators
parseExpr5 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr5 ts = do
  (e, ts') <- parseExpr6 ts
  case ts' of
    (KeywordToken _ _ "-" : ts') -> do
      (subtrahend, rest) <- parseExpr6 ts'
      return (Binary e Minus subtrahend, rest)
    _ -> do
      (es, rest) <- parseAdditions ts'
      return (foldl (\x y -> Binary x Plus y) e es, rest)

-- Parse == and < operators
parseExpr4 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr4 ts = do
  (e1, ts') <- parseExpr5 ts
  case ts' of
    (KeywordToken _ _ "==" : ts') -> do
      (e2, rest) <- parseExpr5 ts'
      return (Binary e1 Equal e2, rest)
    (KeywordToken _ _ "<" : ts') -> do
      (e2, rest) <- parseExpr5 ts'
      return (Binary e1 Smaller e2, rest)
    _ -> return (e1, ts')

-- Parse negation
parseExpr3 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr3 (KeywordToken _ _ "not" : ts) = do
  (e, rest) <- parseExpr4 ts
  return (Unary Not e, rest)
parseExpr3 ts = parseExpr4 ts

-- Parse And Operator
-- And Operator is de-sugared to if-then-else expression to enable short circuit evaluation
parseExpr2 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr2 ts = do
  (e1, ts') <- parseExpr3 ts
  case ts' of
    (KeywordToken _ _ "&" : ts') -> do
      (e2, rest) <- parseExpr2 ts'
      return (If e1 e2 (TruthValue False), rest)
    _ -> return (e1, ts')

-- Parse Or Operator
-- Or Operator is de-sugared to if-then-else expression to enable short circuit evaluation
parseExpr1 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr1 ts = do
  (e1, ts') <- parseExpr2 ts
  case ts' of
    (KeywordToken _ _ "|" : ts') -> do
      (e2, rest) <- parseExpr1 ts'
      return (If e1 (TruthValue True) e2, rest)
    _ -> return (e1, ts')

-- Parse if-then-else and let expressions
-- parseExpr0 :: [Token] -> Either ParseError (Expression, [Token])
-- parseExpr0 (KeywordToken _ _ "if" : ts1) = do
--   (e1, ts2) <- parseExpr0 ts1
--   ts2' <- matchKeywordToken "then" ts2
--   (e2, ts3) <- parseExpr0 ts2'
--   ts3' <- matchKeywordToken "else" ts3
--   (e3, rest) <- parseExpr0 ts3'
--   return (If e1 e2 e3, rest)
-- parseExpr0 (KeywordToken _ _ "let" : ts1) = do
--   (localDefs, ts2) <- parseLocalDefinitions ts1
--   ts2' <- matchKeywordToken "in" ts2
--   (expr, rest) <- parseExpr0 ts2'
--   return (Let localDefs expr, rest)
-- parseExpr0 ts = parseExpr1 ts

-- Parse definitions
parseDefinition :: [Token] -> Either ParseError (Definition, [Token])
parseDefinition (NameToken _ _ t : ts1) = do
  let (vs, ts2) = span isNameToken ts1
  ts3 <- matchKeywordToken "=" ts2
  (e, rest) <- parseExpr1 ts3
  return (Definition t [v | NameToken _ _ v <- vs] e, rest)
parseDefinition (t : _) = Left $ "Expected function name, but found token " ++ tokenToStr t ++ " at position " ++ show (getTokenPos t) ++ "."
parseDefinition [] = error "unreachable case"  -- parseProgram [] makes it unreachable

-- Parse a program
parseProgram :: [Token] -> Either ParseError Program
parseProgram [] = return []
parseProgram ts = do
  (d, ts') <- parseDefinition ts
  rest <- matchKeywordToken ";" ts'
  definitions <- parseProgram rest
  return (d : definitions)

localDefFreeVars :: [LocalDefinition] -> Set Variable
localDefFreeVars [] = Set.empty
localDefFreeVars (LocalDef _ e : localDefs) = Set.union (freeVars e) (localDefFreeVars localDefs)

freeVars :: Expression -> Set Variable
freeVars (TruthValue _) = Set.empty
freeVars (Number _) = Set.empty
freeVars (Var v) = Set.singleton v
freeVars (Lambda vs e) = Set.difference (freeVars e) (Set.fromList vs)
freeVars (FuncApp e1 e2) = Set.union (freeVars e1) (freeVars e2)
freeVars (Unary _ e) = freeVars e
freeVars (Binary e1 _ e2) = Set.union (freeVars e1) (freeVars e2)
freeVars (If e1 e2 e3) = Set.union (Set.union (freeVars e1) (freeVars e2)) (freeVars e3)
freeVars (Let localDefs e) = Set.union (Set.difference (localDefFreeVars localDefs) vs) (Set.difference (freeVars e) vs)
  where
    vs = Set.fromList [v | LocalDef v _ <- localDefs]

lambdaLiftDef :: Definition -> [Definition]
lambdaLiftDef (Definition v vs e) = Definition v vs e' : defs
  where
    (e', defs) = lambdaLiftExpr (Set.fromList vs) e 0

    lambdaLiftExpr :: Set Variable -> Expression -> Int -> (Expression, [Definition])
    lambdaLiftExpr bound e i = case e of
      Lambda vs body ->
        let (body', defs) = lambdaLiftExpr (Set.union bound (Set.fromList vs)) body (i + 1)
            newFuncName = "$" ++ v ++ "_" ++ show i
            freeVariables = Set.toList (Set.intersection bound (freeVars e))
         in (foldl FuncApp (Var newFuncName) [Var v | v <- freeVariables], Definition newFuncName (freeVariables ++ vs) body' : defs)
      FuncApp e1 e2 ->
        let (e1', defs1) = lambdaLiftExpr bound e1 i
            (e2', defs2) = lambdaLiftExpr bound e2 (i + length defs1)
         in (FuncApp e1' e2', defs1 ++ defs2)
      Unary op e ->
        let (e', defs) = lambdaLiftExpr bound e i
         in (Unary op e', defs)
      Binary e1 op e2 ->
        let (e1', defs1) = lambdaLiftExpr bound e1 i
            (e2', defs2) = lambdaLiftExpr bound e2 (i + length defs1)
         in (Binary e1' op e2', defs1 ++ defs2)
      If e1 e2 e3 ->
        let (e1', defs1) = lambdaLiftExpr bound e1 i
            (e2', defs2) = lambdaLiftExpr bound e2 (i + length defs1)
            (e3', defs3) = lambdaLiftExpr bound e3 (i + length defs1 + length defs2)
         in (If e1' e2' e3', defs1 ++ defs2 ++ defs3)
      Let localDefs e ->
        let bound' = Set.union bound (Set.fromList [v | LocalDef v _ <- localDefs])
            (localDefs', defs1) = lambdaLiftLocalDefs bound' localDefs i
            (e', defs2) = lambdaLiftExpr bound' e (i + length defs)
         in (Let localDefs' e', defs1 ++ defs2)
      otherExpr -> (otherExpr, [])

    lambdaLiftLocalDefs :: Set Variable -> [LocalDefinition] -> Int -> ([LocalDefinition], [Definition])
    lambdaLiftLocalDefs _ [] _ = ([], [])
    lambdaLiftLocalDefs bound (LocalDef v e : localDefs) i =
      let (e', def) = lambdaLiftExpr bound e i
          (es, defs) = lambdaLiftLocalDefs bound localDefs (i + length def)
       in (LocalDef v e' : es, def ++ defs)

lambdaLiftProg :: Program -> Program
lambdaLiftProg = concatMap lambdaLiftDef