module Parser (Program, Definition (..), LocalDefinition (..), Expression (..), BinaryOp (..), UnaryOp (..), Variable, parseProgram) where

import Debug.Trace (trace)
import Lexer (Token (..))

type Program = [Definition]

data Definition = Definition Variable [Variable] Expression deriving (Show)

data LocalDefinition = LocalDef Variable Expression deriving (Show)

data Expression
  = Let [LocalDefinition] Expression
  | If Expression Expression Expression
  | Binary Expression BinaryOp Expression
  | Unary UnaryOp Expression
  | FuncApp Expression Expression
  | Var Variable
  | Number Integer
  | TruthValue Bool
  deriving (Show)

data BinaryOp = Equal | Smaller | Plus | Minus | Times | Divide deriving (Eq, Show)

data UnaryOp = Not | Neg deriving (Eq, Show)

type Variable = String

type ParseError = String

-- Grammar:
-- AtomicExpression ::= Variable | Literal | ( Expression0 )
-- Expression8 ::= AtomicExpression {AtomicExpression}
-- Expression7 ::= [-] Expression8
-- Expression6 ::= Expression7 RestExpression6
-- RestExpression6 ::= / Expression7 | {* Expression7}
-- Expression5 ::= Expression6 RestExpression5
-- RestExpression5 ::= - Expression6 | {+ Expression6}
-- Expression4 ::= Expression5 [== Expression5] | Expression5 [< Expression5]
-- Expression3 := [not] Expression4
-- Expression2 := Expression3 [& Expression2]
-- Expression1 := Expression2 [| Expression1]
-- Expression0 := if Expression0 then Expression0 else Expression0
--              | let LocalDefinitions in Expression0
--              | Expression1
-- LocalDefinitions := LocalDefinition {; LocalDefinition}
-- LocalDefinition := Variable = Expression0

-- Definition := Variable {Variable} = Expression0
-- Program := Definition; {Definition ;} 

-- Check if the head of the token list matches the expected token. If it matches, consume the token; Otherwise output an error message
matchKeywordToken :: String -> [Token] -> Either ParseError [Token]
matchKeywordToken t (KeywordToken t' : ts) =
  if t == t'
    then return ts
    else Left $ "Expected token " ++ show t ++ ", but found token " ++ show t' ++ "."
matchKeywordToken t (t' : ts) = Left $ "Expected token " ++ show t ++ ", but found " ++ show t' ++ "."
matchKeywordToken t [] = Left $ "Expected token " ++ show t ++ ", but found end of input."

-- Parse atomic expressions
parseAtomicExpr :: [Token] -> Either ParseError (Expression, [Token])
parseAtomicExpr (NumberToken t : ts) = return (Number t, ts)
parseAtomicExpr (NameToken t : ts) = return (Var t, ts)
parseAtomicExpr (KeywordToken "true" : ts) = return (TruthValue True, ts)
parseAtomicExpr (KeywordToken "false" : ts) = return (TruthValue False, ts)
parseAtomicExpr (KeywordToken "(" : ts) =
  do
    (e, ts') <- parseExpr0 ts
    rest <- matchKeywordToken ")" ts'
    return (e, rest)
parseAtomicExpr (KeywordToken t : ts) = Left $ "Expected expression, but found token" ++ show t ++ "."

startSymbols = ["(", "true", "false"]

isExprStart :: Token -> Bool
isExprStart (KeywordToken t) = t `elem` startSymbols
isExprStart (NumberToken t) = True
isExprStart (NameToken t) = True

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
parseExpr8 ts =
  do
    (es, ts') <- parseManyAtomicExpr ts
    case (es, ts') of
      (e : es, ts') -> return (foldl FuncApp e es, ts')
      ([], []) -> Left "Expected expression, but found end of input."
      ([], t : rest) -> Left $ "Expected expression, but found " ++ show t ++ "."

-- Parse unary minus
parseExpr7 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr7 (KeywordToken "-" : ts) =
  do
    (e, rest) <- parseExpr8 ts
    return (Unary Neg e, rest)
parseExpr7 ts = parseExpr8 ts

parseMultiplications :: [Token] -> Either ParseError ([Expression], [Token])
parseMultiplications (KeywordToken "*" : ts) =
  do
    (e, ts') <- parseExpr7 ts
    (es, rest) <- parseMultiplications ts'
    return (e : es, rest)
parseMultiplications ts = return ([], ts)

-- Parse * and / operators
parseExpr6 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr6 ts =
  do
    (e, ts') <- parseExpr7 ts
    case ts' of
      (KeywordToken "/" : ts') ->
        do
          (divisor, rest) <- parseExpr7 ts'
          return (Binary e Divide divisor, rest)
      _ ->
        do
          (es, rest) <- parseMultiplications ts'
          return (foldl (\x y -> Binary x Times y) e es, rest)

parseAdditions :: [Token] -> Either ParseError ([Expression], [Token])
parseAdditions (KeywordToken "+" : ts) =
  do
    (e, ts') <- parseExpr6 ts
    (es, rest) <- parseAdditions ts'
    return (e : es, rest)
parseAdditions ts = return ([], ts)

-- Parse + and - operators
parseExpr5 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr5 ts =
  do
    (e, ts') <- parseExpr6 ts
    case ts' of
      (KeywordToken "-" : ts') -> do
        (subtrahend, rest) <- parseExpr6 ts'
        return (Binary e Minus subtrahend, rest)
      _ -> do
        (es, rest) <- parseAdditions ts'
        return (foldl (\x y -> Binary x Plus y) e es, rest)

-- Parse == and < operators
parseExpr4 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr4 ts =
  do
    (e1, ts') <- parseExpr5 ts
    case ts' of
      (KeywordToken "==" : ts') -> do
        (e2, rest) <- parseExpr5 ts'
        return (Binary e1 Equal e2, rest)
      (KeywordToken "<" : ts') -> do
        (e2, rest) <- parseExpr5 ts'
        return (Binary e1 Smaller e2, rest)
      _ -> return (e1, ts')

-- Parse negation
parseExpr3 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr3 (KeywordToken "not" : ts) =
  do
    (e, rest) <- parseExpr4 ts
    return (Unary Not e, rest)
parseExpr3 ts = parseExpr4 ts

-- Parse And Operator
-- And Operator is de-sugared to if-then-else expression to enable short circuit evaluation
parseExpr2 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr2 ts =
  do
    (e1, ts') <- parseExpr3 ts
    case ts' of
      (KeywordToken "&" : ts') -> do
        (e2, rest) <- parseExpr2 ts'
        return (If e1 e2 (TruthValue False), rest)
      _ -> return (e1, ts')

-- Parse Or Operator
-- Or Operator is de-sugared to if-then-else expression to enable short circuit evaluation
parseExpr1 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr1 ts =
  do
    (e1, ts') <- parseExpr2 ts
    case ts' of
      (KeywordToken "|" : ts') -> do
        (e2, rest) <- parseExpr1 ts'
        return (If e1 (TruthValue True) e2, rest)
      _ -> return (e1, ts')

-- Parse if-then-else and let expressions
parseExpr0 :: [Token] -> Either ParseError (Expression, [Token])
parseExpr0 (KeywordToken "if" : ts1) =
  do
    (e1, ts2) <- parseExpr0 ts1
    ts2' <- matchKeywordToken "then" ts2
    (e2, ts3) <- parseExpr0 ts2'
    ts3' <- matchKeywordToken "else" ts3
    (e3, rest) <- parseExpr0 ts3'
    return (If e1 e2 e3, rest)
parseExpr0 (KeywordToken "let" : ts1) =
  do
    (localDefs, ts2) <- parseLocalDefinitions ts1
    ts2' <- matchKeywordToken "in" ts2
    (expr, rest) <- parseExpr0 ts2'
    return (Let localDefs expr, rest)
parseExpr0 ts = parseExpr1 ts

-- Parse local definitions
parseLocalDefinitions :: [Token] -> Either ParseError ([LocalDefinition], [Token])
parseLocalDefinitions (NameToken t : KeywordToken "=" : ts) =
  do
    (e, ts') <- parseExpr0 ts
    case ts' of
      KeywordToken ";" : bindings -> do
        (ldefs, rest) <- parseLocalDefinitions bindings
        return (LocalDef t e : ldefs, rest)
      ts' -> return ([LocalDef t e], ts')
parseLocalDefinitions (t : ts) = Left $ "Expected local definition, but found " ++ show t ++ "."
parseLocalDefinitions [] = Left "Expected local definition, but found end of input."

-- Check if a token is NameToken
isNameToken :: Token -> Bool
isNameToken (NameToken t) = True
isNameToken _ = False

-- Parse definitions
parseDefinition :: [Token] -> Either ParseError (Definition, [Token])
parseDefinition (NameToken t : ts) =
  do
    let vs = takeWhile isNameToken ts
    let ts' = dropWhile isNameToken ts
    expr <- matchKeywordToken "=" ts'
    (e, rest) <- parseExpr0 expr
    return (Definition t [v | NameToken v <- vs] e, rest)
parseDefinition (t : ts) = Left $ "Expected function name, but found " ++ show t ++ "."

-- Parse a program
parseProgram :: [Token] -> Either ParseError Program
parseProgram [] = return []
parseProgram ts =
  do
    (d, ts') <- parseDefinition ts
    rest <- matchKeywordToken ";" ts'
    definitions <- parseProgram rest
    return (d : definitions)