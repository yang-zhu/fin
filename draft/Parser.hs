module Parser (Program, Definition(..), LocalDefinition(..), Expression(..), BinaryOp(..), UnaryOp(..), AtomicExpression(..), Variable, parseProgram) where

import Lexer ( Token(..) )

import Debug.Trace( trace )


type Program = [Definition]
data Definition = Definition Variable [Variable] Expression deriving Show
data LocalDefinition = LocalDef Variable Expression deriving Show
data Expression = Let [LocalDefinition] Expression
                | If Expression Expression Expression
                | Binary Expression BinaryOp Expression
                | Unary UnaryOp Expression
                | FuncApp Expression Expression
                | AtomicExpr AtomicExpression deriving Show
data BinaryOp = Equal | Smaller | Plus | Minus | Times | Divide deriving (Eq, Show)
data UnaryOp = Not | Neg deriving (Eq, Show)
data AtomicExpression = Var Variable | Number Integer | TruthValue Bool deriving Show
type Variable = String


parseAtomicExpr :: [Token] -> (AtomicExpression, [Token])
parseAtomicExpr (NumberToken t : ts) = (Number t, ts)
parseAtomicExpr (NameToken t : ts) = (Var t, ts)
parseAtomicExpr (KeywordToken "true" : ts) = (TruthValue True, ts)
parseAtomicExpr (KeywordToken "false" : ts) = (TruthValue False, ts)

-- Parses parentheses and atomic expressions
parseExpr9 :: [Token] -> (Expression, [Token])
parseExpr9 (KeywordToken "(" : ts) = let
    (e, KeywordToken ")" : ts') = parseExpr0 ts 
    in (e, ts')
parseExpr9 ts = let
    (e, ts') = parseAtomicExpr ts 
    in (AtomicExpr e, ts')


startSymbols = ["(", "true", "false"]
isExprStart :: Token -> Bool
isExprStart (KeywordToken t) = t `elem` startSymbols
isExprStart (NumberToken t) = True
isExprStart (NameToken t) = True 

parseManyExpr9 :: [Token] -> ([Expression], [Token])
parseManyExpr9 [] = ([], [])
parseManyExpr9 tok@(t:ts) = if isExprStart t then
                                let (e, ts') = parseExpr9 tok
                                    (es, rest) = parseManyExpr9 ts'
                                in (e:es, rest)
                            else ([], tok)

-- Parses function application
parseExpr8 :: [Token] -> (Expression, [Token])
parseExpr8 ts = let 
    (e:es, ts') = parseManyExpr9 ts
    in (foldl FuncApp e es, ts')


-- Parses unary minus
parseExpr7 :: [Token] -> (Expression, [Token])
parseExpr7 (KeywordToken "-" : ts) = let
    (e, rest) = parseExpr8 ts
    in (Unary Neg e, rest)
parseExpr7 ts = parseExpr8 ts


parseMultiplications :: [Token] -> ([Expression], [Token])
parseMultiplications (KeywordToken "*" : ts) = 
    let 
    (e, ts') = parseExpr7 ts
    (es, rest) = parseMultiplications ts'
    in (e:es, rest)
parseMultiplications ts = ([], ts)

-- Parses * and / operators
parseExpr6 :: [Token] -> (Expression, [Token])
parseExpr6 ts = let
    (e, ts') = parseExpr7 ts
    in case ts' of
        (KeywordToken "/" : ts') -> let 
            (divisor, rest) = parseExpr7 ts'
            in (Binary e Divide divisor, rest)
        _ -> let
            (es, rest) = parseMultiplications ts'
            in (foldl (\x y -> Binary x Times y) e es, rest)


parseAdditions :: [Token] -> ([Expression], [Token])
parseAdditions (KeywordToken "+" : ts) = 
    let 
    (e, ts') = parseExpr6 ts
    (es, rest) = parseAdditions ts'
    in (e:es, rest)
parseAdditions ts = ([], ts)

-- Parses + and - operators
parseExpr5 :: [Token] -> (Expression, [Token])
parseExpr5 ts = let
    (e, ts') = parseExpr6 ts
    in case ts' of
        (KeywordToken "-" : ts') -> let 
            (subtrahend, rest) = parseExpr6 ts'
            in (Binary e Minus subtrahend, rest)
        _ -> let
            (es, rest) = parseAdditions ts'
            in (foldl (\x y -> Binary x Plus y) e es, rest)


-- Parses == and < operators
parseExpr4 :: [Token] -> (Expression, [Token])
parseExpr4 ts = let
    (e1, ts') = parseExpr5 ts
    in case ts' of
        (KeywordToken "==" : ts')  -> let 
                (e2, rest) = parseExpr5 ts'
                in (Binary e1 Equal e2, rest)
        (KeywordToken "<" : ts')  -> let 
                (e2, rest) = parseExpr5 ts'
                in (Binary e1 Smaller e2, rest)
        _ -> (e1, ts')


-- Parses negation
parseExpr3 :: [Token] -> (Expression, [Token])
parseExpr3 (KeywordToken "not" : ts) = let
    (e, rest) = parseExpr4 ts
    in (Unary Not e, rest)
parseExpr3 ts = parseExpr4 ts


-- Parses And Operator
parseExpr2 :: [Token] -> (Expression, [Token])
parseExpr2 ts = let
    (e1, ts') = parseExpr3 ts
    in case ts' of
        (KeywordToken "&" : ts') -> let
            (e2, rest) = parseExpr2 ts'
            in (If e1 e2 (AtomicExpr $ TruthValue False), rest)
        _ -> (e1, ts')


-- Parses Or Operator
parseExpr1 :: [Token] -> (Expression, [Token])
parseExpr1 ts = let
    (e1, ts') = parseExpr2 ts
    in case ts' of
        (KeywordToken "|" : ts') -> let
            (e2, rest) = parseExpr1 ts'
            in (If e1 (AtomicExpr $ TruthValue True) e2, rest)
        _ -> (e1, ts')


-- Parses if-then-else and let expressions
parseExpr0 :: [Token] -> (Expression, [Token])
parseExpr0 (KeywordToken "if" : ts1) = let
    (e1, KeywordToken "then" : ts2) = parseExpr0 ts1
    (e2, KeywordToken "else" : ts3) = parseExpr0 ts2
    (e3, rest) = parseExpr0 ts3
    in (If e1 e2 e3, rest)
parseExpr0 (KeywordToken "let" : ts1) = let
    (localDefs, KeywordToken "in" : ts2) = parseLocalDefinitions ts1
    (expr, rest) = parseExpr0 $ ts2
    in (Let localDefs expr, rest)
parseExpr0 ts = parseExpr1 ts


-- Parses local definitions
parseLocalDefinitions :: [Token] -> ([LocalDefinition], [Token])
parseLocalDefinitions (NameToken t : KeywordToken "=" : ts) = case parseExpr0 ts of
    (expr, KeywordToken ";" : ts') -> let
        (definitions, rest) = parseLocalDefinitions ts'
        in (LocalDef t expr : definitions, rest)
    (expr, ts') -> ([LocalDef t expr], ts')


-- Parses definitions
parseDefinition :: [Token] -> (Definition, [Token])
parseDefinition (NameToken t : ts) =
    let
    vs = takeWhile (/= KeywordToken "=") ts
    KeywordToken "=" : ts' = dropWhile (/= KeywordToken "=") ts
    (e, rest) = parseExpr0 ts'
    in (Definition t [ v | NameToken v <- vs] e, rest)


-- Parses a program
parseProgram :: [Token] -> Program
parseProgram [] = []
parseProgram ts = let
    (d, KeywordToken ";" : rest) = parseDefinition ts
    in d : parseProgram rest