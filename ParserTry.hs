import Data.Char ( isAlpha, isDigit, isSpace )


data Token = NumberToken Integer | NameToken String | KeywordToken String deriving Eq

symbols = ['(', ')', '&', '|', '<', '+', '-', '*', '/', ';']
keywords = ["if", "then", "else", "not", "true", "false"]

tokenize :: String -> [Token]
tokenize "" = []
tokenize (c:s) 
    | isSpace c = tokenize s
    | isAlpha c = let token = c : takeWhile isAlpha s 
                      rest =  dropWhile isAlpha s 
                  in if token `elem` keywords
                      then  KeywordToken token : tokenize rest
                      else NameToken token : tokenize rest
    | isDigit c = NumberToken (read(c : takeWhile isDigit s)) : tokenize (dropWhile isDigit s)
    | c `elem` symbols = KeywordToken [c] : tokenize s
    | c == '=' = case s of
        '=':s' -> KeywordToken "==" : tokenize s'
        _ -> KeywordToken "=" : tokenize s

type Program = [Definition]
data Definition = Definition Variable [Variable] Expression deriving Show
data Expression = If Expression Expression Expression
                | Binary Expression BinaryOp Expression
                | Unary UnaryOp Expression
                | FuncApp Expression Expression
                | AtomicExpr AtomicExpression deriving Show
data BinaryOp = And | Or | Equal | Smaller | Plus | Minus | Times | Divide deriving (Eq, Show)
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

-- Parses * and / operators
parseExpr7 :: [Token] -> (Expression, [Token])
parseExpr7 ts = parseExpr8 ts -- TODO

-- Parses + and - operators
parseExpr6 :: [Token] -> (Expression, [Token])
parseExpr6 ts = parseExpr7 ts -- TODO

-- Parses unary minus
parseExpr5 :: [Token] -> (Expression, [Token])
parseExpr5 ts = parseExpr6 ts -- TODO

-- Parses == and < operators
parseExpr4 :: [Token] -> (Expression, [Token])
parseExpr4 ts = parseExpr5 ts -- TODO

-- Parses negation
parseExpr3 :: [Token] -> (Expression, [Token])
parseExpr3 ts = parseExpr4 ts -- TODO

-- Parses And Operator
parseExpr2 :: [Token] -> (Expression, [Token])
parseExpr2 ts = parseExpr3 ts -- TODO

-- Parses Or Operator
parseExpr1 :: [Token] -> (Expression, [Token])
parseExpr1 ts = parseExpr2 ts -- TODO

-- Parses if-then-else expression
parseExpr0 :: [Token] -> (Expression, [Token])
parseExpr0 ts = parseExpr1 ts -- TODO