module Parser where

import Lexer (Token (KeywordToken, NumberToken, SymbolToken), Keyword (If), Symbol)
import Data.Maybe ( isNothing )
import Data.Tree ( Tree )

type Parser = [Token] -> [(Tree Token, [Token])]

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

-- helper functions

lookAhead :: [Token] -> Maybe Token
lookAhead []     = Nothing
lookAhead (t:ts) = Just t

consume :: [Token] -> [Token]
consume []     = []
consume (t:ts) = ts

addDown :: a -> Tree a -> Tree a
addDown a t = t --TODO: add Variable in the right tree position

-- production functions

-- main parse function: gets a Tree of the allready evaluated values and a List of not evaluated Tokens,
-- returns the Tree with one or more Tokens added and a List with the not evaluated Tokens
parse :: (Tree Token, [Token]) -> Maybe (Tree Token, [Token])
parse t = case head $ snd t of
    KeywordToken k  -> keywordParse t
    NumberToken i   -> numberParse t
    SymbolToken s   -> symbolParse t


keywordParse :: (Tree Token, [Token]) -> Maybe (Tree Token, [Token])
keywordParse t = Just t -- TODO: handle keywords

numberParse :: (Tree Token, [Token]) -> Maybe (Tree Token, [Token])
numberParse t = case head $ snd t of
    KeywordToken k  -> Nothing -- no correct Syntax 
    NumberToken n   -> Nothing -- no correct Syntax
    _               -> if isNothing (lookAhead $ tail $ snd t)
                        then Just t
                        else parse (addDown (head $ snd t) (fst t), consume $ tail $ snd t)

symbolParse :: (Tree Token, [Token]) -> Maybe (Tree Token, [Token])
symbolParse t = Just t --TODO: handle Symbols
