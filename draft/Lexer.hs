module Lexer (Token (..), tokenize) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

data Token = NumberToken Int Int Integer | NameToken Int Int String | KeywordToken Int Int String deriving (Show)

symbols = ['(', ')', '&', '|', '<', '+', '-', '*', '/', ';']

keywords = ["let", "in", "if", "then", "else", "not", "true", "false"]

-- Add position to each character
-- tab ('\t') occupies one column
addCharPositions :: String -> Int -> Int -> [(Int, Int, Char)]
addCharPositions "" _ _ = []
addCharPositions ('\n' : cs) ln col = (ln, col, '\n') : addCharPositions cs (ln + 1) 1
addCharPositions (c : cs) ln col = (ln, col, c) : addCharPositions cs ln (col + 1)

isIdentifierChar :: (Int, Int, Char) -> Bool
isIdentifierChar (_, _, c) = isAlphaNum c || c == '_'

-- Tokenize a string annotated with positions into a list of tokens annotated with positions
tokenizeCharsWithPos :: [(Int, Int, Char)] -> [Token]
tokenizeCharsWithPos [] = []
tokenizeCharsWithPos ((ln, col, c) : cs)
  | isSpace c = tokenizeCharsWithPos cs
  | isAlpha c =
    let tokenWithPos = (ln, col, c) : takeWhile isIdentifierChar cs
        token = [c | (_, _, c) <- tokenWithPos]
        rest = dropWhile isIdentifierChar cs
     in if token `elem` keywords
          then KeywordToken ln col token : tokenizeCharsWithPos rest
          else NameToken ln col token : tokenizeCharsWithPos rest
  | isDigit c =
    let tokenWithPos = (ln, col, c) : takeWhile (\(_, _, c) -> isDigit c) cs
        token = [c | (_, _, c) <- tokenWithPos]
        rest = dropWhile (\(_, _, c) -> isDigit c) cs
     in NumberToken ln col (read token) : tokenizeCharsWithPos rest
  | c `elem` symbols = KeywordToken ln col [c] : tokenizeCharsWithPos cs
  | c == '=' = case cs of
    (_, _, '=') : cs' -> KeywordToken ln col "==" : tokenizeCharsWithPos cs'
    _ -> KeywordToken ln col "=" : tokenizeCharsWithPos cs
  | otherwise = error $ "Invalid input " ++ show c ++ " at position " ++ show (ln, col) ++ "."

tokenize :: String -> [Token]
tokenize s = tokenizeCharsWithPos $ addCharPositions s 0 0