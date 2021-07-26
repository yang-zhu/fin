module Lexer (Token (..), tokenize) where

import Data.Char (isAlpha, isDigit, isSpace, isAlphaNum)

data Token = NumberToken Integer | NameToken String | KeywordToken String deriving (Eq, Show)

symbols = ['(', ')', '&', '|', '<', '+', '-', '*', '/', ';']

keywords = ["let", "in", "if", "then", "else", "not", "true", "false"]

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

-- Tokenize a string into a list of tokens
tokenize :: String -> [Token]
tokenize "" = []
tokenize (c : s)
  | isSpace c = tokenize s
  | isAlpha c =
    let token = c : takeWhile isIdentifierChar s
        rest = dropWhile isIdentifierChar s
     in if token `elem` keywords
          then KeywordToken token : tokenize rest
          else NameToken token : tokenize rest
  | isDigit c = NumberToken (read (c : takeWhile isDigit s)) : tokenize (dropWhile isDigit s)
  | c `elem` symbols = KeywordToken [c] : tokenize s
  | c == '=' = case s of
    '=' : s' -> KeywordToken "==" : tokenize s'
    _ -> KeywordToken "=" : tokenize s
  | otherwise = error $ "Invalid input: " ++ (c:s)