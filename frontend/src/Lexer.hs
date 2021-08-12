module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

data Token = NumberToken Int Int Integer | NameToken Int Int String | KeywordToken Int Int String

type LexerError = String

instance Show Token where
  show (NumberToken _ _ t) = "NumberToken " ++ show t
  show (NameToken _ _ t) = "NameToken " ++ show t
  show (KeywordToken _ _ t) = "KeywordToken " ++ show t

tokenToStr :: Token -> String
tokenToStr (NumberToken _ _ t) = show t
tokenToStr (NameToken _ _ t) = show t
tokenToStr (KeywordToken _ _ t) =  show t

getTokenPos :: Token -> (Int, Int)
getTokenPos (NumberToken ln col _) = (ln, col)
getTokenPos (NameToken ln col _) = (ln, col)
getTokenPos (KeywordToken ln col _) = (ln, col)

symbols :: [Char]
symbols = ['(', ')', '&', '|', '<', '+', '-', '*', '/', ';']

keywords :: [[Char]]
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
tokenizeCharsWithPos :: [(Int, Int, Char)] -> Either LexerError [Token]
tokenizeCharsWithPos [] = return []
tokenizeCharsWithPos ((ln, col, c) : cs)
  | isSpace c = tokenizeCharsWithPos cs
  | isAlpha c =
    let (tokenWithPos, rest) = span isIdentifierChar cs
        token = c : [c | (_, _, c) <- tokenWithPos]
     in if token `elem` keywords
          then do
            tokens <- tokenizeCharsWithPos rest
            return $ KeywordToken ln col token : tokens
          else do
            tokens <- tokenizeCharsWithPos rest
            return $ NameToken ln col token : tokens
  | isDigit c =
    let (tokenWithPos, rest) = span (\(_, _, c) -> isDigit c) cs
        token = c : [c | (_, _, c) <- tokenWithPos]
     in do
          tokens <- tokenizeCharsWithPos rest
          return $ NumberToken ln col (read token) : tokens
  | c `elem` symbols = do
    tokens <- tokenizeCharsWithPos cs
    return $ KeywordToken ln col [c] : tokens
  | c == '=' = case cs of
    (_, _, '=') : cs' -> do
      tokens <- tokenizeCharsWithPos cs'
      return $ KeywordToken ln col "==" : tokens
    _ -> do
      tokens <- tokenizeCharsWithPos cs
      return $ KeywordToken ln col "=" : tokens
  | otherwise = Left $ "Invalid input " ++ show c ++ " at position " ++ show (ln, col) ++ "."

tokenize :: String -> Either LexerError [Token]
tokenize s = tokenizeCharsWithPos $ addCharPositions s 0 0