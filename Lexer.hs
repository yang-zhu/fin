module Lexer (Token(..), tokenize) where

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