module Lexer (Token(..), tokenize) where

import Data.Char ( isAlpha, isDigit, isSpace )

data Operator = Plus | Minus | Times | Div | And | Or | LessThen | BracketA | BracketB
    | Semicolon | If | Then | Else | Not | True | False | Equal | Assignment
    deriving (Show, Eq)

data Token = NumberToken Integer | NameToken Operator | KeywordToken Operator deriving (Show, Eq)

operator :: String -> Operator
operator c | c == "+"       = Plus
           | c == "-"       = Minus
           | c == "*"       = Times
           | c == "/"       = Div
           | c == "("       = BracketA
           | c == ")"       = BracketB
           | c == ";"       = Semicolon
           | c == "<"       = LessThen
           | c == "&"       = And
           | c == "|"       = Or
           | c == "="       = Assignment
           | c == "=="      = Equal
           | c == "if"      = Lexer.If
           | c == "then"    = Then
           | c == "else"    = Else
           | c == "not"     = Not
           | c == "true"    = Lexer.True
           | c == "false"   = Lexer.False

symbols :: [Char]
symbols = ['(', ')', '&', '|', '<', '+', '-', '*', '/', ';']
keywords :: [String]
keywords = ["if", "then", "else", "not", "true", "false"]

tokenize :: String -> [Token]
tokenize "" = []
tokenize (c:s) 
    | isSpace c = tokenize s
    | isAlpha c = let token = c : takeWhile isAlpha s 
                      rest =  dropWhile isAlpha s 
                  in if token `elem` keywords
                      then  KeywordToken (operator token) : tokenize rest
                      else NameToken (operator token) : tokenize rest
    | isDigit c = NumberToken (read(c : takeWhile isDigit s)) : tokenize (dropWhile isDigit s)
    | c `elem` symbols = KeywordToken (operator [c]) : tokenize s
    | c == '=' = case s of
        '=':s' -> KeywordToken Equal : tokenize s'
        _ -> KeywordToken Assignment : tokenize s