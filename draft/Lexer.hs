module Lexer (Token(..), Symbol(..), Keyword(..), tokenize) where

import Data.Char ( isAlpha, isDigit, isSpace )

data Keyword = If | Then | Else | Not | True | False
    deriving (Show, Eq)

data Symbol = Plus | Minus | Times | Div | And | Or | LessThen | BracketA | BracketB | Semicolon | Equal | Assignment
    deriving (Show, Eq)

data Token = NumberToken Integer | SymbolToken Symbol | KeywordToken Keyword
    deriving (Show, Eq)

operator :: String -> Token
operator c | c == "+"       = SymbolToken Plus
           | c == "-"       = SymbolToken Minus
           | c == "*"       = SymbolToken Times
           | c == "/"       = SymbolToken Div
           | c == "("       = SymbolToken BracketA
           | c == ")"       = SymbolToken BracketB
           | c == ";"       = SymbolToken Semicolon
           | c == "<"       = SymbolToken LessThen
           | c == "&"       = SymbolToken And
           | c == "|"       = SymbolToken Or
           | c == "="       = SymbolToken Assignment
           | c == "=="      = SymbolToken Equal
           | c == "if"      = KeywordToken Lexer.If
           | c == "then"    = KeywordToken Lexer.Then
           | c == "else"    = KeywordToken Lexer.Else
           | c == "not"     = KeywordToken Lexer.Not
           | c == "true"    = KeywordToken Lexer.True
           | c == "false"   = KeywordToken Lexer.False

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
                  in operator token : tokenize rest
    | isDigit c = NumberToken (read(c : takeWhile isDigit s)) : tokenize (dropWhile isDigit s)
    | c `elem` symbols = operator [c] : tokenize s
    | c == '=' = case s of
        '=':s' -> SymbolToken Equal : tokenize s'
        _ -> SymbolToken Assignment : tokenize s