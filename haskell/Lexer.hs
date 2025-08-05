module Lexer where

import Data.Char (isAlpha, isDigit, isSpace)

-- Token types for the toy language
data Token
  = TIntLit String
  | TIdent String
  | TKeyword String
  | TOperator String
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TLBracket
  | TRBracket
  | TSemicolon
  | TDot
  | TComma
  | TEOF
  deriving (Show, Eq)

-- Main lexer function
lexer :: String -> [Token]
lexer [] = [TEOF]
lexer (c : cs)
  | isSpace c = lexer cs
  | isDigit c = let (num, rest) = span isDigit (c : cs) in TIntLit num : lexer rest
  | isAlpha c =
      let (ident, rest) = span (\x -> isAlpha x || isDigit x) (c : cs)
       in case ident of
            "func" -> TKeyword "func" : lexer rest
            "int" -> TKeyword "int" : lexer rest
            "bool" -> TKeyword "bool" : lexer rest
            "true" -> TKeyword "true" : lexer rest
            "false" -> TKeyword "false" : lexer rest
            "if" -> TKeyword "if" : lexer rest
            "else" -> TKeyword "else" : lexer rest
            "while" -> TKeyword "while" : lexer rest
            "for" -> TKeyword "for" : lexer rest
            "print" -> TKeyword "print" : lexer rest
            "return" -> TKeyword "return" : lexer rest
            _ -> TIdent ident : lexer rest
  | c `elem` "+-*/=!" = TOperator [c] : lexer cs
  | c == '(' = TLParen : lexer cs
  | c == ')' = TRParen : lexer cs
  | c == '{' = TLBrace : lexer cs
  | c == '}' = TRBrace : lexer cs
  | c == '[' = TLBracket : lexer cs
  | c == ']' = TRBracket : lexer cs
  | c == ';' = TSemicolon : lexer cs
  | c == '.' = TDot : lexer cs
  | c == ',' = TComma : lexer cs
  | otherwise = error $ "Invalid character: " ++ [c]
