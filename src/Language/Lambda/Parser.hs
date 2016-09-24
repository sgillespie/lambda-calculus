module Language.Lambda.Parser (parseExpr) where

import Text.Parsec
import Text.Parsec.String

import Language.Lambda.Expression

parseExpr :: String -> Either ParseError (LambdaExpr String)
parseExpr = parse (expr <* eof) ""

expr :: Parser (LambdaExpr String)
expr = var <|> parens
  
var :: Parser (LambdaExpr String)
var = Var <$> identifier

parens :: Parser (LambdaExpr String)
parens = symbol '(' *> expr <* symbol ')'
  
identifier :: Parser String
identifier = lexeme $ (:) <$> first <*> many rest
  where first = letter <|> char '_'
        rest  = first <|> digit

symbol :: Char -> Parser Char
symbol = lexeme . char

lexeme :: Parser a -> Parser a
lexeme = (flip (<*)) . many $ whitespace
  where whitespace :: Parser Char
        whitespace = space <|> tab
