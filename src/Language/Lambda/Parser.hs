module Language.Lambda.Parser (parseExpr) where

import Text.Parsec
import Text.Parsec.String

import Language.Lambda.Expression

parseExpr :: String -> Either ParseError (LambdaExpr String)
parseExpr = parse expr ""

expr :: Parser (LambdaExpr String)
expr = var
  
var :: Parser (LambdaExpr String)
var = Var <$> identifier
  
identifier :: Parser String
identifier = lexeme $ (:) <$> first <*> many rest
  where first = letter <|> char '_'
        rest  = first <|> digit

lexeme :: Parser a -> Parser a
lexeme = (flip (<*)) . many whitespace
  where whitespace :: Parser Char
        whitespace = space <|> tab
