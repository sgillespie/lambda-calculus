module Language.Lambda.Parser (parseExpr) where

import Control.Monad
import Prelude hiding (abs, curry, id)

import Text.Parsec
import Text.Parsec.String

import Language.Lambda.Expression

parseExpr :: String -> Either ParseError (LambdaExpr String)
parseExpr = parse (whitespace *> expr <* eof) ""

expr :: Parser (LambdaExpr String)
expr = abs <|> var <|> parens
  
var :: Parser (LambdaExpr String)
var = Var <$> identifier

abs :: Parser (LambdaExpr String)
abs = curry <$> idents <*> expr
  where idents = (symbol '\\') *> many1 identifier <* (symbol '.')
        curry = flip (foldr Abs)

parens :: Parser (LambdaExpr String)
parens = symbol '(' *> expr <* symbol ')'

lexeme :: Parser a -> Parser a
lexeme p =  p <* whitespace

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \t"

identifier :: Parser String
identifier = lexeme ((:) <$> first <*> many rest)
  where first = letter <|> char '_'
        rest  = first <|> digit

symbol :: Char -> Parser ()
symbol = void . lexeme . char

