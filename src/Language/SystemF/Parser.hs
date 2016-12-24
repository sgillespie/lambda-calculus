module Language.SystemF.Parser (parseExpr) where

import Control.Monad
import Prelude hiding (abs)

import Text.Parsec
import Text.Parsec.String

import Language.SystemF.Expression

parseExpr :: String -> Either ParseError (SystemFExpr String String)
parseExpr = parse (whitespace *> expr <* eof) ""

expr :: Parser (SystemFExpr String String)
expr = try app <|> term

app :: Parser (SystemFExpr String String)
app = chainl1 term (return App)

term :: Parser (SystemFExpr String String)
term = var <|> parens

var :: Parser (SystemFExpr String String)
var = Var <$> identifier

parens :: Parser (SystemFExpr String String)
parens = symbol '(' *> expr <* symbol ')'

identifier :: Parser String
identifier = lexeme ((:) <$> first <*> many rest)
  where first = letter <|> char '_'
        rest = first <|> digit

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \t"

symbol :: Char -> Parser ()
symbol = void . lexeme . char

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
