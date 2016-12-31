module Language.SystemF.Parser (
  parseExpr,
  parseType
  ) where

import Control.Monad
import Prelude hiding (abs)

import Text.Parsec
import Text.Parsec.String

import Language.SystemF.Expression

parseExpr :: String -> Either ParseError (SystemFExpr String String)
parseExpr = parse (whitespace *> expr <* eof) ""

parseType :: String -> Either ParseError (Ty String)
parseType = parse (whitespace *> ty <* eof) ""

-- Parse expressions
expr :: Parser (SystemFExpr String String)
expr = try app <|> term

app :: Parser (SystemFExpr String String)
app = chainl1 term (return App)

term :: Parser (SystemFExpr String String)
term = abs <|> var <|> parens expr

var :: Parser (SystemFExpr String String)
var = Var <$> identifier

abs :: Parser (SystemFExpr String String)
abs = curry 
    <$> (symbol '\\' *> many1 args <* symbol '.') 
    <*> expr
  where args = (,) <$> (identifier <* symbol ':') <*> ty
        curry = flip . foldr . uncurry $ Abs

-- Parse type expressions
ty :: Parser (Ty String)
ty = try arrow

arrow :: Parser (Ty String)
arrow = chainr1 tyterm (symbol' "->" *> return TyArrow)

tyterm :: Parser (Ty String)
tyterm = tyvar <|> parens ty

tyvar :: Parser (Ty String)
tyvar = TyVar <$> identifier

parens :: Parser a -> Parser a
parens p = symbol '(' *> p <* symbol ')'

identifier :: Parser String
identifier = lexeme ((:) <$> first <*> many rest)
  where first = letter <|> char '_'
        rest = first <|> digit

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \t"

symbol :: Char -> Parser ()
symbol = void . lexeme . char

symbol' :: String -> Parser ()
symbol' = void . lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
