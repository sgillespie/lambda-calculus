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
expr = try tyapp <|> try app <|> term

app :: Parser (SystemFExpr String String)
app = chainl1 term (return App)

tyapp :: Parser (SystemFExpr String String)
tyapp = TyApp
      <$> term
      <*> ty'
  where ty' = symbol '[' *> ty <* symbol ']'

term :: Parser (SystemFExpr String String)
term = try abs <|> tyabs <|> var <|> parens expr

var :: Parser (SystemFExpr String String)
var = Var <$> exprId

abs :: Parser (SystemFExpr String String)
abs = curry 
    <$> (symbol '\\' *> many1 args <* symbol '.') 
    <*> expr
  where args = (,) <$> (exprId <* symbol ':') <*> ty
        curry = flip . foldr . uncurry $ Abs

tyabs :: Parser (SystemFExpr String String)
tyabs = curry <$> args <*> expr
  where args = symbol '\\' *> many1 typeId <* symbol '.'
        curry = flip (foldr TyAbs)

-- Parse type expressions
ty :: Parser (Ty String)
ty = try arrow

arrow :: Parser (Ty String)
arrow = chainr1 tyterm (symbol' "->" *> return TyArrow)

tyterm :: Parser (Ty String)
tyterm = tyvar <|> parens ty

tyvar :: Parser (Ty String)
tyvar = TyVar <$> typeId

parens :: Parser a -> Parser a
parens p = symbol '(' *> p <* symbol ')'

identifier :: Parser Char -> Parser String
identifier firstChar = lexeme ((:) <$> first <*> many rest)
  where first = firstChar <|> char '_'
        rest = first <|> digit

typeId, exprId :: Parser String
typeId = identifier upper
exprId = identifier lower

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \t"

symbol :: Char -> Parser ()
symbol = void . lexeme . char

symbol' :: String -> Parser ()
symbol' = void . lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
