module FOL.Parser.Util where

import           Control.Monad.Combinators.Expr
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

infixL :: String -> (a -> a -> a) -> Operator Parser a
infixL name f = InfixL (f <$ symbol name)

infixR :: String -> (a -> a -> a) -> Operator Parser a
infixR name f = InfixR (f <$ symbol name)

prefix :: String -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)

postfix :: String -> (a -> a) -> Operator Parser a
postfix name f = Postfix (f <$ symbol name)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
