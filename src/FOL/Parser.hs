module FOL.Parser where

import           Control.Monad.Combinators.Expr
import           Data.Functor
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           FOL.Base
import           FOL.Parser.Util

lpar :: Parser String
lpar = symbol "("

rpar :: Parser String
rpar = symbol ")"

comma :: Parser String
comma = symbol ","

identifier :: Bool -> Parser Char -> Parser String
identifier prepend p = lexeme $ do
  c <- p
  many (choice [alphaNumChar, single '\'', single '_', single '-']) <&>
    if prepend then (c :) else id

constant :: Parser String
constant = identifier False $ single '_'

var :: Parser String
var = identifier False $ single ':'

function :: Parser String
function = identifier True lowerChar

term :: Parser Term
term = choice
  [ constant <&> Constant
  , var <&> Var
  , do
    func <- function
    lpar
    tl <- termList
    rpar
    return $ Function func tl
  ]

termList :: Parser [Term]
termList = term >>= termList' . (: [])
  where
    termList' :: [Term] -> Parser [Term]
    termList' ts = (do
        comma
        t <- term
        termList' $ t : ts
      ) <|> return (reverse ts)

neg :: Parser String
neg = symbol "!"

conj :: Parser String
conj = symbol "&"

disj :: Parser String
disj = symbol "|"

impl :: Parser String
impl = symbol "->"

fall :: Parser String
fall = symbol "@"

exst :: Parser String
exst = symbol "?"

dot :: Parser String
dot = symbol "."

top :: Parser String
top = symbol "true"

bottom :: Parser String
bottom = symbol "false"

predicate :: Parser String
predicate = identifier True upperChar

formula :: Parser FOL
formula = choice
  [ T <$ top
  , F <$ bottom
  , parens formulaFull
  , neg >> formula <&> Not
  , do
    fall
    v <- var
    dot
    formulaFull <&> Forall v
  , do
    exst
    v <- var
    dot
    formulaFull <&> Exists v
  , do
    pred <- predicate
    lpar
    tl <- termList
    rpar
    return $ Predicate pred tl
  ]

opTable :: [[Operator Parser FOL]]
opTable =
  [ [ infixL "&" (:&) ]
  , [ infixL "|" (:|) ]
  , [ infixR "->" (:>) ]
  ]

formulaFull :: Parser FOL
formulaFull = makeExprParser formula opTable

parse :: String -> Either (ParseErrorBundle String Void) FOL
parse = runParser (space *> formulaFull <* eof) ""
