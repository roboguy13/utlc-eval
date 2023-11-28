module UTLC.Syntax.Term
  where

import Prelude

import Parsing as Parsing
import Parsing.String
import Parsing.Token
import Parsing.Language
import Parsing.Combinators
import Parsing.Expr

import Control.Lazy

data Term a
  = Var a
  | Lam a (Term a)
  | App (Term a) (Term a)
  | Print

data Def a =
  Def
  { name :: String
  , body :: Term a
  }

type NamedTerm = Term String
type NamedDef = Def String

showTerm :: NamedTerm -> String
showTerm (Var x) = x
showTerm (Lam x body) = "\\" <> x <> ". " <> showTerm body
showTerm (App a b) = showParens a <> " " <> showTerm b
  where
    showParens e =
      if isNested e
        then "(" <> showTerm e <> ")"
        else showTerm e

    isNested (Var _) = false
    isNested (Lam _ _) = true
    isNested (App _ _) = true
    isNested Print = false
showTerm Print = "print"

showDef :: NamedDef -> String
showDef (Def d) = d.name <> " := " <> showTerm d.body

parseName :: Parser String
parseName = lexeme identifier

parseTerm' :: Parser NamedTerm
parseTerm' = defer \_ ->
  try parsePrint
  <|> try parseVar
  <|> try (symbol "(" *> parseTerm <* symbol ")")

parseTerm :: Parser NamedTerm
parseTerm = defer \_ ->
  try parseLam
  <|> try parseApp
  <|> try parseTerm'

parsePrint :: Parser NamedTerm
parsePrint = lexeme $ keyword "print" $> Print

parseLam :: Parser NamedTerm
parseLam = lexeme do
  _ <- symbol "\\"
  x <- identifier
  _ <- symbol "."
  body <- parseTerm
  pure $ Lam x body

parseVar :: Parser NamedTerm
parseVar = lexeme $ Var <$> identifier

parseApp :: Parser NamedTerm
parseApp = lexeme $ defer \_ -> do
  f <- parseTerm'
  arg <- parseTerm'
  pure $ App f arg

parseDef :: Parser NamedDef
parseDef = lexeme do
  name <- identifier
  body <- parseTerm
  pure $ Def { name: name, body: body }

tokenParser = makeTokenParser (LanguageDef ((unGenLanguageDef haskellStyle) { reservedNames = keywords }))
identifier = tokenParser.identifier

keywords :: Array String
keywords = ["print"]

type Parser = Parsing.Parser String

lexeme = tokenParser.lexeme
keyword = tokenParser.reserved
symbol = tokenParser.symbol
