module UTLC.Syntax.Term
  where

import Prelude

import Parsing as Parsing
import Parsing.String
import Parsing.Token
import Parsing.Language
import Parsing.Combinators
import Parsing.Expr

import Data.Traversable

import Data.Maybe
import Data.Array
import Data.Array as Array
import Data.List hiding (some)

import Control.Lazy

import Bound

import UTLC.Syntax.Name
import UTLC.Utils

data Term a
  = Var a
  | Lam String (Term a)
  | App (Term a) (Term a)
  | Print

data Def a =
  Def
  { name :: String
  , body :: Term a
  }

derive instance Foldable Term
derive instance Traversable Term

derive instance Functor Term

type NamedTerm = Term String
type NamedDef = Def String

type IxTerm = Term IxV
type IxDef = Def IxV

findDef :: List NamedDef -> String -> Maybe NamedDef
findDef Nil _ = Nothing
findDef (Cons (Def d) ds) x =
  if d.name == x
  then Just $ Def d
  else findDef ds x

defFromNamed :: List NamedDef -> NamedDef -> IxDef
defFromNamed defs (Def def) = Def $ def { body = fromNamed defs def.body }

defToNamed :: IxDef -> NamedDef
defToNamed (Def def) = Def $ def { body = toNamed def.body }

fromNamed :: List NamedDef -> NamedTerm -> IxTerm
fromNamed defs = go emptyNamingCtx
  where
    go :: NamingCtx -> NamedTerm -> IxTerm
    go nCtx (Var name) =
      case nameToIx_maybe nCtx name of
        Just i -> Var (BV (IxName { name: name, ix: i }))
        Nothing ->
          case findDef defs name of
            Just (Def def) -> fromNamed defs $ def.body
            Nothing -> Var $ FV name
    go nCtx (App x y) = App (go nCtx x) (go nCtx y)
    go nCtx (Lam x body) =
      let nCtx' = liftNamingCtx x nCtx
      in
      Lam x (go nCtx' body)
    go _nCtx Print = Print

toNamed :: IxTerm -> NamedTerm
toNamed = go mempty
  where
    go :: List Name -> IxTerm -> NamedTerm
    go _ctx (Var (FV x)) = Var x
    go ctx (Var (BV ixName@(IxName n))) =
      Var $
      case lookupIx_maybe ctx n.ix of
          Nothing -> showIxName ixName
          Just name -> name
    go ctx (Lam x0 body) =
      let x = fresh x0 ctx
      in
      Lam x $ go (Cons x ctx) body
    go ctx (App x y) = App (go ctx x) (go ctx y)
    go _ctx Print = Print

showTerm :: NamedTerm -> String
showTerm (Var x) = x
showTerm (Lam x body) = "\\" <> x <> ". " <> showTerm body
showTerm (App a b) = showFn a <> " " <> showParens b
  where
    showParens e =
      if isNested e
        then parens $ showTerm e
        else showTerm e

    showFn e@(Lam _ _) = parens (showTerm e)
    showFn e = showTerm e

    parens s = "(" <> s <> ")"

    isNested (Var _) = false
    isNested (Lam _ _) = true
    isNested (App _ _) = true
    isNested Print = false
showTerm Print = "print"

showDef :: NamedDef -> String
showDef (Def d) = d.name <> " := " <> showTerm d.body <> ";"

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
  args <- some parseTerm'
  pure $ Array.foldl App f args

parseDef :: Parser NamedDef
parseDef = lexeme do
  name <- identifier
  _ <- symbol ":="
  body <- parseTerm
  _ <- symbol ";"
  pure $ Def { name: name, body: body }

tokenParser = makeTokenParser (LanguageDef ((unGenLanguageDef haskellStyle) { reservedNames = keywords }))
identifier = tokenParser.identifier

keywords :: Array String
keywords = ["print"]

type Parser = Parsing.Parser String

lexeme = tokenParser.lexeme
keyword = tokenParser.reserved
symbol = tokenParser.symbol
