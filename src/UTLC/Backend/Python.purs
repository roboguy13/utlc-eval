module UTLC.Backend.Python
  where

import Prelude

import Data.List

import UTLC.Syntax.Term
import UTLC.Utils
import UTLC.Syntax.Name (Name)

toPythonVar :: String -> String
toPythonVar x = "_" <> x

toPythonExpr :: List Name -> NamedTerm -> String
toPythonExpr env (Var x) =
  if x `elem` env
  then toPythonVar x
  else show x
toPythonExpr env (Lam x body) =
  "lambda " <> toPythonVar x <> ": " <> toPythonExpr (Cons x env) body
toPythonExpr env (App x y) =
  "(" <> toPythonExpr env x <> ")(" <> toPythonExpr env y <> ")"
toPythonExpr _env Print = "printImpl"

toPythonDef :: List Name -> NamedDef -> String
toPythonDef env (Def def) = toPythonVar def.name <> " = " <> toPythonExpr env def.body

toPython :: List NamedDef -> NamedTerm -> String
toPython defs term =
  unlines (toUnfoldable (map (toPythonDef defNames) defs))
    <> "\n" <> toPythonExpr defNames term
  where
    defNames = map (\(Def d) -> d.name) defs

pythonPrelude :: String
pythonPrelude =
  unlines
    [ "def printImpl(x):"
    , "  def result(y):"
    , "    print(x)"
    , "    return y"
    , "  return result"
    ]

