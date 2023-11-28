module UTLC.Eval.NbE
  where

import Prelude

import UTLC.Syntax.Term

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

import Control.Apply

import Data.Maybe
import Data.Tuple

import Data.List
import Data.Either

data EvalError = TooManySteps | UnknownName String

type StepCounter m = StateT Int m

type StdoutWriter = WriterT String

maxSteps :: Int
maxSteps = 10000

-- TODO: This probably needs a fresh name generator?
type Eval = StepCounter (StdoutWriter (Either EvalError))

data Value
  = VLam Abstraction
  | VNeutral Neutral

data Neutral
  = NVar String
  | NApp Neutral Value
  | NPrint

type Abstraction =
  { argName :: String
  , fn :: Value -> Eval Value
  }

runEval :: forall a. Eval a -> Either String (Tuple String a)
runEval m =
  case runWriterT $ flip runStateT 0 m of
    Left TooManySteps -> Left $ "Too many steps"
    Left (UnknownName x) -> Left $ "Unknown name " <> x
    Right (Tuple (Tuple r _) stdout) -> Right (Tuple stdout r)

normalize :: Env -> NamedTerm -> Eval NamedTerm
normalize env = reify env <=< eval env

eval :: Env -> NamedTerm -> Eval Value
eval env e = step *> eval' env e

eval' :: Env -> NamedTerm -> Eval Value
eval' env (Var x) =
  case lookup env x of
      Nothing -> pure $ VNeutral $ NVar x
      Just v -> pure v

eval' env (App a b) = do
  vA <- eval env a
  case vA of
    VNeutral n -> do
       vB <- eval env b
       pure $ VNeutral $ NApp n vB

    VLam abstr ->
      abstr.fn =<< eval env b

eval' env (Lam x body) = do
  pure $ VLam
    { argName: x
    , fn: \v -> eval (extend env x v) body
    }

eval' _env Print = pure $ VNeutral NPrint

reify :: Env -> Value -> Eval NamedTerm
reify env (VLam abstr) = do
  e <- reify env =<< abstr.fn (VNeutral (NVar abstr.argName))
  pure $ Lam abstr.argName e

reify env (VNeutral n) = reifyNeutral env n

reifyNeutral :: Env -> Neutral -> Eval NamedTerm
reifyNeutral _env (NVar x) = pure $ Var x
reifyNeutral env (NApp a b) = lift2 App (reifyNeutral env a) (reify env b)
reifyNeutral _env NPrint = pure Print

step :: Eval Unit
step = do
  n <- get
  if n >= maxSteps
    then throwError TooManySteps
    else modify_ (_ + 1)

type Env = List (Tuple String Value)

extend :: Env -> String -> Value -> Env
extend env name val = Cons (Tuple name val) env

lookup :: Env -> String -> Maybe Value
lookup Nil name = Nothing
lookup (Cons (Tuple x val) rest) name =
  if x == name
    then pure val
    else lookup rest name

