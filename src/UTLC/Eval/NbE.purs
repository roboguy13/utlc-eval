module UTLC.Eval.NbE
  where

import Prelude

import UTLC.Syntax.Term
import UTLC.Syntax.Name

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

import Control.Apply

import Data.Maybe
import Data.Traversable
import Data.Tuple
import Data.String as String

import Data.List
import Data.Either

import Data.Enum

import Bound

data EvalError = TooManySteps | UnknownName String

type StepCounter m = StateT Int m

type StdoutWriter = WriterT (List String)

maxSteps :: Int
maxSteps = 1500

-- TODO: This probably needs a fresh name generator?
type Eval = StepCounter (StdoutWriter (Either EvalError))

data Value
  = VLam Abstraction
  | VNeutral Neutral

data Neutral
  = NVar LvlV
  | NApp Neutral Value
  | NPrint Neutral Value

type Abstraction =
  { argName :: String
  , fn :: Value -> Eval Value
  }

runEval :: forall a. Eval a -> Either String (Tuple (List String) a)
runEval m =
  case runWriterT $ flip runStateT 0 m of
    Left TooManySteps -> Left $ "Too many steps"
    Left (UnknownName x) -> Left $ "Unknown name " <> x
    Right (Tuple (Tuple r _) stdout) -> Right (Tuple stdout r)

-- normalizeWithDefs :: List IxDef -> IxTerm -> Eval IxTerm
-- normalizeWithDefs defs term = do
--   env <- extendWithDefs mempty defs
--   normalize env term

normalize :: Env -> IxTerm -> Eval IxTerm
normalize env = reify initialLevel <=< eval env

class ToValue a where
  toValue :: Env -> a -> Eval Value

instance ToValue Value where
  toValue _ x = pure x

instance ToValue a => ToValue (Term a) where
  toValue env e = step *> eval' env e

instance ToValue IxName where
  toValue env (IxName n) = pure $ lookupIx env n.ix

instance ToValue a => ToValue (V a) where
  toValue _env (FV x) = pure $ VNeutral $ NVar $ FV x
  toValue env (BV x) = toValue env x

-- instance ToValue String where
--   toValue env x =
--     case lookup env x of
--         Nothing -> pure $ VNeutral $ NVar x
--         Just v -> pure v

eval :: forall a. ToValue a => Env -> Term a -> Eval Value
eval = toValue

eval' :: forall a. ToValue a => Env -> Term a -> Eval Value
eval' env (Var x) = toValue env x

eval' env (App a b) = do
  vA <- eval env a
  case vA of
    VNeutral n -> do
       vB <- eval env b

       case n of
         -- NPrint -> do
         --    vT <- reify env vB
         --    tell $ singleton (showTerm vT)
         --    pure $ VNeutral $ NApp n vB
         --    -- pure vB  -- 'print' gives back its argument
         _ -> pure $ VNeutral $ NApp n vB


    VLam abstr ->
      abstr.fn =<< eval env b

eval' env (Lam x body) = do
  pure $ VLam
    { argName: x
    , fn: \v -> eval (extend env v) body
    }

eval' env Print = do
  let x1 = "x1" -- fresh env
  let x2 = "x2" -- fresh (extend env x1 (VNeutral (NVar x1)))
  pure $ VLam
    { argName: x1
    , fn: \v1 -> pure $ VLam
        { argName: x2
        , fn: \v2 -> evalPrint env v1 v2
        }
    }

evalPrint :: Env -> Value -> Value -> Eval Value
evalPrint env val cont = do
  valTerm <- reify (mkLevel env) val
  tell $ singleton $ showTerm $ toNamed valTerm
  pure cont

reify :: Lvl -> Value -> Eval IxTerm
reify depth (VLam abstr) = do
  v <- abstr.fn (VNeutral (NVar (BV (LvlName { name: abstr.argName, lvl: depth }))))
  Lam abstr.argName <$> reify (nextLevel depth) v
  -- e <- reify env =<< abstr.fn (VNeutral (NVar abstr.argName))
  -- pure $ Lam abstr.argName e

reify env (VNeutral n) = reifyNeutral env n

reifyNeutral :: Lvl -> Neutral -> Eval IxTerm
reifyNeutral depth (NVar x) = pure $ Var $ map (lvlNameIx depth) x
reifyNeutral depth (NApp a b) = lift2 App (reifyNeutral depth a) (reify depth b)
reifyNeutral depth (NPrint x y) = do
  x' <- reifyNeutral depth x
  y' <- reify depth y
  pure $ App (App Print x') y'

step :: Eval Unit
step = do
  n <- get
  if n >= maxSteps
    then throwError TooManySteps
    else modify_ (_ + 1)

type Env = List Value

-- -- NOTE: Recursion is not supported. Can be implemented indirectly inside the language using a fixpoint combinator
-- extendWithDef :: Env -> IxDef -> Eval Env
-- extendWithDef env (Def d) = extend env d.name <$> eval env d.body

-- extendWithDefs :: Env -> List IxDef -> Eval Env
-- extendWithDefs env Nil = pure env
-- extendWithDefs env (Cons d ds) = do
--   env' <- extendWithDef env d
--   extendWithDefs env' ds

extend :: Env -> Value -> Env
extend env val = Cons val env

-- lookup :: Env -> String -> Maybe Value
-- lookup Nil _name = Nothing
-- lookup (Cons (Tuple x val) rest) name =
--   if x == name
--     then pure val
--     else lookup rest name

