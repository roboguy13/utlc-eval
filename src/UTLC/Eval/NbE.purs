module UTLC.Eval.NbE
  where

import Prelude

import UTLC.Syntax.Term

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Maybe.Trans

import Data.Maybe

type StepCounter m = MaybeT (StateT Int m)

type StdoutWriter = Writer String

maxSteps :: Int
maxSteps = 10000

-- TODO: This probably needs a fresh name generator
type Eval = StepCounter StdoutWriter

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

step :: Eval Unit
step = do
  n <- get
  if n >= maxSteps
    then lift $ pure Nothing
    else ?a unit

