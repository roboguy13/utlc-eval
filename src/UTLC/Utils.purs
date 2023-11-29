module UTLC.Utils
  where

import Prelude

import Effect.Unsafe
import Effect.Exception

import Data.String as String
import Data.List

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

unlines :: Array String -> String
unlines = foldr go ""
  where
    go :: String -> String -> String
    go here rest =
      if String.null rest
      then here
      else here <> "\n" <> rest

