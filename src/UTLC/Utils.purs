module UTLC.Utils
  where

import Prelude

import Effect.Unsafe
import Effect.Exception

error :: forall a. String -> a
error = unsafePerformEffect <<< throw
