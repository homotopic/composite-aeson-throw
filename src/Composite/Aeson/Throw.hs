{- |
   Module     : Composite.Aeson.Throw
   License    : MIT
   Stability  : experimental

MonadThrow behaviour for composite-aeson.
-}
module Composite.Aeson.Throw (
  CompositeAesonParseException
, parseValue'
) where

import Composite.Aeson
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.BetterErrors
import Data.Typeable

-- | Simple exception type for composite-aeson.
newtype CompositeAesonParseException a = CompositeAesonParseException a
  deriving (Eq, Show, Ord)

instance (Typeable a, Show a) => Exception (CompositeAesonParseException a)

-- | Parse a value according to the provided `JsonFormat` and throw to `MonadThrow` on exception.
parseValue' :: (Typeable e, Show e, MonadThrow m) => JsonFormat e x -> Value -> m x
parseValue' f v = do
  let a = parseValue (fromJsonWithFormat f) v
  either (throwM . CompositeAesonParseException) return a
