{-# LANGUAGE CPP #-}

module Duckling.Compat
  ( KM.insert
  , KM.delete
  , fromText
  ) where

# if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.KeyMap as KM
# else
import qualified Data.HashMap.Strict as KM
# endif

# if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.Key (fromText)
# else
fromText :: Text -> Text
fromText = id
# endif
