-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}


module Duckling.Dimensions.Types
  ( Some(..)
  , Dimension(..)

  , fromName
  , toName
  ) where

import Data.Maybe
import Data.Some
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Types

toName :: Dimension a -> Text
toName RegexMatch = "regex"
toName CreditCardNumber = "credit-card-number"
toName Distance = "distance"
toName Duration = "duration"
toName Email = "email"
toName AmountOfMoney = "amount-of-money"
toName Numeral = "number"
toName Ordinal = "ordinal"
toName PhoneNumber = "phone-number"
toName Quantity = "quantity"
toName Temperature = "temperature"
toName Time = "time"
toName TimeGrain = "time-grain"
toName Url = "url"
toName Volume = "volume"
toName (CustomDimension dim) = Text.pack (show dim)

fromName :: Text -> Maybe (Some Dimension)
fromName name = HashMap.lookup name m
  where
    m = HashMap.fromList
      [ ("amount-of-money", This AmountOfMoney)
      , ("credit-card-number", This CreditCardNumber)
      , ("distance", This Distance)
      , ("duration", This Duration)
      , ("email", This Email)
      , ("number", This Numeral)
      , ("ordinal", This Ordinal)
      , ("phone-number", This PhoneNumber)
      , ("quantity", This Quantity)
      , ("temperature", This Temperature)
      , ("time", This Time)
      , ("url", This Url)
      , ("volume", This Volume)
      ]
