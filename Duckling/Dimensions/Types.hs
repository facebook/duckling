-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}


module Duckling.Dimensions.Types
  ( Seal(..)
  , Dimension(..)

  , fromName
  , toName
  ) where

import Data.Maybe
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

fromName :: Text -> Maybe (Seal Dimension)
fromName name = HashMap.lookup name m
  where
    m = HashMap.fromList
      [ ("amount-of-money", Seal AmountOfMoney)
      , ("credit-card-number", Seal CreditCardNumber)
      , ("distance", Seal Distance)
      , ("duration", Seal Duration)
      , ("email", Seal Email)
      , ("number", Seal Numeral)
      , ("ordinal", Seal Ordinal)
      , ("phone-number", Seal PhoneNumber)
      , ("quantity", Seal Quantity)
      , ("temperature", Seal Temperature)
      , ("time", Seal Time)
      , ("time-grain", Seal TimeGrain)
      , ("url", Seal Url)
      , ("volume", Seal Volume)
      ]
