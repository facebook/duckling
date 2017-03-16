-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.Helpers
  ( currencyOnly
  , financeWith
  , withCents
  , withValue
  )
  where

import Prelude

import Duckling.Dimensions.Types
import Duckling.AmountOfMoney.Types (Currency (..), AmountOfMoneyData (..))
import Duckling.Types hiding (Entity(..))

-- -----------------------------------------------------------------
-- Patterns

financeWith :: (AmountOfMoneyData -> t) -> (t -> Bool) -> PatternItem
financeWith f pred = Predicate $ \x ->
  case x of
    (Token AmountOfMoney x) -> pred (f x)
    _ -> False

-- -----------------------------------------------------------------
-- Production

currencyOnly :: Currency -> AmountOfMoneyData
currencyOnly c = AmountOfMoneyData {currency = c, value = Nothing}

withValue :: Double -> AmountOfMoneyData -> AmountOfMoneyData
withValue x fd = fd {value = Just x}

withCents :: Double -> AmountOfMoneyData -> AmountOfMoneyData
withCents x fd@AmountOfMoneyData {value = Just value} = fd
  {value = Just $ value + x / 100}
withCents x AmountOfMoneyData {value = Nothing} = AmountOfMoneyData
  {value = Just x, currency = Cent}
