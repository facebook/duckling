-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}


module Duckling.AmountOfMoney.Helpers
  ( currencyOnly
  , financeWith
  , isCents
  , isMoneyWithValue
  , isWithoutCents
  , withCents
  , withInterval
  , withMax
  , withMin
  , withValue
  )
  where

import Data.Maybe (isJust)
import Prelude

import Duckling.AmountOfMoney.Types (Currency (..), AmountOfMoneyData (..))
import Duckling.Numeral.Types (isInteger)
import Duckling.Dimensions.Types
import Duckling.Types hiding (Entity(..))

-- -----------------------------------------------------------------
-- Patterns

financeWith :: (AmountOfMoneyData -> t) -> (t -> Bool) -> PatternItem
financeWith f pred = Predicate $ \x ->
  case x of
    (Token AmountOfMoney x) -> pred (f x)
    _ -> False

isCents :: Predicate
isCents (Token AmountOfMoney AmountOfMoneyData{value = Just _, currency = Cent}) = True
isCents _ = False

isWithoutCents :: Predicate
isWithoutCents (Token AmountOfMoney AmountOfMoneyData{currency = Cent}) = False
isWithoutCents (Token AmountOfMoney AmountOfMoneyData{value = Just v}) = isInteger v
isWithoutCents _ = False

isMoneyWithValue :: Predicate
isMoneyWithValue (Token AmountOfMoney AmountOfMoneyData{value = v1, minValue = v2, maxValue = v3}) =
 any isJust [v1, v2, v3]
isMoneyWithValue _ = False

-- -----------------------------------------------------------------
-- Production

currencyOnly :: Currency -> AmountOfMoneyData
currencyOnly c = AmountOfMoneyData
  {currency = c, value = Nothing, minValue = Nothing, maxValue = Nothing}

withValue :: Double -> AmountOfMoneyData -> AmountOfMoneyData
withValue x fd = fd {value = Just x}

withCents :: Double -> AmountOfMoneyData -> AmountOfMoneyData
withCents x fd@AmountOfMoneyData {value = Just value} = fd
  {value = Just $ value + x / 100}
withCents x AmountOfMoneyData {value = Nothing} = AmountOfMoneyData
  {value = Just x, currency = Cent, minValue = Nothing, maxValue = Nothing}

withInterval :: (Double, Double) -> AmountOfMoneyData -> AmountOfMoneyData
withInterval (from, to) fd = fd
  {minValue = Just from, maxValue = Just to, value = Nothing}

withMin :: Double -> AmountOfMoneyData -> AmountOfMoneyData
withMin x fd = fd {minValue = Just x}

withMax :: Double -> AmountOfMoneyData -> AmountOfMoneyData
withMax x fd = fd {maxValue = Just x}
