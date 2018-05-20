-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}


module Duckling.AmountOfMoney.Helpers
  ( currencyOnly
  , isSimpleAmountOfMoney
  , isCent
  , isCents
  , isCurrencyOnly
  , isDime
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
import Duckling.Numeral.Types (getIntValue, isInteger)
import Duckling.Dimensions.Types
import Duckling.Types hiding (Entity(..))

-- -----------------------------------------------------------------
-- Patterns

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

isCurrencyOnly :: Predicate
isCurrencyOnly (Token AmountOfMoney AmountOfMoneyData
  {value = Nothing, minValue = Nothing, maxValue = Nothing}) = True
isCurrencyOnly _ = False

isSimpleAmountOfMoney :: Predicate
isSimpleAmountOfMoney (Token AmountOfMoney AmountOfMoneyData
  {value = Just _, minValue = Nothing, maxValue = Nothing}) = True
isSimpleAmountOfMoney _ = False

isDime :: Predicate
isDime (Token AmountOfMoney AmountOfMoneyData
  {value = Just d, currency = Cent}) =
  maybe False (\i -> (i `mod` 10) == 0) $ getIntValue d
isDime _ = False

isCent :: Predicate
isCent (Token AmountOfMoney AmountOfMoneyData
  {value = Just c, currency = Cent}) =
  maybe False (\i -> i >= 0 && i <= 9) $ getIntValue c
isCent _ = False

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
