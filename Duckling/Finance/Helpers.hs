-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.Helpers
  ( currencyOnly
  , financeWith
  , withCents
  , withValue
  )
  where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Finance.Types (Currency (..), FinanceData (..))
import Duckling.Types hiding (Entity(..))

-- -----------------------------------------------------------------
-- Patterns

financeWith :: (FinanceData -> t) -> (t -> Bool) -> PatternItem
financeWith f pred = Predicate $ \x ->
  case x of
    (Token Finance x) -> pred (f x)
    _ -> False

-- -----------------------------------------------------------------
-- Production

currencyOnly :: Currency -> FinanceData
currencyOnly c = FinanceData {currency = c, value = Nothing}

withValue :: Double -> FinanceData -> FinanceData
withValue x fd = fd {value = Just x}

withCents :: Double -> FinanceData -> FinanceData
withCents x fd@FinanceData {value = Just value} = fd
  {value = Just $ value + x / 100}
withCents x FinanceData {value = Nothing} = FinanceData
  {value = Just x, currency = Cent}
