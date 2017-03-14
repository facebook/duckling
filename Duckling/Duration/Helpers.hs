-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.Helpers
  ( duration
  , isGrain
  , isNatural
  , isNumberWith
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Duration.Types (DurationData (DurationData))
import qualified Duckling.Duration.Types as TDuration
import Duckling.Number.Types (NumberData (NumberData))
import qualified Duckling.Number.Types as TNumber
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

-- -----------------------------------------------------------------
-- Patterns

isGrain :: TG.Grain -> Predicate
isGrain value (Token TimeGrain grain) = grain == value
isGrain _ _ = False

isNatural :: Predicate
isNatural (Token Numeral NumberData {TNumber.value = x}) =
  TNumber.isNatural x
isNatural _ = False

isNumberWith :: (NumberData -> t) -> (t -> Bool) -> PatternItem
isNumberWith f pred = Predicate $ \x -> case x of
  (Token Numeral x) -> pred $ f x
  _ -> False

-- -----------------------------------------------------------------
-- Production

duration :: TG.Grain -> Int -> DurationData
duration grain n = DurationData {TDuration.grain = grain, TDuration.value = n}
