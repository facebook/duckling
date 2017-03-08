-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.Helpers
  ( distance
  , unitDistance
  , withUnit
  ) where


import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Types (DistanceData(..))
import qualified Duckling.Distance.Types as TDistance
import Duckling.Types

-- -----------------------------------------------------------------
-- Patterns

unitDistance :: TDistance.Unit -> PatternItem
unitDistance value = Predicate $ \x -> case x of
  (Token Distance DistanceData {TDistance.unit = Just unit}) -> value == unit
  _ -> False

-- -----------------------------------------------------------------
-- Production

distance :: Double -> DistanceData
distance x = DistanceData {TDistance.value = x, TDistance.unit = Nothing}

withUnit :: TDistance.Unit -> DistanceData -> DistanceData
withUnit value dd = dd {TDistance.unit = Just value}
