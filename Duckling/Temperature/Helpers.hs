-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}

module Duckling.Temperature.Helpers
  ( isLatent
  , isValueOnly
  , isSimpleTemperature
  , withUnit
  , withInterval
  , withValue
  , valueOnly
  , unitOnly
  , withMin
  , withMax
  ) where

import Data.Maybe
import Prelude

import Duckling.Dimensions.Types
import qualified Duckling.Temperature.Types as TTemperature
import Duckling.Temperature.Types (TemperatureData(..))
import Duckling.Types hiding (isLatent)

-- -----------------------------------------------------------------
-- Patterns

isLatent :: Predicate
isLatent (Token Temperature TemperatureData{TTemperature.unit = Nothing}) =
  True
isLatent _ = False

isValueOnly :: Bool -> Predicate
isValueOnly allowDegree (Token Temperature TemperatureData
                         { TTemperature.unit = u, TTemperature.value = Just _
                         , TTemperature.minValue = Nothing
                         , TTemperature.maxValue = Nothing})
      = isNothing u || (allowDegree && u == Just TTemperature.Degree)
isValueOnly _ _ = False

isSimpleTemperature :: Predicate
isSimpleTemperature (Token Temperature TemperatureData
                      {TTemperature.value = Just _}) = True
isSimpleTemperature _ = False

-- -----------------------------------------------------------------
-- Production

withUnit :: TTemperature.TemperatureUnit -> TemperatureData -> TemperatureData
withUnit u td = td {TTemperature.unit = Just u}

withInterval :: (Int, Int) -> TemperatureData -> TemperatureData
withInterval (from, to) td = td
  { TTemperature.value = Nothing
  , TTemperature.minValue = Just from
  , TTemperature.maxValue = Just to
  }

withValue :: Int -> TemperatureData -> TemperatureData
withValue v td = td
  { TTemperature.value = Just v
  , TTemperature.minValue = Nothing
  , TTemperature.maxValue = Nothing
  }

valueOnly :: Int -> TemperatureData
valueOnly v = TemperatureData
  { TTemperature.unit = Nothing
  , TTemperature.value = Just v
  , TTemperature.minValue = Nothing
  , TTemperature.maxValue = Nothing
  }

unitOnly :: TTemperature.TemperatureUnit -> TemperatureData
unitOnly u = TemperatureData
  { TTemperature.unit = Just u
  , TTemperature.value = Nothing
  , TTemperature.minValue = Nothing
  , TTemperature.maxValue = Nothing
  }

withMax :: Int -> TemperatureData -> TemperatureData
withMax v td = td
  { TTemperature.value = Nothing
  , TTemperature.minValue = Nothing
  , TTemperature.maxValue = Just v
  }

withMin :: Int -> TemperatureData -> TemperatureData
withMin v td = td
  { TTemperature.value = Nothing
  , TTemperature.minValue = Just v
  , TTemperature.maxValue = Nothing
  }
