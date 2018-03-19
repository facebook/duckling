-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}


module Duckling.Temperature.Helpers
  ( isLatent
  , withUnit
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

-- -----------------------------------------------------------------
-- Production

withUnit :: TTemperature.TemperatureUnit -> TemperatureData -> TemperatureData
withUnit u td = td {TTemperature.unit = Just u}
