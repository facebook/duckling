-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Volume.Helpers
  ( isSimpleVolume
  , isUnitOnly
  , volume
  , unitOnly
  , valueOnly
  , withUnit
  , withValue
  , withInterval
  , withMin
  , withMax
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Volume.Types (Unit(..), VolumeData(..))
import Duckling.Types
import qualified Duckling.Volume.Types as TVolume

-- -----------------------------------------------------------------
-- Patterns

isSimpleVolume :: Predicate
isSimpleVolume (Token Volume VolumeData {TVolume.value = Just _
                                        , TVolume.minValue = Nothing
                                        , TVolume.maxValue = Nothing}) = True
isSimpleVolume _ = False

isUnitOnly :: Predicate
isUnitOnly (Token Volume VolumeData {TVolume.value = Nothing
                                    , TVolume.unit = Just _
                                    , TVolume.minValue = Nothing
                                    , TVolume.maxValue = Nothing}) = True
isUnitOnly _ = False

-- -----------------------------------------------------------------
-- Production

volume :: Unit -> Double -> VolumeData
volume u v = VolumeData {TVolume.unit = Just u
                        , TVolume.value = Just v
                        , TVolume.minValue = Nothing
                        , TVolume.maxValue = Nothing}

unitOnly :: Unit -> VolumeData
unitOnly u = VolumeData {TVolume.unit = Just u
                        , TVolume.value = Nothing
                        , TVolume.minValue = Nothing
                        , TVolume.maxValue = Nothing}

valueOnly :: Double -> VolumeData
valueOnly v = VolumeData {TVolume.unit = Nothing
                        , TVolume.value = Just v
                        , TVolume.minValue = Nothing
                        , TVolume.maxValue = Nothing}

withUnit :: Unit -> VolumeData -> VolumeData
withUnit u vd = vd {TVolume.unit = Just u}

withValue :: Double -> VolumeData -> VolumeData
withValue v vd = vd {TVolume.value = Just v}

withInterval :: (Double, Double) -> VolumeData -> VolumeData
withInterval (from, to) vd = vd {TVolume.value = Nothing
                                , TVolume.minValue = Just from
                                , TVolume.maxValue = Just to}

withMin :: Double -> VolumeData -> VolumeData
withMin from vd = vd {TVolume.minValue = Just from}

withMax :: Double -> VolumeData -> VolumeData
withMax to vd = vd {TVolume.maxValue = Just to}
