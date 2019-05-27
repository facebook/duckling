-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}

module Duckling.Distance.Helpers
  ( distance
  , distanceSum
  , isDistanceOfUnit
  , isSimpleDistance
  , unitOnly
  , withInterval
  , withMax
  , withMin
  , withUnit
  , withValue
  ) where

import Prelude
import Data.Semigroup ((<>))

import Duckling.Dimensions.Types
import Duckling.Distance.Types (DistanceData(..))
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance
import qualified Duckling.DistanceUnits.Types as DGTypes

-- -----------------------------------------------------------------
-- Patterns

isSimpleDistance :: Predicate
isSimpleDistance (Token Distance DistanceData {TDistance.value = Just _
                                              , TDistance.unit = Just _}) = True
isSimpleDistance _ = False

isDistanceOfUnit :: TDistance.Unit -> Predicate
isDistanceOfUnit unit (Token Distance DistanceData {TDistance.unit = Just u}) =
    unit == u
isDistanceOfUnit _ _ = False

-- -----------------------------------------------------------------
-- Production

distance :: Double -> DistanceData
distance x = DistanceData {TDistance.value = Just x
                          , TDistance.unit = Nothing
                          , TDistance.minValue = Nothing
                          , TDistance.maxValue = Nothing}

distanceSum ::
     Double
  -> TDistance.Unit
  -> Double
  -> TDistance.Unit
  -> Maybe DistanceData
distanceSum v1 u1 v2 u2 = unwrapContext $ cd1 <> cd2
  where
    wrapContext v u = DGTypes.ContextualDistance v $ DGTypes.toSystemUnit u
    cd1 = wrapContext v1 u1
    cd2 = wrapContext v2 u2

    unwrapContext DGTypes.Nonrelatable = Nothing
    unwrapContext (DGTypes.ContextualDistance v u) =
        Just $ withUnit (DGTypes.toRawUnit u) $ distance v

unitOnly :: TDistance.Unit -> DistanceData
unitOnly u = DistanceData {TDistance.unit = Just u
                          , TDistance.value = Nothing
                          , TDistance.minValue = Nothing
                          , TDistance.maxValue = Nothing}

withUnit :: TDistance.Unit -> DistanceData -> DistanceData
withUnit u dd = dd {TDistance.unit = Just u}

withValue :: Double -> DistanceData -> DistanceData
withValue value dd = dd {TDistance.value = Just value}

withInterval :: (Double, Double) -> DistanceData -> DistanceData
withInterval (from, to) dd = dd {TDistance.minValue = Just from
                                , TDistance.maxValue = Just to}

withMin :: Double -> DistanceData -> DistanceData
withMin from dd = dd {TDistance.minValue = Just from}

withMax :: Double -> DistanceData -> DistanceData
withMax to dd = dd {TDistance.maxValue = Just to}
