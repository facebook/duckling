-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.Corpus
  ( datetime
  , datetimeInterval
  , datetimeOpenInterval
  , examples
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Time.LocalTime.TimeZone.Series as Series
import Prelude
import Data.String

import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Types hiding (Entity(..))

datetime :: Datetime -> Grain -> Context -> SingleTimeValue
datetime d g ctx =
  timeValue tzSeries TimeObject {start = dt d, end = Nothing, grain = g}
  where
    DucklingTime (Series.ZoneSeriesTime _ tzSeries) = referenceTime ctx

datetimeInterval :: (Datetime, Datetime) -> Grain -> Context -> SingleTimeValue
datetimeInterval (d1, d2) g ctx = timeValue tzSeries TimeObject
  {start = dt d1, end = Just $ dt d2, grain = g}
  where
    DucklingTime (Series.ZoneSeriesTime _ tzSeries) = referenceTime ctx

datetimeOpenInterval
  :: IntervalDirection -> Datetime -> Grain -> Context -> SingleTimeValue
datetimeOpenInterval dir d g ctx = openInterval tzSeries dir TimeObject
  {start = dt d, end = Nothing, grain = g}
  where
    DucklingTime (Series.ZoneSeriesTime _ tzSeries) = referenceTime ctx

check :: ToJSON a => (Context -> a) -> TestPredicate
check f context (Resolved {jsonValue}) = case jsonValue of
  Object o -> toJSON (f context) == (Object $ H.delete "values" o)
  _ -> False

examples :: ToJSON a => (Context -> a) -> [Text] -> [Example]
examples f = examplesCustom (check f)
