-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.Corpus
  ( datetime
  , datetimeHoliday
  , datetimeInterval
  , datetimeIntervalHoliday
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

datetime :: Datetime -> Grain -> Context -> TimeValue
datetime d g ctx = datetimeIntervalHolidayHelper (d, Nothing) g Nothing ctx

datetimeHoliday :: Datetime -> Grain -> Text -> Context -> TimeValue
datetimeHoliday d g h ctx =
  datetimeIntervalHolidayHelper (d, Nothing) g (Just h) ctx

datetimeInterval :: (Datetime, Datetime) -> Grain -> Context -> TimeValue
datetimeInterval (d1, d2) g ctx =
  datetimeIntervalHolidayHelper (d1, Just d2) g Nothing ctx

datetimeIntervalHoliday ::
  (Datetime, Datetime) -> Grain -> Text -> Context -> TimeValue
datetimeIntervalHoliday (d1, d2) g h ctx =
  datetimeIntervalHolidayHelper (d1, Just d2) g (Just h) ctx

datetimeIntervalHolidayHelper ::
  (Datetime, Maybe Datetime) -> Grain -> Maybe Text -> Context -> TimeValue
datetimeIntervalHolidayHelper (d1, md2) g hol ctx = TimeValue tv [tv] hol
  where
    DucklingTime (Series.ZoneSeriesTime _ tzSeries) = referenceTime ctx
    tv = timeValue tzSeries TimeObject {start = dt d1, end = d, grain = g}
    d = case md2 of
      Nothing -> Nothing
      Just d2 -> Just $ dt d2

datetimeOpenInterval
  :: IntervalDirection -> Datetime -> Grain -> Context -> TimeValue
datetimeOpenInterval dir d g ctx = TimeValue tv [tv] Nothing
  where
    DucklingTime (Series.ZoneSeriesTime _ tzSeries) = referenceTime ctx
    tv = openInterval tzSeries dir TimeObject
      {start = dt d, end = Nothing, grain = g}


check :: ToJSON a => (Context -> a) -> TestPredicate
check f context Resolved{rval = RVal _ v} = case toJSON v of
  Object o -> deleteValues (toJSON (f context)) == deleteValues (Object o)
  _ -> False
  where
    deleteValues :: Value -> Value
    deleteValues (Object o) = Object $ H.delete "values" o
    deleteValues _ = Object H.empty

examples :: ToJSON a => (Context -> a) -> [Text] -> [Example]
examples f = examplesCustom (check f)
