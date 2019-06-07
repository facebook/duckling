-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.Resolve
  ( Context(..)
  , DucklingTime(..)
  , Options(..)
  , Resolve(..)
  , fromUTC
  , toUTC
  ) where

import Data.Aeson (ToJSON)
import Prelude
import qualified Data.Time as Time
import qualified Data.Time.LocalTime.TimeZone.Series as Series

import Duckling.Locale

-- | Internal time reference.
-- We work as if we were in UTC time and use `ZoneSeriesTime` to house the info.
-- We convert to local time at resolution, using `fromUTC`.
newtype DucklingTime = DucklingTime Series.ZoneSeriesTime
  deriving (Eq, Show)

data Context = Context
  { referenceTime :: DucklingTime
  , locale :: Locale
  }
  deriving (Eq, Show)

newtype Options = Options
  { withLatent :: Bool  -- When set, includes less certain parses, e.g. "7" as an hour of the day
  }
  deriving (Eq, Show)

class ( Eq (ResolvedValue a)
      , Show (ResolvedValue a)
      , ToJSON (ResolvedValue a)
      ) => Resolve a where
  type ResolvedValue a
  resolve :: Context -> Options -> a -> Maybe (ResolvedValue a, Bool)

-- | Given a UTCTime and an TimeZone, build a ZonedTime (no conversion)
fromUTC :: Time.UTCTime -> Time.TimeZone -> Time.ZonedTime
fromUTC (Time.UTCTime day diffTime) timeZone = Time.ZonedTime localTime timeZone
  where
    localTime = Time.LocalTime day timeOfDay
    timeOfDay = Time.timeToTimeOfDay diffTime

-- | Given a LocalTime, build a UTCTime (no conversion)
toUTC :: Time.LocalTime -> Time.UTCTime
toUTC (Time.LocalTime day timeOfDay) = Time.UTCTime day diffTime
  where
    diffTime = Time.timeOfDayToTime timeOfDay
