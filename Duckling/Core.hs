-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE NoRebindableSyntax #-}

-- | Everything needed to run Duckling.

module Duckling.Core
  ( Context(..)
  , Dimension(..)
  , Entity(..)
  , Lang(..)
  , Locale
  , Node(..)
  , Options(..)
  , Range(..)
  , Region(..)
  , ResolvedVal(..)
  , Some(..)
  , fromName
  , makeLocale
  , toJText
  , toName

  -- Duckling API
  , parse
  , supportedDimensions
  , allLocales

  -- Reference time builders
  , currentReftime
  , fromZonedTime
  , makeReftime
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time
import Data.Time.LocalTime.TimeZone.Series
import Prelude
import qualified Data.HashMap.Strict as HashMap

import Duckling.Api
import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Types

-- | Builds a `DucklingTime` for timezone `tz` at `utcTime`.
-- If no `series` found for `tz`, uses UTC.
makeReftime :: HashMap Text TimeZoneSeries -> Text -> UTCTime -> DucklingTime
makeReftime series tz utcTime = DucklingTime $ ZoneSeriesTime ducklingTime tzs
  where
    tzs = HashMap.lookupDefault (TimeZoneSeries utc []) tz series
    ducklingTime = toUTC $ utcToLocalTime' tzs utcTime

-- | Builds a `DucklingTime` for timezone `tz` at current time.
-- If no `series` found for `tz`, uses UTC.
currentReftime :: HashMap Text TimeZoneSeries -> Text -> IO DucklingTime
currentReftime series tz = do
  utcNow <- getCurrentTime
  return $ makeReftime series tz utcNow

-- | Builds a `DucklingTime` from a `ZonedTime`.
fromZonedTime :: ZonedTime -> DucklingTime
fromZonedTime (ZonedTime localTime timeZone) = DucklingTime $
  ZoneSeriesTime (toUTC localTime) (TimeZoneSeries timeZone [])
