-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

module Duckling.Time.HolidayHelpers
  ( computeEarthHour, computeKingsDay
  ) where

import Data.Maybe
import Prelude

import Duckling.Time.Helpers
import Duckling.Time.Computed
import Duckling.Time.Types (TimeData (..))
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

-- Last Saturday of March unless it falls on Holy Saturday,
-- in which case it's the Saturday before.
computeEarthHour :: Maybe TimeData
computeEarthHour =
  let holySaturday = cycleNthAfter False TG.Day (-1) easterSunday
      tentative = predLastOf (dayOfWeek 6) (month 3)
      alternative = cycleNthAfter False TG.Day (-7) tentative
    in do
      day <- intersectWithReplacement holySaturday tentative alternative
      start <- intersect day $ hourMinute True 20 30
      interval TTime.Closed start $ cycleNthAfter False TG.Minute 60 start

-- King's day is on April 27th unless it's on Sunday, in which case it's a day
-- earlier. We intersect with the last Sunday of April. If 4/27 is a Sunday it
-- will be the last Sunday in April.
computeKingsDay :: Maybe TimeData
computeKingsDay =
  let tentative = monthDay 4 27
      alternative = monthDay 4 26
      lastSundayOfApril = predLastOf (dayOfWeek 7) (month 4)
    in intersectWithReplacement lastSundayOfApril tentative alternative
