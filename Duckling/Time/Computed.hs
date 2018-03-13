-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

module Duckling.Time.Computed
  ( easterSunday
  ) where

import Data.Maybe
import Prelude
import qualified Data.Time as Time

import Duckling.Time.Helpers (timeComputed)
import Duckling.Time.Types (TimeData(..), TimeObject(..), timedata')
import qualified Duckling.TimeGrain.Types as TG

toTimeObjectM :: (Integer, Int, Int) -> Maybe TimeObject
toTimeObjectM (year, month, day) = do
  day <- Time.fromGregorianValid year month day
  let start = Time.UTCTime day 0
  return TimeObject
    { start = start
    , grain = TG.Day
    , end = Nothing
    }

computedDays :: [TimeObject] -> TimeData
computedDays xs = timedata'
  { timePred = timeComputed xs
  , timeGrain = TG.Day
  }

easterSunday :: TimeData
easterSunday = computedDays easterSunday'

easterSunday' :: [TimeObject]
easterSunday' = mapMaybe toTimeObjectM
  [ (1950, 4, 9)
  , (1951, 3, 25)
  , (1952, 4, 13)
  , (1953, 4, 5)
  , (1954, 4, 18)
  , (1955, 4, 10)
  , (1956, 4, 1)
  , (1957, 4, 21)
  , (1958, 4, 6)
  , (1959, 3, 29)
  , (1960, 4, 17)
  , (1961, 4, 2)
  , (1962, 4, 22)
  , (1963, 4, 14)
  , (1964, 3, 29)
  , (1965, 4, 18)
  , (1966, 4, 10)
  , (1967, 3, 26)
  , (1968, 4, 14)
  , (1969, 4, 6)
  , (1970, 3, 29)
  , (1971, 4, 11)
  , (1972, 4, 2)
  , (1973, 4, 22)
  , (1974, 4, 14)
  , (1975, 3, 30)
  , (1976, 4, 18)
  , (1977, 4, 10)
  , (1978, 3, 26)
  , (1979, 4, 15)
  , (1980, 4, 6)
  , (1981, 4, 19)
  , (1982, 4, 11)
  , (1983, 4, 3)
  , (1984, 4, 22)
  , (1985, 4, 7)
  , (1986, 3, 30)
  , (1987, 4, 19)
  , (1988, 4, 3)
  , (1989, 3, 26)
  , (1990, 4, 15)
  , (1991, 3, 31)
  , (1992, 4, 19)
  , (1993, 4, 11)
  , (1994, 4, 3)
  , (1995, 4, 16)
  , (1996, 4, 7)
  , (1997, 3, 30)
  , (1998, 4, 12)
  , (1999, 4, 4)
  , (2000, 4, 23)
  , (2001, 4, 15)
  , (2002, 3, 31)
  , (2003, 4, 20)
  , (2004, 4, 11)
  , (2005, 3, 27)
  , (2006, 4, 16)
  , (2007, 4, 8)
  , (2008, 3, 23)
  , (2009, 4, 12)
  , (2010, 4, 4)
  , (2011, 4, 24)
  , (2012, 4, 8)
  , (2013, 3, 31)
  , (2014, 4, 20)
  , (2015, 4, 5)
  , (2016, 3, 27)
  , (2017, 4, 16)
  , (2018, 4, 1)
  , (2019, 4, 21)
  , (2020, 4, 12)
  , (2021, 4, 4)
  , (2022, 4, 17)
  , (2023, 4, 9)
  , (2024, 3, 31)
  , (2025, 4, 20)
  , (2026, 4, 5)
  , (2027, 3, 28)
  , (2028, 4, 16)
  , (2029, 4, 1)
  , (2030, 4, 21)
  , (2031, 4, 13)
  , (2032, 3, 28)
  , (2033, 4, 17)
  , (2034, 4, 9)
  , (2035, 3, 25)
  , (2036, 4, 13)
  , (2037, 4, 5)
  , (2038, 4, 25)
  , (2039, 4, 10)
  , (2040, 4, 1)
  , (2041, 4, 21)
  , (2042, 4, 6)
  , (2043, 3, 29)
  , (2044, 4, 17)
  , (2045, 4, 9)
  , (2046, 3, 25)
  , (2047, 4, 14)
  , (2048, 4, 5)
  , (2049, 4, 18)
  , (2050, 4, 10)
  ]
