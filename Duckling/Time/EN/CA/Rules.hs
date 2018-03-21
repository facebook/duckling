-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.CA.Rules
  ( rules
  ) where

import Data.Maybe
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types

-- Although one can see both MMDD and DDMM in Canada,
-- there is no direct way to implement this today. Let's fallback to MMDD (US).
ruleMMDD :: Rule
ruleMMDD = Rule
  { name = "mm/dd"
  , pattern =
    [ regex "(1[0-2]|0?[1-9])\\s?[/-]\\s?(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleMMDDYYYY :: Rule
ruleMMDDYYYY = Rule
  { name = "mm/dd/yyyy"
  , pattern =
    [ regex "(1[0-2]|0?[1-9])[-/\\s](3[01]|[12]\\d|0?[1-9])[-/\\s](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-- Clashes with HHMMSS, hence only 4-digit years
ruleMMDDYYYYDot :: Rule
ruleMMDDYYYYDot = Rule
  { name = "mm.dd.yyyy"
  , pattern =
    [ regex "(1[0-2]|0?[1-9])\\.(3[01]|[12]\\d|0?[1-9])\\.(\\d{4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "Anniversary of the Statute of Westminster", "anniversary of the statute of westminster", monthDay 12 11 )
  , ( "Canada/Dominion/Memorial Day", "(canada|dominion|memorial) day", monthDay 7 1 )
  , ( "Groundhog Day", "groundhog day", monthDay 2 2 )
  , ( "Healthcare Aide Day", "healthcare aide day", monthDay 10 18 )
  , ( "National Aboriginal Day", "national aboriginal day", monthDay 6 21 )
  , ( "National Flag of Canada Day", "national flag of canada day", monthDay 2 15 )
  , ( "National Tartan Day", "national tartan day", monthDay 4 6 )
  , ( "Nunavut Day", "nunavut day", monthDay 7 9 )
  , ( "Remembrance Day", "remembrance day", monthDay 11 11 )
  , ( "St David's Day", "st\\.? david'?s day", monthDay 3 1 )
  , ( "St. Jean Baptiste Day", "st\\.? jean baptiste day", monthDay 6 24 )
  , ( "Vimy Ridge Day", "vimy ridge day", monthDay 4 9 )

  -- Fixed day/week/month, year over year
  , ( "Thanksgiving Day", "thanks?giving( day)?", nthDOWOfMonth 2 1 10 )
  ]

rules :: [Rule]
rules =
  [ ruleMMDD
  , ruleMMDDYYYY
  , ruleMMDDYYYYDot
  ]
  ++ rulePeriodicHolidays
