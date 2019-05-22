-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.PH.Rules
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
  [ ( "Arbor Day", "arbor day", monthDay 6 25 )
  , ( "Bonifacio Day", "bonifacio day", monthDay 11 30 )
  , ( "Independence Day", "independence day", monthDay 6 12 )
  , ( "Labour Day", "labour day", monthDay 5 1 )
  , ( "Ninoy Aquino Day", "ninoy aquino day", monthDay 8 21 )
  , ( "People Power Anniversary", "(edsa revolution|people power) anniversary"
    , monthDay 2 25 )
  , ( "Rizal Day", "rizal day", monthDay 12 30 )
  , ( "The Day of Valor", "the day of valor", monthDay 4 9 )

  -- Fixed day/week/month, year over year
  , ( "Father's Day", "father'?s?'? day", nthDOWOfMonth 3 7 6 )
  , ( "Mother's Day", "mother'?s?'? day", nthDOWOfMonth 2 7 5 )
  , ( "National Elections Day", "national elections day", nthDOWOfMonth 2 1 5 )
  , ( "National Heroes' Day", "(national )?heroes' day"
    , predLastOf (dayOfWeek 1) (month 8) )
  , ( "Parents' Day", "parents' day", nthDOWOfMonth 1 1 12 )
  , ( "Thanksgiving Day", "thanks?giving( day)?", nthDOWOfMonth 4 4 11 )
  ]

rules :: [Rule]
rules =
  [ ruleMMDD
  , ruleMMDDYYYY
  , ruleMMDDYYYYDot
  ]
  ++ rulePeriodicHolidays
