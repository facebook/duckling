-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.ZA.Rules
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
import qualified Duckling.Time.Types as TTime

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

ruleDDMMYYYY :: Rule
ruleDDMMYYYY = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[-/\\s](1[0-2]|0?[1-9])[-/\\s](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        y <- parseInt yy
        d <- parseInt dd
        m <- parseInt mm
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-- Clashes with HHMMSS, hence only 4-digit years
ruleDDMMYYYYDot :: Rule
ruleDDMMYYYYDot = Rule
  { name = "dd.mm.yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\.(1[0-2]|0?[1-9])\\.(\\d{4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        y <- parseInt yy
        d <- parseInt dd
        m <- parseInt mm
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "Day of Goodwill", "day of goodwill", monthDay 12 26 )
  , ( "Day of Reconciliation", "day of( the)? (covenant|reconciliation|vow)", monthDay 12 16 )
  , ( "Heritage Day", "heritage day", monthDay 9 24 )
  , ( "Heroes' Day", "(heroes'|kruger) day", monthDay 10 10 )
  , ( "Labour Day", "labour day", monthDay 5 1 )
  , ( "National Women's Day", "national women'?s day", monthDay 8 9 )
  , ( "Workers' Day", "workers'? day", monthDay 5 1 )

  -- Fixed day/week/month, year over year
  , ( "Administrative Professionals' Day"
    , "(administrative professional|secretarie|admin)('?s'?)? day"
    , nthDOWOfMonth 1 3 9 )
  , ( "Father's Day", "father'?s?'? day", nthDOWOfMonth 3 7 6 )
  , ( "Mother's Day", "mother'?s?'? day", nthDOWOfMonth 2 7 5 )
  , ( "Thanksgiving Day", "thanks?giving( day)?", nthDOWOfMonth 4 4 11 )
  ]

rulePeriodicHolidays' :: [Rule]
rulePeriodicHolidays' = mkRuleHolidays'
  -- Fixed dates, year over year
  [ ( "National Arbor Week", "national arbor week"
    , interval TTime.Open (monthDay 9 1) (monthDay 9 7) )
  ]

rules :: [Rule]
rules =
  [ ruleMMDD
  , ruleDDMMYYYY
  , ruleDDMMYYYYDot
  ]
  ++ rulePeriodicHolidays
  ++ rulePeriodicHolidays'
