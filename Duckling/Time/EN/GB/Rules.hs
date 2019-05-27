-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.GB.Rules
  ( rules
  ) where

import Data.Maybe
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Computed (easterSunday)
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

ruleDDMM :: Rule
ruleDDMM = Rule
  { name = "dd/mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s?[/-]\\s?(1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
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
  [ ( "Bank Holiday", "bank holiday", monthDay 12 28 )
  , ( "Battle of the Boyne", "battle of the boyne", monthDay 7 12 )
  , ( "Burns Night", "burns night", monthDay 1 25 )
  , ( "Guy Fawkes Day", "guy fawkes day", monthDay 11 5 )
  , ( "Shakespeare Day", "shakespeare day", monthDay 4 23 )
  , ( "St Andrew's Day", "st\\.? andrew'?s day", monthDay 11 30 )
  , ( "St. David's Day", "st\\.? david'?s day", monthDay 3 1 )
  , ( "Victory in Europe Day", "victory in europe day", monthDay 5 8 )

  -- Fixed day/week/month, year over year
  , ( "August Bank Holiday", "(august|(late )?summer) bank holiday"
    , predLastOf (dayOfWeek 1) (month 8) )
  , ( "Early May Bank Holiday", "early may bank holiday", nthDOWOfMonth 1 1 5 )
  , ( "Father's Day", "father'?s?'? day", nthDOWOfMonth 3 7 6 )
  , ( "Remembrance Sunday", "remembrance sunday", nthDOWOfMonth 2 7 11 )
  , ( "Spring Bank Holiday", "(sprint|late may) bank holiday"
    , predLastOf (dayOfWeek 1) (month 5) )
  , ( "Thanksgiving Day", "thanks?giving( day)?", nthDOWOfMonth 4 4 11 )
  ]

ruleComputedHolidays :: [Rule]
ruleComputedHolidays = mkRuleHolidays
  -- 3 weeks before Easter
  [ ( "Mothering Sunday", "mothering sunday|mother'?s?'? day"
    , cycleNthAfter False TG.Day (-21) easterSunday )
  ]

rules :: [Rule]
rules =
  [ ruleDDMM
  , ruleDDMMYYYY
  , ruleDDMMYYYYDot
  ]
  ++ ruleComputedHolidays
  ++ rulePeriodicHolidays
