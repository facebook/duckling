-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.AU.Rules
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
import qualified Duckling.Time.Types as TTime
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
  [ ( "ANZAC Day", "anzac day", monthDay 4 25 )
  , ( "Australia Day", "(ana|anniversary|australia|foundation) day"
    , monthDay 1 26 )
  , ( "Harmony Day", "harmony day", monthDay 3 21 )
  , ( "National Sorry Day", "national sorry day", monthDay 5 26 )
  , ( "Queensland Day", "queensland day", monthDay 6 6 )
  , ( "Remembrance Day", "remembrance day", monthDay 11 11 )
  , ( "Take our Daughters and Sons to Work Day"
    , "take our daughters and sons to work day", monthDay 1 5 )

  -- Fixed day/week/month, year over year
  , ( "Adelaide Cup", "adelaide cup", nthDOWOfMonth 2 1 3 )
  , ( "Administrative Professionals' Day"
    , "(administrative professional|secretarie|admin)('?s'?)? day"
    , nthDOWOfMonth 1 5 5 )
  , ( "Canberra Day", "canberra day", nthDOWOfMonth 2 1 3 )
  , ( "Eight Hours Day", "eight hours day", nthDOWOfMonth 2 1 3 )
  , ( "Father's Day", "father'?s?'? day", nthDOWOfMonth 1 7 9 )
  , ( "Labour Day", "labour day", nthDOWOfMonth 1 1 10 )
  , ( "Melbourne Cup Day", "melbourne cup day", nthDOWOfMonth 1 2 11 )
  , ( "Mother's Day", "mother'?s?'? day", nthDOWOfMonth 2 7 5 )
  , ( "National Close the Gap Day", "national close the gap day"
    , nthDOWOfMonth 3 4 3 )
  , ( "National Tree Day", "(arbor|national tree) day"
    , predLastOf (dayOfWeek 7) (month 6) )
  , ( "National Schools Tree Day", "national schools tree day"
    , predLastOf (dayOfWeek 5) (month 6) )
  , ( "New South Wales Bank Holiday", "new south wales bank holiday"
    , nthDOWOfMonth 1 1 8 )
  , ( "Picnic Day", "(northern territory )?picnic day", nthDOWOfMonth 1 1 8 )
  , ( "Recreation Day", "recreation day", nthDOWOfMonth 1 1 10 )
  , ( "Thanksgiving Day", "thanks?giving( day)?", nthDOWOfMonth 4 4 11 )
  , ( "Western Australia Day", "western australia day", nthDOWOfMonth 1 1 6 )

  -- Other
  , ( "Reconciliation Day", "reconciliation\\s+day"
    , predNthAfter 0 (dayOfWeek 1) (monthDay 5 26) )
  ]

rulePeriodicHolidays' :: [Rule]
rulePeriodicHolidays' = mkRuleHolidays'
  -- Fixed day/week/month, year over year
  -- Week from Sunday of July until following Sunday that has the second Friday
  [ ( "NAIDOC Week"
    , "(naidoc|national aboriginal and islander day observance committee) week"
    , let fri = nthDOWOfMonth 2 5 7
          start = cycleNthAfter False TG.Day (- 5) fri
          end = cycleNthAfter False TG.Day 2 fri
      in interval TTime.Open start end )
  -- 3 days ending on the second Monday of February
  , ( "Royal Hobart Regatta", "royal hobart regatta"
    , let end = nthDOWOfMonth 2 1 2
      in interval TTime.Open (cycleNthAfter False TG.Day (- 2) end) end )

  -- Other
  -- Wednesday of the Royal Queensland Show
  -- Starts on the first Friday of August if it's not before August 5th
  -- Otherwise on the second Friday of August
  , ( "Royal Queensland Show Day"
    , "(royal (national agricultural|queensland)|rna) show day|ekka day"
    , let tentative = nthDOWOfMonth 1 5 8
          alternative = nthDOWOfMonth 2 5 8
      in do
        forbidden <- interval TTime.Open (monthDay 8 1) (monthDay 8 4)
        start <- intersectWithReplacement forbidden tentative alternative
        return $ cycleNthAfter False TG.Day 5 start )
  -- Starts on the first Friday of August if it's not before August 5th
  -- Otherwise on the second Friday of August
  , ( "Royal Queensland Show"
    , "ekka|(royal (national agricultural|queensland)|rna) show"
    , let tentative = nthDOWOfMonth 1 5 8
          alternative = nthDOWOfMonth 2 5 8
      in do
        forbidden <- interval TTime.Open (monthDay 8 1) (monthDay 8 4)
        start <- intersectWithReplacement forbidden tentative alternative
        interval TTime.Open start $ cycleNthAfter False TG.Day 9 start )
  ]

ruleComputedHolidays :: [Rule]
ruleComputedHolidays = mkRuleHolidays
  [ ( "Easter Tuesday", "easter\\s+tue(sday)?"
    , cycleNthAfter False TG.Day 2 easterSunday )
  ]

rules :: [Rule]
rules =
  [ ruleDDMM
  , ruleDDMMYYYY
  , ruleDDMMYYYYDot
  ]
  ++ ruleComputedHolidays
  ++ rulePeriodicHolidays
  ++ rulePeriodicHolidays'
