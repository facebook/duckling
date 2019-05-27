-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.US.Rules where

import Data.Maybe
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Computed
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

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

ruleBackwardCompatibleHolidays :: [Rule]
ruleBackwardCompatibleHolidays = mkRuleHolidays
  [ ("Thanksgiving Day", "thanks?giving( day)?", nthDOWOfMonth 4 4 11)
  , ("Father's Day", "father'?s?'? day", nthDOWOfMonth 3 7 6)
  ]

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "African Liberation Day", "african liberation day", monthDay 5 25 )
  , ( "Air Force Birthday", "air force birthday", monthDay 9 18 )
  , ( "Alaska Day", "alaska day", monthDay 10 18 )
  , ( "American Eagle Day", "american eagle day", monthDay 6 20 )
  , ( "Army Birthday", "army birthday", monthDay 6 14 )
  , ( "Bennington Battle Day", "bennington battle day", monthDay 8 16 )
  , ( "Bunker Hill Day", "bunker hill day", monthDay 6 17 )
  , ( "California Admission Day", "california admission day", monthDay 9 9 )
  , ( "Cinco de Mayo", "cinco de mayo", monthDay 5 5 )
  , ( "Citizenship Day", "citizenship day|i am an american day", monthDay 9 17 )
  , ( "Coast Guard Birthday", "coast guard (birth)?day", monthDay 8 4 )
  , ( "Colorado Day", "colorado day", monthDay 8 1 )
  , ( "Constitution Day and Citizenship Day", "constitution day and citizenship day", monthDay 9 17 )
  , ( "César Chávez Day", "c[ée]sar ch[áa]vez day", monthDay 3 31 )
  , ( "D-Day", "d\\-day", monthDay 6 6 )
  , ( "Day After Christmas Day", "day after christmas day", monthDay 12 26 )
  , ( "Elizabeth Peratrovich Day", "elizabeth peratrovich day", monthDay 2 16 )
  , ( "Evacuation Day", "evacuation day", monthDay 3 17 )
  , ( "Father Damien Day", "father damien day", monthDay 4 15 )
  , ( "Feast of Our Lady of Guadalupe", "feast of our lady of guadalupe", monthDay 12 12 )
  , ( "Flag Day", "flag day", monthDay 6 14 )
  , ( "Groundhog Day", "groundhogs? day", monthDay 2 2 )
  , ( "Harvey Milk Day", "harvey milk day", monthDay 5 22 )
  , ( "Inauguration Day", "inauguration day", monthDay 1 20 )
  , ( "Independence Day", "independence day", monthDay 7 4 )
  , ( "Juneteenth", "juneteenth", monthDay 6 19 )
  , ( "Kamehameha Day", "kamehameha day", monthDay 6 11 )
  , ( "Kansas Day", "kansas day", monthDay 1 29 )
  , ( "Kent State Shootings Remembrance", "kent state shootings remembrance", monthDay 5 4 )
  , ( "Loyalty Day", "l(aw|ei|oyalty) day", monthDay 5 1 )
  , ( "Leif Erikson Day", "leif erikson day", monthDay 10 9 )
  , ( "Lincoln's Birthday", "(abraham )?lincoln'?s?'? birthday", monthDay 2 12 )
  , ( "Linus Pauling Day", "linus pauling day", monthDay 2 28 )
  , ( "Lyndon Baines Johnson Day", "lyndon baines johnson day", monthDay 8 27 )
  , ( "Marine Corps Birthday", "marine corps (birth)?day", monthDay 11 10 )
  , ( "Maryland Day", "maryland day", monthDay 3 25 )
  , ( "National Aviation Day", "national aviation day", monthDay 8 19 )
  , ( "National Freedom Day", "national freedom day", monthDay 2 1 )
  , ( "National Guard Birthday", "national guard (birth)?day", monthDay 12 13 )
  , ( "National Korean War Veterans Armistice Day", "national korean war veterans armistice day", monthDay 7 27 )
  , ( "National Maritime Day", "national maritime day", monthDay 5 22 )
  , ( "National Missing Children's Day", "national missing children'?s day", monthDay 5 25 )
  , ( "National Nurses Day", "national nurses day", monthDay 5 6 )
  , ( "National Tartan Day", "national tartan day", monthDay 4 6 )
  , ( "Navy Birthday", "(u\\.?s\\.? )?navy (birth)?day", monthDay 10 13 )
  , ( "Oklahoma Day", "oklahoma day", monthDay 4 22 )
  , ( "Pan American Aviation Day", "pan american aviation day", monthDay 12 17 )
  , ( "Pascua Florida Day", "pascua florida day", monthDay 4 2 )
  , ( "Patriot Day", "patriot day", monthDay 9 11 )
  , ( "Peace Officers Memorial Day", "peace officers memorial day", monthDay 5 15 )
  , ( "Pearl Harbor Remembrance Day", "pearl harbor remembrance day", monthDay 12 7 )
  , ( "Pioneer Day", "pioneer day", monthDay 7 24 )
  , ( "Prince Jonah Kuhio Kalanianaole Day", "prince jonah kuhio kalanianaole day", monthDay 3 26 )
  , ( "Purple Heart Day", "purple heart day", monthDay 8 7 )
  , ( "Read Across America Day", "read across america day", monthDay 3 2 )
  , ( "Rhode Island Independence Day", "rhode island independence day", monthDay 5 4 )
  , ( "Rosa Parks Day", "rosa parks day", monthDay 2 4 )
  , ( "San Jacinto Day", "san jacinto day", monthDay 4 21 )
  , ( "Self-Injury Awareness Day", "self\\-injury awareness day", monthDay 3 1 )
  , ( "Senior Citizens Day", "senior citizens day", monthDay 8 21 )
  , ( "Siblings Day", "(national )?sibling'?s?'? day", monthDay 4 10 )
  , ( "St Nicholas' Day", "st\\.? nicholas'? day", monthDay 12 6 )
  , ( "St. David's Day", "st\\.? david'?s day", monthDay 3 1 )
  , ( "Statehood Day", "statehood day", monthDay 6 1 )
  , ( "Statehood Day in Arizona", "statehood day in arizona", monthDay 2 14 )
  , ( "Stephen Foster Memorial Day", "stephen foster memorial day", monthDay 1 13 )
  , ( "Susan B Anthony's Birthday", "susan b\\.? anthony'?s birthday", monthDay 2 15 )
  , ( "Texas Independence Day", "texas independence day", monthDay 3 2 )
  , ( "Thomas Jefferson's Birthday", "thomas jefferson'?s birthday", monthDay 4 13 )
  , ( "Truman Day", "truman day", monthDay 5 8 )
  , ( "Veterans Day", "veterans? day", monthDay 11 11 )
  , ( "West Virginia Day", "west virginia day", monthDay 6 20 )
  , ( "White Cane Safety Day", "white cane safety day", monthDay 10 15 )
  , ( "Women's Equality Day", "women'?s equality day", monthDay 8 26 )
  , ( "Wright Brothers Day", "wright brothers day", monthDay 12 17 )

  -- Fixed day/week/month, year over year
  , ( "Arbor Day", "arbor day", predLastOf (dayOfWeek 5) (month 4) )
  , ( "Armed Forces Day", "armed forces day", nthDOWOfMonth 3 6 5 )
  , ( "Casimir Pulaski Day", "casimir pulaski day", nthDOWOfMonth 1 1 3 )
  , ( "Child Health Day", "child health day", nthDOWOfMonth 1 1 10 )
  , ( "Columbus Day", "columbus day", nthDOWOfMonth 2 1 10 )
  , ( "Daylight Saving Start Day", "daylight savings? start( day)?", nthDOWOfMonth 2 7 3 )
  , ( "Daylight Saving End Day", "daylight savings? end( day)?", nthDOWOfMonth 1 7 11 )

  -- Saturday after Labor Day
  , ( "Carl Garner Federal Lands Cleanup Day"
    , "carl garner federal lands cleanup day"
    , cycleNthAfter False TG.Day 5 $ nthDOWOfMonth 1 1 9 )
  -- Monday after Thanksgiving
  , ( "Cyber Monday", "cyber monday"
    , cycleNthAfter False TG.Day 4 $ nthDOWOfMonth 4 4 11 )
  , ( "Election Day", "election day"
    , cycleNthAfter False TG.Day 1 $ nthDOWOfMonth 1 1 11 )
  , ( "Employee Appreciation Day", "employee appreciation day"
    , nthDOWOfMonth 1 5 3 )
  , ( "Friendship Day", "friendship day", nthDOWOfMonth 1 7 8 )
  , ( "Gold Star Mother's Day", "gold star mother's day"
    , predLastOf (dayOfWeek 7) (month 9) )
  , ( "Indigenous People's Day", "indigenous people's day"
    , nthDOWOfMonth 2 1 10 )
  , ( "Labor Day", "labor day", nthDOWOfMonth 1 1 9 )
  -- Long weekend before the first Monday of September
  , ( "Labor Day weekend", "labor day week(\\s|-)?ends?"
    , longWEBefore $ nthDOWOfMonth 1 1 9
    )
  , ( "Lee Jackson Day", "lee jackson day", nthDOWOfMonth 2 5 1 )
  -- Last Monday of May
  , ( "Memorial Day", "(decoration|memorial) day"
    , predLastOf (dayOfWeek 1) (month 5) )
  -- Long weekend before the last Monday of May
  , ( "Memorial Day weekend", "(decoration|memorial) day week(\\s|-)?ends?"
    , longWEBefore $ predLastOf (dayOfWeek 1) (month 5) )
  -- 2 days before Mother's Day
  , ( "Military Spouse Day", "military spouse (appreciation )?day"
    , cycleNthAfter False TG.Day (- 2) $ nthDOWOfMonth 2 7 5 )
  , ( "Mother's Day", "mother'?s?'? day", nthDOWOfMonth 2 7 5 )
  , ( "National CleanUp Day", "national clean\\-?up day", nthDOWOfMonth 3 6 9 )
  , ( "National Day of Prayer", "national day of prayer", nthDOWOfMonth 1 4 5 )
  , ( "National Defense Transportation Day"
    , "national defense transportation day"
    , nthDOWOfMonth 3 5 5 )
  , ( "National Explosive Ordnance Disposal Day"
    , "national (eod|explosive ordnance disosal) day"
    , nthDOWOfMonth 1 6 5 )
  -- First Sunday after Labor Day
  , ( "National Grandparents Day", "national grandparents day"
    , cycleNthAfter False TG.Day 6 $ nthDOWOfMonth 1 1 9 )
  , ( "National POW/MIA Recognition Day", "national pow/mia recognition day"
    , nthDOWOfMonth 3 5 9 )
  , ( "National Wear Red Day", "national wear red day", nthDOWOfMonth 1 5 2 )
  -- Day after Thanksgiving
  , ( "Native American Heritage Day"
    , "(american indian|native american) heritage day"
    , cycleNthAfter False TG.Day 1 $ nthDOWOfMonth 4 4 11 )
  , ( "Native Americans' Day", "native americans' day", nthDOWOfMonth 4 1 9 )
  , ( "Nevada Day", "nevada day", predLastOf (dayOfWeek 5) (month 10) )
  , ( "Parents' Day", "parents' day", nthDOWOfMonth 4 7 7 )
  , ( "Patriots' Day", "patriot'?s'? day", nthDOWOfMonth 3 1 4 )
  , ( "Robert E. Lee Day", "robert e\\. lee day|lee's (birth)?day"
    , nthDOWOfMonth 3 1 1 )
  , ( "Seward's Day", "seward's day", predLastOf (dayOfWeek 1) (month 3) )
  , ( "Statehood Day", "(admission|statehood) day", nthDOWOfMonth 3 5 8 )
  , ( "Sweetest Day", "sweetest day", nthDOWOfMonth 3 6 10 )
  , ( "Take our Daughters and Sons to Work Day"
    , "take our daughters and sons to work day", nthDOWOfMonth 4 4 4 )
  , ( "Town Meeting Day", "town meeting day", nthDOWOfMonth 1 2 3 )
  , ( "Victory Day", "victory day", nthDOWOfMonth 2 1 8 )
  , ( "President's Day"
    , "(george )?washington'?s? (birth)?day|president'?s?'? day|daisy gatson bates'? day"
    , nthDOWOfMonth 3 1 2
    )

  -- Wednesday of the last full week of April, where a full week starts on
  -- Sunday and ends on Saturday.
  , ( "Administrative Professionals' Day"
    , "(administrative professional|secretarie|admin)('?s'?)? day"
    , cycleNthAfter False TG.Day (-3) $
        predNthAfter (-1) (dayOfWeek 6) (monthDay 5 1) )
  -- Wednesday of the 3rd full week in May, starting on a Sunday
  , ( "Emergency Medical Services for Children Day"
    , "(national )?(emsc|emergency medical services for children) day"
    , cycleNthAfter False TG.Day 3 $ predNthAfter 2 (dayOfWeek 7) (monthDay 5 1)
    )

  -- Other
  , ( "Emancipation Day", "emancipation day"
    , predNthClosest 0 weekday $ monthDay 4 16 )
  ]

rulePeriodicHolidays' :: [Rule]
rulePeriodicHolidays' = mkRuleHolidays'
  -- Fixed dates, year over year
  [ ( "Kwanzaa", "kwanzaa", interval TTime.Open (monthDay 12 26) (monthDay 1 1) )

  -- 3rd full week in May, starting on a Sunday
  , ( "Emergency Medical Services Week"
    , "(national )?(ems|emergency medical services) week"
    , let start = predNthAfter 2 (dayOfWeek 7) (monthDay 5 1)
          end = cycleNthAfter False TG.Day 6 start
      in interval TTime.Open start end )

  -- Other
  -- First weekday on or after April 15 if different than Emancipation Day
  -- Otherwise, first weekday following Emancipation Day
  , ( "Tax Day", "tax day"
    , let emancipationDay = predNthClosest 0 weekday $ monthDay 4 16
          tentative = predNthAfter 0 weekday $ monthDay 4 15
          alternative = predNthAfter 1 weekday emancipationDay
      in intersectWithReplacement emancipationDay tentative alternative )
  ]

ruleComputedHolidays' :: [Rule]
ruleComputedHolidays' = mkRuleHolidays'
  [ ( "Global Youth Service Day", "national youth service day"
    , let start = globalYouthServiceDay
          end = cycleNthAfter False TG.Day 2 globalYouthServiceDay
        in interval TTime.Open start end )
  ]

rulesBackwardCompatible :: [Rule]
rulesBackwardCompatible =
  [ ruleMMDD
  , ruleMMDDYYYY
  , ruleMMDDYYYYDot
  ]
  ++ ruleBackwardCompatibleHolidays

rules :: [Rule]
rules = rulesBackwardCompatible
  ++ rulePeriodicHolidays
  ++ rulePeriodicHolidays'
  ++ ruleComputedHolidays'
