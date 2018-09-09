-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.KM.Rules
  ( rules
  ) where

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
 [ ( "now"             , TG.Second,  0, "(ឥឡូវ|ឥលូវ)(នេះ)?" )
 , ( "today"           , TG.Day   ,  0, "ថ្ងៃនេះ" )
 , ( "tomorrow"        , TG.Day   ,  1, "(ថ្ងៃ)?ស្អែក" )
 , ( "yesterday"       , TG.Day   , -1, "(ថ្ងៃ)?ម្សិល(មិញ)?" )
 , ( "after tomorrow"  , TG.Day   ,  2, "ខានស្អែក" )
 , ( "before yesterday", TG.Day   , -2, "(ថ្ងៃ)?ម្សិលម្ង៉ៃ" )
 ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "monday"    , "(ថ្ងៃ)?ច័ន្ទ" )
  , ( "tuesday"   , "(ថ្ងៃ)?អង្គារ" )
  , ( "wednesday" , "(ថ្ងៃ)?ពុធ" )
  , ( "thursday"  , "(ថ្ងៃ)?ព្រហស្បតិ៍" )
  , ( "friday"    , "(ថ្ងៃ)?សុក្រ" )
  , ( "saturday"  , "(ថ្ងៃ)?សៅរ៍" )
  , ( "sunday"    , "(ថ្ងៃ)?អាទិត្យ" )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"   , "(ខែ)?មករា" )
  , ( "February"  , "(ខែ)?កុម្ភៈ" )
  , ( "March"     , "(ខែ)?មីនា" )
  , ( "April"    , "(ខែ)?មេសា" )
  , ( "May"      , "(ខែ)?ឧសភា" )
  , ( "June"     , "(ខែ)?មិថុនា" )
  , ( "July"     , "(ខែ)?កក្កដា" )
  , ( "August"   , "(ខែ)?សីហា" )
  , ( "September", "(ខែ)?កញ្ញា" )
  , ( "October"  , "(ខែ)?តុលា" )
  , ( "November" , "(ខែ)?វិច្ឆិកា" )
  , ( "December" , "(ខែ)?ធ្នូ" )
  ]

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "New Year's Day", "(ថ្ងៃ)?(បុណ្យ)?ចូលឆ្នាំស(ា)?កល", monthDay 1 1 )
  , ( "Independence Day", "(ថ្ងៃ)?(បុណ្យ)?ឯករាជ្យ(ជាតិ)?", monthDay 11 9 )
  ]

rules :: [Rule]
rules =
  []
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ rulePeriodicHolidays
