-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.NL.Rules
  ( rules
  ) where

import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
 [ ( "now"             , TG.Second,  0, "meteen|nu|direct|zojuist" )
 , ( "today"           , TG.Day   ,  0, "vandaag|op deze dag" )
 , ( "tomorrow"        , TG.Day   ,  1, "morgen" )
 , ( "yesterday"       , TG.Day   , -1, "gisteren" )
 , ( "after tomorrow"  , TG.Day   ,  2, "overmorgen" )
 , ( "before yesterday", TG.Day   , -2, "eergisteren" )
 , ( "EOM|End of month", TG.Month ,  1, "(het )?einde? van de maand" )
 , ( "EOY|End of year" , TG.Year  ,  1, "(het )? einde? van het jaar" )
 ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "monday"    , "maandags?|ma\\."      )
  , ( "tuesday"   , "dinsdags?|di\\."      )
  , ( "wednesday" , "woensdags?|woe\\."    )
  , ( "thursday"  , "donderdags?|do\\."    )
  , ( "friday"    , "vrijdags?|vr(ij)?\\." )
  , ( "saturday"  , "zaterdags?|zat?\\."   )
  , ( "sunday"    , "zondags?|zon?\\."     )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"   , "januari|jan\\.?"    )
  , ( "February"  , "februari|feb\\.?"   )
  , ( "March"     , "maart|mar\\.?"      )
  , ( "April"    , "april|apr\\.?"       )
  , ( "May"      , "mei\\.?"             )
  , ( "June"     , "juni?\\.?"           )
  , ( "July"     , "juli?\\.?"           )
  , ( "August"   , "augustus|aug\\.?"    )
  , ( "September", "september|sept?\\.?" )
  , ( "October"  , "oktober|okt\\.?"     )
  , ( "November" , "november|nov\\.?"    )
  , ( "December" , "december|dec\\.?"    )
  ]

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "summer" , "zomer"          , monthDay  6 21, monthDay  9 23 )
  , ( "fall"   , "herfst|najaar"  , monthDay  9 23, monthDay 12 21 )
  , ( "winter" , "winter"         , monthDay 12 21, monthDay  3 20 )
  , ( "spring" , "lente|voorjaar" , monthDay  3 20, monthDay  6 21 )
  ]

ruleHolidays :: [Rule]
ruleHolidays = mkRuleHolidays
  [ ( "Nieuwjaarsdag", "nieuwjaars?(dag)?", monthDay  1  1 )
  , ( "Valentijnsdag", "valentijns?(dag)?", monthDay  2 14 )
  , ( "Halloween", "hall?oween?", monthDay 10 31 )
  , ( "Allerheiligen", "allerheiligen?|aller heiligen?", monthDay 11  1 )
  , ( "Kerstavond", "kerstavond", monthDay 12 24 )
  , ( "Tweede Kerstdag", "tweede kerstdag", monthDay 12 26 )
  , ( "Kerstmis", "kerstmis|(eerste )?kerstdag|kerst", monthDay 12 25 )
  , ( "Oudjaar", "oudjaar|oudejaars?avond", monthDay 12 31 )
  , ( "Moederdag", "moederdag", nthDOWOfMonth 2 7 5 )
  , ( "Vaderdag", "vaderdag", nthDOWOfMonth 3 7 6 )
  ]

ruleComputedHolidays' :: [Rule]
ruleComputedHolidays' = mkRuleHolidays'
  [
  -- king's day is on April 27th unless its on Sunday in which case its a day
  -- earlier used a bit of a trick since intersectWithReplacement expects all
  -- times to be aligned in granularity (e.g once a week, twice a year, etc.)
  -- we intersect with last Sunday of April since if "4/27 is on Sunday" it
  -- will be the last Sunday on April
    ( "Koningsdag", "king's day|koningsdag"
    , let tentative = monthDay 4 27
          alternative = monthDay 4 26
          lastSundayOfApril = predLastOf (dayOfWeek 7) (month 4)
        in intersectWithReplacement lastSundayOfApril tentative alternative )
  ]
ruleRelativeMinutesToOrAfterIntegerPartOfDay :: Rule
ruleRelativeMinutesToOrAfterIntegerPartOfDay = Rule
  { name = "relative minutes to|till|before|after <integer> (time-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "(voor|over|na)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) -> case Text.toLower match of
        "voor" -> tt $ durationBefore (duration TG.Minute $ floor v) td
        _        -> tt $ durationAfter (duration TG.Minute $ floor v) td
      _ -> Nothing
  }

ruleQuarterTotillbeforeIntegerHourofday :: Rule
ruleQuarterTotillbeforeIntegerHourofday = Rule
  { name = "quarter to|till|before <integer> (hour-of-day)"
  , pattern =
    [ regex "kwart(ier)? voor"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleHalfTotillbeforeIntegerHourofday :: Rule
ruleHalfTotillbeforeIntegerHourofday = Rule
  { name = "half to|till|before <integer> (hour-of-day)"
  , pattern =
    [ regex "half"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleTheOrdinalCycleOfTime :: Rule
ruleTheOrdinalCycleOfTime = Rule
  { name = "the <ordinal> <cycle> of <time>"
  , pattern =
    [ regex "de|het"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "van|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleNthTimeOfTime2 :: Rule
ruleNthTimeOfTime2 = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ regex "de|het"
    , dimension Ordinal
    , dimension Time
    , regex "in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "afgelopen|vorige?"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|t\\/m|tot en met"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleEvening :: Rule
ruleEvening = Rule
  { name = "evening"
  , pattern =
    [ regex "\\'s avonds|avond"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

-- TODO: Single-word composition (#110)
ruleEvenings :: Rule
ruleEvenings = Rule
  { name = "today/tomorrow/yesterday evening"
  , pattern =
    [ regex "(van|morgen|gister(en)?)avond"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        td1 <- case Text.toLower match of
          "van"    -> Just today
          "morgen" -> Just $ cycleNth TG.Day 1
          _        -> Just $ cycleNth TG.Day $ - 1
        td2 <- interval TTime.Open (hour False 18) (hour False 0)
        Token Time . partOfDay <$> intersect td1 td2
      _ -> Nothing
  }

ruleTheDayofmonthNonOrdinal :: Rule
ruleTheDayofmonthNonOrdinal = Rule
  { name = "the <day-of-month> (non ordinal)"
  , pattern =
    [ regex "de"
    , Predicate $ isIntegerBetween 1 31
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "in|over"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "laatste|vorige|afgelopen"
    , dimension TimeGrain
    , regex "van|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleFromDatetimeDatetimeInterval :: Rule
ruleFromDatetimeDatetimeInterval = Rule
  { name = "from <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "van"
    , dimension Time
    , regex "\\-|tot"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleQuarterAfterpastIntegerHourofday :: Rule
ruleQuarterAfterpastIntegerHourofday = Rule
  { name = "quarter after|past <integer> (hour-of-day)"
  , pattern =
    [ regex "kwart(ier)? over"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time TimeData
       {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleMonthDdddInterval :: Rule
ruleMonthDdddInterval = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ regex "([012]?\\d|30|31)(ste|de)?"
    , regex "\\-|tot en met|t\\/m"
    , regex "([012]?\\d|30|31)(ste|de)?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       _:
       Token RegexMatch (GroupMatch (m2:_)):
       Token Time td:
       _) -> do
        v1 <- parseInt m1
        v2 <- parseInt m2
        from <- intersect (dayOfMonth v1) td
        to <- intersect (dayOfMonth v2) td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleTheCycleAfterTime :: Rule
ruleTheCycleAfterTime = Rule
  { name = "the <cycle> after <time>"
  , pattern =
    [ regex "de|het"
    , dimension TimeGrain
    , regex "na"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleTheCycleBeforeTime :: Rule
ruleTheCycleBeforeTime = Rule
  { name = "the <cycle> before <time>"
  , pattern =
    [ regex "de"
    , dimension TimeGrain
    , regex "voor"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleYearLatent2 :: Rule
ruleYearLatent2 = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 2101 10000
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ year v
      _ -> Nothing
  }

ruleTimeAfterNext :: Rule
ruleTimeAfterNext = Rule
  { name = "<time> after next"
  , pattern =
    [ dimension Time
    , regex "navolgend"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 True td
      _ -> Nothing
  }

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "aanstaande|komende|volgende"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleBetweenTimeofdayAndTimeofdayInterval :: Rule
ruleBetweenTimeofdayAndTimeofdayInterval = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "tussen"
    , Predicate isATimeOfDay
    , regex "en"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNextCycle :: Rule
ruleNextCycle = Rule
  { name = "next <cycle>"
  , pattern =
    [ regex "aanstaande|komende?|volgende?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleTimeofdayApproximately :: Rule
ruleTimeofdayApproximately = Rule
  { name = "<time-of-day> approximately"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "ongeveer"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleOnDate :: Rule
ruleOnDate = Rule
  { name = "on <date>"
  , pattern =
    [ regex "op( de)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleDurationFromNow :: Rule
ruleDurationFromNow = Rule
  { name = "<duration> from now"
  , pattern =
    [ dimension Duration
    , regex "vanaf (vandaag|nu)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "(am |zu )?mittags?"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 14
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleLastCycle :: Rule
ruleLastCycle = Rule
  { name = "last <cycle>"
  , pattern =
    [ regex "vorige?|afgelopen"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "('s )?middags?|(in de )?middag|namiddag"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 18
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

-- TODO: Single-word composition (#110)
ruleAfternoons :: Rule
ruleAfternoons = Rule
  { name = "today/tomorrow/yesterday afternoon"
  , pattern =
    [ regex "(vanmiddag|morgenmiddags?|gistermiddag|gisteren(na)?middag)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        td1 <- case Text.toLower match of
          "vanmiddag"     -> Just today
          "morgenmiddag"  -> Just $ cycleNth TG.Day 1
          "morgenmiddags" -> Just $ cycleNth TG.Day 1
          _               -> Just . cycleNth TG.Day $ - 1
        td2 <- interval TTime.Open (hour False 12) (hour False 18)
        Token Time . partOfDay <$> intersect td1 td2
      _ -> Nothing
  }

ruleNamedmonthDayofmonthOrdinal :: Rule
ruleNamedmonthDayofmonthOrdinal = Rule
  { name = "<day-of-month> (ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleInduringThePartofday :: Rule
ruleInduringThePartofday = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "(tijdens|gedurende?)( de)?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleHourofdayIntegerAsRelativeMinutes :: Rule
ruleHourofdayIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData
       {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleDayofmonthordinalNamedmonth :: Rule
ruleDayofmonthordinalNamedmonth = Rule
  { name = "<day-of-month>(ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleIntersectBy :: Rule
ruleIntersectBy = Rule
  { name = "intersect by ','"
  , pattern =
    [ Predicate isNotLatent
    , regex ",( den|r)?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "na"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> tt $ predNthAfter (v - 1) td1 td2
      _ -> Nothing
  }

ruleMmdd :: Rule
ruleMmdd = Rule
  { name = "dd/mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s?[/.-]\\s?(1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        d <- parseInt m1
        m <- parseInt m2
        tt $ monthDay m d
      _ -> Nothing
  }

ruleAfterDuration :: Rule
ruleAfterDuration = Rule
  { name = "after <duration>"
  , pattern =
    [ regex "over|na"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleTimeofdayLatent :: Rule
ruleTimeofdayLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (n < 12) n
      _ -> Nothing
  }

ruleFromTimeofdayTimeofdayInterval :: Rule
ruleFromTimeofdayTimeofdayInterval = Rule
  { name = "from <time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ regex "(van|vanaf|na|vroegst om|om)"
    , Predicate isATimeOfDay
    , regex "((maar)? voor)|\\-|tot"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleExactlyTimeofday :: Rule
ruleExactlyTimeofday = Rule
  { name = "exactly <time-of-day>"
  , pattern =
    [ regex "(stipt|exact|precies)( om)?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleBetweenDatetimeAndDatetimeInterval :: Rule
ruleBetweenDatetimeAndDatetimeInterval = Rule
  { name = "between <datetime> and <datetime> (interval)"
  , pattern =
    [ regex "tussen"
    , dimension Time
    , regex "en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDurationHenceAgo :: Rule
ruleDurationHenceAgo = Rule
  { name = "<duration> hence|ago"
  , pattern =
    [ dimension Duration
    , regex "(later|geleden)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
        "geleden" -> tt $ durationAgo dd
        _ -> tt $ inDuration dd
      _ -> Nothing
  }

ruleByTheEndOfTime :: Rule
ruleByTheEndOfTime = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "tot( het)? einde( van)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Closed now td
      _ -> Nothing
  }

ruleAfterWork :: Rule
ruleAfterWork = Rule
  { name = "after work"
  , pattern =
    [ regex "na het werk"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 17) (hour False 21)
      Token Time . partOfDay <$> intersect today td2
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "laatste|afgelopen|vorige?"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain (- n)
      _ -> Nothing
  }

ruleTimeofdaySharp :: Rule
ruleTimeofdaySharp = Rule
  { name = "<time-of-day> sharp"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "genau|exakt|p(ü)nktlich|punkt( um)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleWithinDuration :: Rule
ruleWithinDuration = Rule
  { name = "within <duration>"
  , pattern =
    [ regex "binnen|innerhalb( von)?"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> Token Time <$>
        interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }

ruleMidnighteodendOfDay :: Rule
ruleMidnighteodendOfDay = Rule
  { name = "midnight|EOD|end of day"
  , pattern =
    [ regex "middernacht|EOD|einde? van de dag"
    ]
  , prod = \_ -> tt $ hour False 0
  }

ruleDayofmonthNonOrdinalNamedmonth :: Rule
ruleDayofmonthNonOrdinalNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isNotLatent
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleAboutTimeofday :: Rule
ruleAboutTimeofday = Rule
  { name = "about <time-of-day>"
  , pattern =
    [ regex "ongeveer|circa|ca\\.|tegen|rond"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleUntilTimeofday :: Rule
ruleUntilTimeofday = Rule
  { name = "until <time-of-day>"
  , pattern =
    [ regex "voor|tot|op zijn laatst om"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleUntilTimeofdayPostfix :: Rule
ruleUntilTimeofdayPostfix = Rule
  { name = "<time-of-day> until"
  , pattern =
    [ dimension Time
    , regex "op zijn laatst"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "om|@"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "in|van"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleTimePartofday :: Rule
ruleTimePartofday = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "week(\\s|-)?ei?nde?"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

ruleNthTimeAfterTime2 :: Rule
ruleNthTimeAfterTime2 = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ regex "de|het"
    , dimension Ordinal
    , dimension Time
    , regex "na"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> tt $ predNthAfter (v - 1) td1 td2
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "(volgende?|komende?)"
    , Predicate $ and . sequence [isOkWithThisNext, not . isATimeOfDay]
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleOrdinalQuarterYear :: Rule
ruleOrdinalQuarterYear = Rule
  { name = "<ordinal> quarter <year>"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:_:Token Time td:_) ->
        tt $ cycleNthAfter False TG.Quarter (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleYyyymmdd :: Rule
ruleYyyymmdd = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})[.-](1[0-2]|0?[1-9])[.-](3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m1
        m <- parseInt m2
        d <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTheOrdinalCycleAfterTime :: Rule
ruleTheOrdinalCycleAfterTime = Rule
  { name = "the <ordinal> <cycle> after <time>"
  , pattern =
    [ regex "de|het"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "na"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleIntersectByOfFromS :: Rule
ruleIntersectByOfFromS = Rule
  { name = "intersect by 'of', 'from', 's"
  , pattern =
    [ Predicate isNotLatent
    , regex "von|der|im"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleNextNCycle :: Rule
ruleNextNCycle = Rule
  { name = "next n <cycle>"
  , pattern =
    [ regex "(volgende?|komende?)"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleADuration :: Rule
ruleADuration = Rule
  { name = "a <duration>"
  , pattern =
    [ regex "(in )?eine?(r|n)?"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "(in de )?morgen( van)?|(in de )?ochtend|\\'s ochtends|\\'s morgens"
    ]
  , prod = \_ ->
      let from = hour False 0
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

-- TODO: Single-word composition (#110)
ruleMornings :: Rule
ruleMornings = Rule
  { name = "today/tomorrow/yesterday morning"
  , pattern =
    [ regex "(vanmorgen|morgenochtend|gisterenochtend)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        td1 <- case Text.toLower match of
          "vanmorgen"     -> Just today
          "morgenochtend" -> Just $ cycleNth TG.Day 1
          _               -> Just . cycleNth TG.Day $ - 1
        td2 <- interval TTime.Open (hour False 0) (hour False 12)
        Token Time . partOfDay <$> intersect td1 td2
      _ -> Nothing
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "deze|dit"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay <$> intersect today td
      _ -> Nothing
  }

ruleThisCycle :: Rule
ruleThisCycle = Rule
  { name = "this <cycle>"
  , pattern =
    [ regex "dit|deze|komende?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "dit|deze|huidige?"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

ruleDayofmonthNonOrdinalOfNamedmonth :: Rule
ruleDayofmonthNonOrdinalOfNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) of <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , regex "van"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleAfterLunch :: Rule
ruleAfterLunch = Rule
  { name = "after lunch"
  , pattern =
    [ regex "na de lunch|na het middageten"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 13) (hour False 17)
      Token Time . partOfDay <$> intersect today td2
  }

ruleOnANamedday :: Rule
ruleOnANamedday = Rule
  { name = "on a named-day"
  , pattern =
    [ regex "op( een)?"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 999]
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        y <- getIntValue token
        tt . mkLatent $ year y
      _ -> Nothing
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "na|vanaf|op zijn vroegst"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleAfterTimeofdayPostfix :: Rule
ruleAfterTimeofdayPostfix = Rule
  { name = "<time-of-day> after"
  , pattern =
    [ dimension Time
    , regex "op zijn vroegst"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleNight :: Rule
ruleNight = Rule
  { name = "night"
  , pattern =
    [ regex "(\\'s nachts|(in de )?nacht)"
    ]
  , prod = \_ ->
      let from = hour False 20
          to = hour False 8
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

-- TODO: Single-word composition (#110)
ruleNights :: Rule
ruleNights = Rule
  { name = "yesterday/tomorrow night"
  , pattern =
    [ regex "(morgen|gisteren)nacht"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        td1 <- case Text.toLower match of
          "morgen" -> Just $ cycleNth TG.Day 1
          _        -> Just . cycleNth TG.Day $ - 1
        td2 <- interval TTime.Open (hour False 18) (hour False 0)
        Token Time . partOfDay <$> intersect td1 td2
      _ -> Nothing
  }

ruleDayofmonthOrdinal :: Rule
ruleDayofmonthOrdinal = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern =
    [ Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleTimeofdayAmpm :: Rule
ruleTimeofdayAmpm = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "([ap])\\.?m\\.?(?:[\\s'\"-_{}\\[\\]()]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
  }

ruleOrdinalCycleAfterTime :: Rule
ruleOrdinalCycleAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "na"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleOrdinalCycleOfTime :: Rule
ruleOrdinalCycleOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "in|van"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleAfterNextTime :: Rule
ruleAfterNextTime = Rule
  { name = "after next <time>"
  , pattern =
    [ regex "(ü)ber ?n(ä)chste[ns]?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 1 True td
      _ -> Nothing
  }

ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern = [ regex "((?:[01]?\\d)|(?:2[0-3]))[:hu]([0-5]\\d)(?:uur|u|h)?" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute False h m
      _ -> Nothing
  }

ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        y <- getIntValue token
        tt $ year y
      _ -> Nothing
  }

ruleNamedmonthDayofmonthNonOrdinal :: Rule
ruleNamedmonthDayofmonthNonOrdinal = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleHhmmMilitary :: Rule
ruleHhmmMilitary = Rule
  { name = "hhmm (military)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:m:_)):_) -> do
        hh <- parseInt h
        mm <- parseInt m
        tt . mkLatent $ hourMinute False hh mm
      _ -> Nothing
  }

ruleAbsorptionOfAfterNamedDay :: Rule
ruleAbsorptionOfAfterNamedDay = Rule
  { name = "absorption of , after named day"
  , pattern =
    [ Predicate isADayOfWeek
    , regex ","
    ]
  , prod = \tokens -> case tokens of
      (x:_) -> Just x
      _ -> Nothing
  }

ruleLastDayofweekOfTime :: Rule
ruleLastDayofweekOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "laatste"
    , Predicate isADayOfWeek
    , regex "in|van"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) -> tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleHhmmMilitaryAmpm :: Rule
ruleHhmmMilitaryAmpm = Rule
  { name = "hhmm (military) am|pm"
  , pattern =
    [ regex "((?:1[012]|0?\\d))([0-5]\\d)"
    , regex "([ap])\\.?m\\.?(?:[\\s'\"-_{}\\[\\]()]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):Token RegexMatch (GroupMatch (ap:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True h m
      _ -> Nothing
  }

ruleTimeofdayTimeofdayInterval :: Rule
ruleTimeofdayTimeofdayInterval = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\-|tot"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleTimeofdayTimeofdayInterval2 :: Rule
ruleTimeofdayTimeofdayInterval2 = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "\\-|tot"
    , Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDurationAfterTime :: Rule
ruleDurationAfterTime = Rule
  { name = "<duration> after <time>"
  , pattern =
    [ dimension Duration
    , regex "na"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) -> tt $ durationAfter dd td
      _ -> Nothing
  }

ruleOrdinalQuarter :: Rule
ruleOrdinalQuarter = Rule
  { name = "<ordinal> quarter"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) -> tt .
        cycleNthAfter False TG.Quarter (v - 1) $ cycleNth TG.Year 0
      _ -> Nothing
  }

ruleTheDayofmonthOrdinal :: Rule
ruleTheDayofmonthOrdinal = Rule
  { name = "the <day-of-month> (ordinal)"
  , pattern =
    [ regex "der"
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleDurationBeforeTime :: Rule
ruleDurationBeforeTime = Rule
  { name = "<duration> before <time>"
  , pattern =
    [ dimension Duration
    , regex "voor"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

rulePartofdayOfTime :: Rule
rulePartofdayOfTime = Rule
  { name = "<part-of-day> of <time>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "(van|op|in)( de)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleMmddyyyy :: Rule
ruleMmddyyyy = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/.-](1[0-2]|0?[1-9])[/.-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m3
        m <- parseInt m2
        d <- parseInt m1
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTimeofdayOclock :: Rule
ruleTimeofdayOclock = Rule
  { name = "<time-of-day>  o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "uur|h|u(?:[\\s'\"-_{}\\[\\]()]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleDayofmonthordinalNamedmonthYear :: Rule
ruleDayofmonthordinalNamedmonthYear = Rule
  { name = "<day-of-month>(ordinal) <named-month> year"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:
       Token Time td:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
        n <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year n)
      _ -> Nothing
  }

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleADuration
  , ruleAboutTimeofday
  , ruleAbsorptionOfAfterNamedDay
  , ruleAfterDuration
  , ruleAfterLunch
  , ruleAfterNextTime
  , ruleAfterTimeofday
  , ruleAfterTimeofdayPostfix
  , ruleAfterWork
  , ruleAfternoon
  , ruleAfternoons
  , ruleAtTimeofday
  , ruleBetweenDatetimeAndDatetimeInterval
  , ruleBetweenTimeofdayAndTimeofdayInterval
  , ruleByTheEndOfTime
  , ruleDatetimeDatetimeInterval
  , ruleDayofmonthNonOrdinalNamedmonth
  , ruleDayofmonthNonOrdinalOfNamedmonth
  , ruleDayofmonthOrdinal
  , ruleDayofmonthordinalNamedmonth
  , ruleDayofmonthordinalNamedmonthYear
  , ruleDurationAfterTime
  , ruleDurationHenceAgo
  , ruleDurationBeforeTime
  , ruleDurationFromNow
  , ruleEvening
  , ruleEvenings
  , ruleExactlyTimeofday
  , ruleFromDatetimeDatetimeInterval
  , ruleFromTimeofdayTimeofdayInterval
  , ruleHhmm
  , ruleHhmmMilitary
  , ruleHhmmMilitaryAmpm
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleInDuration
  , ruleInduringThePartofday
  , ruleIntersect
  , ruleIntersectBy
  , ruleIntersectByOfFromS
  , ruleLastCycle
  , ruleLastCycleOfTime
  , ruleLastDayofweekOfTime
  , ruleLastNCycle
  , ruleLastTime
  , ruleLunch
  , ruleMidnighteodendOfDay
  , ruleMmdd
  , ruleMmddyyyy
  , ruleMonthDdddInterval
  , ruleMorning
  , ruleMornings
  , ruleNamedmonthDayofmonthNonOrdinal
  , ruleNamedmonthDayofmonthOrdinal
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNextTime
  , ruleNight
  , ruleNights
  , ruleNthTimeAfterTime
  , ruleNthTimeAfterTime2
  , ruleNthTimeOfTime
  , ruleNthTimeOfTime2
  , ruleOnANamedday
  , ruleOnDate
  , ruleOrdinalCycleAfterTime
  , ruleOrdinalCycleOfTime
  , ruleOrdinalQuarter
  , ruleOrdinalQuarterYear
  , rulePartofdayOfTime
  , ruleRelativeMinutesToOrAfterIntegerPartOfDay
  , ruleTheCycleAfterTime
  , ruleTheCycleBeforeTime
  , ruleTheDayofmonthNonOrdinal
  , ruleTheDayofmonthOrdinal
  , ruleTheOrdinalCycleAfterTime
  , ruleTheOrdinalCycleOfTime
  , ruleThisCycle
  , ruleThisPartofday
  , ruleThisTime
  , ruleThisnextDayofweek
  , ruleTimeAfterNext
  , ruleTimePartofday
  , ruleTimeofdayAmpm
  , ruleTimeofdayApproximately
  , ruleTimeofdayLatent
  , ruleTimeofdayOclock
  , ruleTimeofdaySharp
  , ruleTimeofdayTimeofdayInterval
  , ruleTimeofdayTimeofdayInterval2
  , ruleUntilTimeofday
  , ruleUntilTimeofdayPostfix
  , ruleWeekend
  , ruleWithinDuration
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYyyymmdd
  , ruleQuarterTotillbeforeIntegerHourofday
  , ruleHalfTotillbeforeIntegerHourofday
  , ruleQuarterAfterpastIntegerHourofday
  , ruleTimezone
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ ruleHolidays
  ++ ruleComputedHolidays'
