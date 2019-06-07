-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.DA.Rules
  ( rules
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Data.Text as Text
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  -- 'man' is a common Danish word ('you'), leading to false positives
  [ ( "Monday"   , "mandag"            )
  , ( "Tuesday"  , "tirsdag|tirs?\\.?" )
  , ( "Wednesday", "onsdag|ons\\.?"    )
  , ( "Thursday" , "torsdag|tors?\\.?" )
  , ( "Friday"   , "fredag|fre\\.?"    )
  , ( "Saturday" , "lørdag|lør\\.?"    )
  , ( "Sunday"   , "søndag|søn\\.?"    )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"  , "januar|jan\\.?"      )
  , ( "February" , "februar|feb\\.?"     )
  , ( "March"    , "marts|mar\\.?"       )
  , ( "April"    , "april|apr\\.?"       )
  , ( "May"      , "maj"                 )
  , ( "June"     , "juni|jun\\.?"        )
  , ( "July"     , "juli|jul\\.?"        )
  , ( "August"   , "august|aug\\.?"      )
  , ( "September", "september|sept?\\.?" )
  , ( "October"  , "oktober|okt\\.?"     )
  , ( "November" , "november|nov\\.?"    )
  , ( "December" , "december|dec\\.?"    )
  ]

ruleTheDayAfterTomorrow :: Rule
ruleTheDayAfterTomorrow = Rule
  { name = "the day after tomorrow"
  , pattern =
    [ regex "i overmorgen"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleRelativeMinutesTotillbeforeIntegerHourofday :: Rule
ruleRelativeMinutesTotillbeforeIntegerHourofday = Rule
  { name = "relative minutes to|till|before <integer> (hour-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "i"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }

ruleQuarterTotillbeforeIntegerHourofday :: Rule
ruleQuarterTotillbeforeIntegerHourofday = Rule
  { name = "quarter to|till|before <integer> (hour-of-day)"
  , pattern =
    [ regex "(et|ét)? ?kvart(er)? i"
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
    [ regex "halv time i"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleConstitutionDay :: Rule
ruleConstitutionDay = Rule
  { name = "constitution day"
  , pattern =
    [ regex "grundlov'?s?'? dag"
    ]
  , prod = \_ -> tt $ monthDay 6 5
  }

ruleValentinesDay :: Rule
ruleValentinesDay = Rule
  { name = "valentine's day"
  , pattern =
    [ regex "valentine'?s?( dag)?"
    ]
  , prod = \_ -> tt $ monthDay 2 14
  }

ruleTheOrdinalCycleOfTime :: Rule
ruleTheOrdinalCycleOfTime = Rule
  { name = "the <ordinal> <cycle> of <time>"
  , pattern =
    [ regex "den"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "af|i|fra"
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
    [ regex "den"
    , dimension Ordinal
    , dimension Time
    , regex "af|i"
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

ruleNewYearsDay :: Rule
ruleNewYearsDay = Rule
  { name = "new year's day"
  , pattern =
    [ regex "nytårsdag"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "(sidste|seneste)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|til|indtil"
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
    [ regex "afte(s|n(en)?)"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleTheDayofmonthNonOrdinal :: Rule
ruleTheDayofmonthNonOrdinal = Rule
  { name = "the <day-of-month> (non ordinal)"
  , pattern =
    [ regex "den"
    , Predicate $ isIntegerBetween 1 31
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt $ dayOfMonth n
      _ -> Nothing
  }

ruleCycleAfterTime :: Rule
ruleCycleAfterTime = Rule
  { name = "<cycle> after <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "efter"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "om"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "lige nu|nu|(i )?dette øjeblik"
    ]
  , prod = \_ -> tt now
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "sidste"
    , dimension TimeGrain
    , regex "af|i"
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
    [ regex "fra"
    , dimension Time
    , regex "\\-|til|indtil"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleRelativeMinutesAfterpastIntegerHourofday :: Rule
ruleRelativeMinutesAfterpastIntegerHourofday = Rule
  { name = "relative minutes after|past <integer> (hour-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "over"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:
       _:
       Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleQuarterAfterpastIntegerHourofday :: Rule
ruleQuarterAfterpastIntegerHourofday = Rule
  { name = "quarter after|past <integer> (hour-of-day)"
  , pattern =
    [ regex "(et|ét)? ?kvart(er)? over"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHalfAfterpastIntegerHourofday :: Rule
ruleHalfAfterpastIntegerHourofday = Rule
  { name = "half after|past <integer> (hour-of-day)"
  , pattern =
    [ regex "halv time over"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleMonthDdddInterval :: Rule
ruleMonthDdddInterval = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ regex "([012]?\\d|30|31)(ter|\\.)?"
    , regex "\\-|til"
    , regex "([012]?\\d|30|31)(ter|\\.)?"
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
        from <- intersect td (dayOfMonth v1)
        to <- intersect td (dayOfMonth v2)
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleTheCycleAfterTime :: Rule
ruleTheCycleAfterTime = Rule
  { name = "the <cycle> after <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "(en|tet|et)? efter"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleTheCycleBeforeTime :: Rule
ruleTheCycleBeforeTime = Rule
  { name = "the <cycle> before <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "(en|tet|et)? før"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleSeason4 :: Rule
ruleSeason4 = Rule
  { name = "season"
  , pattern =
    [ regex "forår"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (monthDay 3 20) (monthDay 6 21)
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
    [ regex "n(æ)ste"
    , dimension Time
    , regex "igen"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 1 True td
      _ -> Nothing
  }

ruleTheIdesOfNamedmonth :: Rule
ruleTheIdesOfNamedmonth = Rule
  { name = "the ides of <named-month>"
  , pattern =
    [ regex "midten af"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td@TimeData {TTime.form = Just (TTime.Month m)}:_) ->
        Token Time <$>
          intersect td (dayOfMonth $ if elem m [3, 5, 7, 10] then 15 else 13)
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "middag|(kl(\\.|okken)?)? tolv"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleToday :: Rule
ruleToday = Rule
  { name = "today"
  , pattern =
    [ regex "i dag|idag"
    ]
  , prod = \_ -> tt today
  }

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "(kommende|n(æ)ste)"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleTheDayBeforeYesterday :: Rule
ruleTheDayBeforeYesterday = Rule
  { name = "the day before yesterday"
  , pattern =
    [ regex "i forgårs"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
  }

ruleBetweenTimeofdayAndTimeofdayInterval :: Rule
ruleBetweenTimeofdayAndTimeofdayInterval = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "mellem"
    , Predicate isATimeOfDay
    , regex "og"
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
    [ regex "n(æ)ste|kommende"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleTheCycleOfTime :: Rule
ruleTheCycleOfTime = Rule
  { name = "the <cycle> of <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "(en|tet|et)? af"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain 0 td
      _ -> Nothing
  }

ruleTimeofdayApproximately :: Rule
ruleTimeofdayApproximately = Rule
  { name = "<time-of-day> approximately"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(cirka|ca\\.|-?ish)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleOnDate :: Rule
ruleOnDate = Rule
  { name = "on <date>"
  , pattern =
    [ regex "den|på"
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
    , regex "fra (i dag|nu)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "(til )?middag"
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
    [ regex "sidste|seneste"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd/mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[\\/-](0?[1-9]|1[0-2])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        m <- parseInt m2
        d <- parseInt m1
        tt $ monthDay m d
      _ -> Nothing
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "eftermiddag(en)?"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleTimeBeforeLast :: Rule
ruleTimeBeforeLast = Rule
  { name = "<time> before last"
  , pattern =
    [ regex "sidste"
    , dimension Time
    , regex "igen"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth (-2) False td
      _ -> Nothing
  }

ruleNamedmonthDayofmonthOrdinal :: Rule
ruleNamedmonthDayofmonthOrdinal = Rule
  { name = "<named-month> <day-of-month> (ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleChristmasEve :: Rule
ruleChristmasEve = Rule
  { name = "christmas eve"
  , pattern =
    [ regex "juleaften(sdag)?"
    ]
  , prod = \_ -> tt $ monthDay 12 24
  }

ruleInduringThePartofday :: Rule
ruleInduringThePartofday = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "om|i"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
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
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHourofdayQuart :: Rule
ruleHourofdayQuart = Rule
  { name = "<hour-of-day> quarter (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(et|ét)? ?kvart(er)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHourofdayHalf :: Rule
ruleHourofdayHalf = Rule
  { name = "<hour-of-day> half (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "halv time"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
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
  { name = "intersect by \",\""
  , pattern =
    [ Predicate isNotLatent
    , regex ","
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
    , regex "efter"
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

ruleAfterDuration :: Rule
ruleAfterDuration = Rule
  { name = "after <duration>"
  , pattern =
    [ regex "efter"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt . withDirection TTime.After $ inDuration dd
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
        v <- getIntValue token
        tt . mkLatent $ hour False v
      _ -> Nothing
  }

ruleDayofmonthOrdinalOfNamedmonth :: Rule
ruleDayofmonthOrdinalOfNamedmonth = Rule
  { name = "<day-of-month> (ordinal) of <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , regex "af|i"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleFromTimeofdayTimeofdayInterval :: Rule
ruleFromTimeofdayTimeofdayInterval = Rule
  { name = "from <time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ regex "(efter|fra)"
    , Predicate isATimeOfDay
    , regex "((men )?før)|\\-|til|indtil"
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
    [ regex "pr(æ)cis( klokken| kl\\.?)?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleInduringThePartofday2 :: Rule
ruleInduringThePartofday2 = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "om|i"
    , Predicate isAPartOfDay
    , regex "en|ten"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleSeason3 :: Rule
ruleSeason3 = Rule
  { name = "season"
  , pattern =
    [ regex "vinter"
    ]
  , prod = \_ ->
      let from = monthDay 12 21
          to = monthDay 3 20
      in Token Time <$> interval TTime.Open from to
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "season"
  , pattern =
    [ regex "sommer"
    ]
  , prod = \_ ->
      let from = monthDay 6 21
          to = monthDay 9 23
      in Token Time <$> interval TTime.Open from to
  }

ruleBetweenDatetimeAndDatetimeInterval :: Rule
ruleBetweenDatetimeAndDatetimeInterval = Rule
  { name = "between <datetime> and <datetime> (interval)"
  , pattern =
    [ regex "mellem"
    , dimension Time
    , regex "og"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNewYearsEve :: Rule
ruleNewYearsEve = Rule
  { name = "new year's eve"
  , pattern =
    [ regex "nytårsaften(sdag)?"
    ]
  , prod = \_ -> tt $ monthDay 12 31
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ dimension Duration
    , regex "siden"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleByTheEndOfTime :: Rule
ruleByTheEndOfTime = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "i slutningen af"
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
    [ regex "efter arbejde"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 17) (hour False 21)
      Token Time . partOfDay <$> intersect today td2
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "sidste|seneste"
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
    , regex "(sharp|pr(æ)cis)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleWithinDuration :: Rule
ruleWithinDuration = Rule
  { name = "within <duration>"
  , pattern =
    [ regex "indenfor"
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
    [ regex "midnat|EOD"
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
    [ regex "(omkring|cirka|ca\\.)( klokken| kl\\.?)?"
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
    [ regex "(engang )?inden|indtil|før|op til"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "klokken|kl\\.?|@"
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
    , regex "af|i"
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
    [ regex "(week(\\s|-)?end)(en)?"
    ]
  , prod = \_ -> tt weekend
  }

ruleAboutDuration :: Rule
ruleAboutDuration = Rule
  { name = "about <duration>"
  , pattern =
    [ regex "(omkring|cirka|ca.)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> Just . Token Duration $ dd
      _ -> Nothing
  }

ruleEomendOfMonth :: Rule
ruleEomendOfMonth = Rule
  { name = "EOM|End of month"
  , pattern =
    [ regex "EOM"
    ]
  , prod = \_ -> tt $ cycleNth TG.Month 1
  }

ruleNthTimeAfterTime2 :: Rule
ruleNthTimeAfterTime2 = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ regex "den"
    , dimension Ordinal
    , dimension Time
    , regex "efter"
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
    [ regex "n(æ)ste|kommende"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
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
    [ regex "(\\d{2,4})-(0?[1-9]|1[0-2])-(3[01]|[12]\\d|0?[1-9])"
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
    [ regex "den"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "efter"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleIntersectByOfFromS :: Rule
ruleIntersectByOfFromS = Rule
  { name = "intersect by \"of\", \"from\", \"'s\""
  , pattern =
    [ Predicate isNotLatent
    , regex "i"
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
    [ regex "n(æ)ste"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain n
      _ -> Nothing
  }

ruleADuration :: Rule
ruleADuration = Rule
  { name = "a <duration>"
  , pattern =
    [ regex "(om )?en|et|én|ét"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "morgen(en)?"
    ]
  , prod = \_ ->
      let from = hour False 4
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "i|denne"
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
    [ regex "denne|dette|i|nuv(æ)rende|indev(æ)rende"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "(denne|dette|i|den her)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleDurationHence :: Rule
ruleDurationHence = Rule
  { name = "<duration> hence"
  , pattern =
    [ dimension Duration
    , regex "hence"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleDayofmonthNonOrdinalOfNamedmonth :: Rule
ruleDayofmonthNonOrdinalOfNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) of <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , regex "af|i"
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
    [ regex "efter (frokost|middag)"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 13) (hour False 17)
      Token Time . partOfDay <$> intersect today td2
  }

ruleOnANamedday :: Rule
ruleOnANamedday = Rule
  { name = "on a named-day"
  , pattern =
    [ regex "på en"
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
    [ Predicate $ isIntegerBetween (- 10000) 999
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        y <- getIntValue token
        tt . mkLatent $ year y
      _ -> Nothing
  }

ruleYesterday :: Rule
ruleYesterday = Rule
  { name = "yesterday"
  , pattern =
    [ regex "i går|igår"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleSeason2 :: Rule
ruleSeason2 = Rule
  { name = "season"
  , pattern =
    [ regex "efterår"
    ]
  , prod = \_ ->
      let from = monthDay 9 23
          to = monthDay 12 21
      in Token Time <$> interval TTime.Open from to
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "(engang )?efter"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleChristmas :: Rule
ruleChristmas = Rule
  { name = "christmas"
  , pattern =
    [ regex "((1\\.?)|første)? ?juledag"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleNight :: Rule
ruleNight = Rule
  { name = "night"
  , pattern =
    [ regex "nat(ten)?"
    ]
  , prod = \_ ->
      let from = hour False 0
          to = hour False 4
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleDayofmonthOrdinal :: Rule
ruleDayofmonthOrdinal = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern =
    [ Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        tt . mkLatent $ dayOfMonth v
      _ -> Nothing
  }

ruleOrdinalCycleAfterTime :: Rule
ruleOrdinalCycleAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "efter"
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
    , regex "af|i|fra"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute False h m
      _ -> Nothing
  }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern =
    [ regex "i aften"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect today td2
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

ruleHalloweenDay :: Rule
ruleHalloweenDay = Rule
  { name = "halloween day"
  , pattern =
    [ regex "hall?owe?en"
    ]
  , prod = \_ -> tt $ monthDay 10 31
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
    [ regex "sidste"
    , Predicate isADayOfWeek
    , regex "af|i"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleFathersDay :: Rule
ruleFathersDay = Rule
  { name = "Father's Day"
  , pattern =
    [ regex "far'?s?'? dag"
    ]
  , prod = \_ -> tt $ monthDay 6 5
  }

ruleCycleBeforeTime :: Rule
ruleCycleBeforeTime = Rule
  { name = "<cycle> before <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "før"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[\\/-](0?[1-9]|1[0-2])[\\/-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m3
        m <- parseInt m2
        d <- parseInt m1
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTimeofdayTimeofdayInterval :: Rule
ruleTimeofdayTimeofdayInterval = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\-|til|indtil"
    , Predicate isATimeOfDay
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
    , regex "efter"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationAfter dd td
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
      (token:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter False TG.Quarter (n - 1) $
          cycleNth TG.Year 0
      _ -> Nothing
  }

ruleTheDayofmonthOrdinal :: Rule
ruleTheDayofmonthOrdinal = Rule
  { name = "the <day-of-month> (ordinal)"
  , pattern =
    [ regex "den"
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
    , regex "før"
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
    , regex "(en |ten )?den"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleEoyendOfYear :: Rule
ruleEoyendOfYear = Rule
  { name = "EOY|End of year"
  , pattern =
    [ regex "EOY"
    ]
  , prod = \_ -> tt $ cycleNth TG.Year 1
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "i morgen|imorgen"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleMothersDay :: Rule
ruleMothersDay = Rule
  { name = "Mother's Day"
  , pattern =
    [ regex "mor'?s? dag"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 1 7 5
  }

ruleTimeofdayOclock :: Rule
ruleTimeofdayOclock = Rule
  { name = "<time-of-day> o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "h"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
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
         td2 <- intersectDOM td token
         Token Time <$> intersect td2 (year n)
      _ -> Nothing
  }

ruleHhmmss :: Rule
ruleHhmmss = Rule
  { name = "hh:mm:ss"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)[:.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        s <- parseInt m3
        tt $ hourMinuteSecond False h m s
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
  , ruleAboutDuration
  , ruleAboutTimeofday
  , ruleAbsorptionOfAfterNamedDay
  , ruleAfterDuration
  , ruleAfterLunch
  , ruleAfterTimeofday
  , ruleAfterWork
  , ruleAfternoon
  , ruleAtTimeofday
  , ruleBetweenDatetimeAndDatetimeInterval
  , ruleBetweenTimeofdayAndTimeofdayInterval
  , ruleByTheEndOfTime
  , ruleChristmas
  , ruleChristmasEve
  , ruleConstitutionDay
  , ruleCycleAfterTime
  , ruleCycleBeforeTime
  , ruleDatetimeDatetimeInterval
  , ruleDayofmonthNonOrdinalNamedmonth
  , ruleDayofmonthNonOrdinalOfNamedmonth
  , ruleDayofmonthOrdinal
  , ruleDayofmonthOrdinalOfNamedmonth
  , ruleDayofmonthordinalNamedmonth
  , ruleDayofmonthordinalNamedmonthYear
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleDurationAfterTime
  , ruleDurationAgo
  , ruleDurationBeforeTime
  , ruleDurationFromNow
  , ruleDurationHence
  , ruleEomendOfMonth
  , ruleEoyendOfYear
  , ruleEvening
  , ruleExactlyTimeofday
  , ruleFathersDay
  , ruleFromDatetimeDatetimeInterval
  , ruleFromTimeofdayTimeofdayInterval
  , ruleHalloweenDay
  , ruleHhmm
  , ruleHhmmss
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleInDuration
  , ruleInduringThePartofday
  , ruleInduringThePartofday2
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
  , ruleMonthDdddInterval
  , ruleMorning
  , ruleMothersDay
  , ruleNamedmonthDayofmonthNonOrdinal
  , ruleNamedmonthDayofmonthOrdinal
  , ruleNewYearsDay
  , ruleNewYearsEve
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNextTime
  , ruleNight
  , ruleNoon
  , ruleNow
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
  , ruleRelativeMinutesAfterpastIntegerHourofday
  , ruleRelativeMinutesTotillbeforeIntegerHourofday
  , ruleSeason
  , ruleSeason2
  , ruleSeason3
  , ruleSeason4
  , ruleTheCycleAfterTime
  , ruleTheCycleBeforeTime
  , ruleTheCycleOfTime
  , ruleTheDayAfterTomorrow
  , ruleTheDayBeforeYesterday
  , ruleTheDayofmonthNonOrdinal
  , ruleTheDayofmonthOrdinal
  , ruleTheIdesOfNamedmonth
  , ruleTheOrdinalCycleAfterTime
  , ruleTheOrdinalCycleOfTime
  , ruleThisCycle
  , ruleThisPartofday
  , ruleThisTime
  , ruleThisnextDayofweek
  , ruleTimeAfterNext
  , ruleTimeBeforeLast
  , ruleTimePartofday
  , ruleTimeofdayApproximately
  , ruleTimeofdayLatent
  , ruleTimeofdayOclock
  , ruleTimeofdaySharp
  , ruleTimeofdayTimeofdayInterval
  , ruleToday
  , ruleTomorrow
  , ruleTonight
  , ruleUntilTimeofday
  , ruleValentinesDay
  , ruleWeekend
  , ruleWithinDuration
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYesterday
  , ruleYyyymmdd
  , ruleQuarterTotillbeforeIntegerHourofday
  , ruleHalfTotillbeforeIntegerHourofday
  , ruleQuarterAfterpastIntegerHourofday
  , ruleHalfAfterpastIntegerHourofday
  , ruleHourofdayQuart
  , ruleHourofdayHalf
  , ruleTimezone
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
