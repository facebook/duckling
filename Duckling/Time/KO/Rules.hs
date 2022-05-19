-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.KO.Rules
  ( rules ) where

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (isGrain)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Types (OrdinalData(..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData(..))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleHolidays :: [Rule]
ruleHolidays = mkRuleHolidays
  [ ( "Liberation Day"            , "광복절"
    , monthDay  8   15 )
  , ( "Constitution Day"          , "제헌절"
    , monthDay  7   17 )
  , ( "New Year's Day"            , "(신정)|(설날)"
    , monthDay  1   1  )
  , ( "Hangul Day"                , "한글날"
    , monthDay  10  9  )
  , ( "National Foundation Day"   , "개천절"
    , monthDay  10  3  )
  , ( "Independence Movement Day" , "삼일절"
    , monthDay  3   1  )
  , ( "Memorial Day"              , "현충일"
    , monthDay  6   6  )
  , ( "Christmas"                 , "크리스마스"
    , monthDay  12  25 )
  , ( "Christmas Eve"             , "(크리스마스)?이브"
    , monthDay  12  24 )
  , ( "Children's Day"            , "어린이날"
    , monthDay  5   5  )
  ]

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "Summer"  , "여름"  , monthDay  6   21, monthDay  9   23 )
  , ( "Fall"    , "가을"  , monthDay  9   23, monthDay  12  21 )
  , ( "Winter"  , "겨울"  , monthDay  12  21, monthDay  3   20 )
  , ( "Spring"  , "봄"    , monthDay  3   20, monthDay  6   21 )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"    , "월(요일|욜)" )
  , ( "Tuesday"   , "화(요일|욜)" )
  , ( "Wednesday" , "수(요일|욜)" )
  , ( "Thursday"  , "목(요일|욜)" )
  , ( "Friday"    , "금(요일|욜)" )
  , ( "Saturday"  , "토(요일|욜)" )
  , ( "Sunday"    , "일(요일|욜)" )
  ]

ruleNamedday :: Rule
ruleNamedday = Rule
  { name = "<named-day>에"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "에"
    ]
  , prod = \tokens -> case tokens of
      (x:_) -> Just x
      _ -> Nothing
  }

ruleTheDayAfterTomorrow :: Rule
ruleTheDayAfterTomorrow = Rule
  { name = "the day after tomorrow - 내일모레"
  , pattern =
    [ regex "(내일)?모\xb808"
    ]
  , prod = \_ ->
      tt . cycleNthAfter False TG.Day 1 $ cycleNth TG.Day 1
  }

ruleTimeofday4 :: Rule
ruleTimeofday4 = Rule
  { name = "<time-of-day>이전"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(이)?전"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleDay :: Rule
ruleDay = Rule
  { name = "day"
  , pattern =
    [ Predicate isDOMInteger
    , regex "일"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleSinceTimeofday :: Rule
ruleSinceTimeofday = Rule
  { name = "since <time-of-day>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "이래로"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt . withDirection TTime.After $ predNth (- 1) False td
      _ -> Nothing
  }

ruleThisDayofweek :: Rule
ruleThisDayofweek = Rule
  { name = "this <day-of-week>"
  , pattern =
    [ regex "이번(주)?|금주"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "전|저번|지난"
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
    , regex "\\-|\\~|부터"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ dimension Duration
    , regex "(안|내)(에)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "방금|지금|방금|막"
    ]
  , prod = \_ -> tt now
  }

ruleMonth :: Rule
ruleMonth = Rule
  { name = "month"
  , pattern =
    [ Predicate $ isIntegerBetween 1 12
    , regex "월"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt $ month v
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
    , regex "after next"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 True td
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "정오"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleToday :: Rule
ruleToday = Rule
  { name = "today"
  , pattern =
    [ regex "오늘|당일|금일"
    ]
  , prod = \_ -> tt today
  }

ruleIntegerHourofdayRelativeMinutes :: Rule
ruleIntegerHourofdayRelativeMinutes = Rule
  { name = "<integer> (hour-of-day) relative minutes 전"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    , regex "분전"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> do
        v <- getIntValue token
        Token Time <$> minutesBefore v td
      _ -> Nothing
  }

ruleHourofdayIntegerAsRelativeMinutes :: Rule
ruleHourofdayIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    , regex "분"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute True hours n
      _ -> Nothing
  }

ruleHalfHourofdayRelativeMinutes :: Rule
ruleHalfHourofdayRelativeMinutes = Rule
  { name = "half (hour-of-day) relative minutes 전"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "반전"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleHourofdayHalfAsRelativeMinutes :: Rule
ruleHourofdayHalfAsRelativeMinutes = Rule
  { name = "<hour-of-day> half (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "반"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 30
      _ -> Nothing
  }

ruleSeconds :: Rule
ruleSeconds = Rule
  { name = "seconds"
  , pattern =
    [ Predicate $ isIntegerBetween 0 59
    , regex "초"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt $ second v
      _ -> Nothing
  }

ruleTimeOrdinalCycle :: Rule
ruleTimeOrdinalCycle = Rule
  { name = "<time> <ordinal> <cycle>"
  , pattern =
    [ dimension Time
    , dimension Ordinal
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Ordinal od:Token TimeGrain grain:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleTheDayBeforeYesterday :: Rule
ruleTheDayBeforeYesterday = Rule
  { name = "the day before yesterday - 엊그제"
  , pattern =
    [ regex "(엊)?그(제|재)"
    ]
  , prod = \_ ->
      tt . cycleNthAfter False TG.Day (-1) $ cycleNth TG.Day (-1)
  }

ruleNextCycle :: Rule
ruleNextCycle = Rule
  { name = "next <cycle>"
  , pattern =
    [ regex "다음|오는|차|내"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleNamedmonth :: Rule
ruleNamedmonth = Rule
  { name = "<named-month>에"
  , pattern =
    [ Predicate isAMonth
    , regex "에"
    ]
  , prod = \tokens -> case tokens of
      (x:_) -> Just x
      _ -> Nothing
  }

ruleTimeofdayApproximately :: Rule
ruleTimeofdayApproximately = Rule
  { name = "<time-of-day> approximately"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "정도|쯤"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleDurationFromNow :: Rule
ruleDurationFromNow = Rule
  { name = "<duration> from now"
  , pattern =
    [ regex "지금부터"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "점심"
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
    [ regex "지난|작|전|저번"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "오후"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleInduringThePartofday :: Rule
ruleInduringThePartofday = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "에|동안"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
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

ruleMmdd :: Rule
ruleMmdd = Rule
  { name = "mm/dd"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])/(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleAfterDuration :: Rule
ruleAfterDuration = Rule
  { name = "after <duration>"
  , pattern =
    [ dimension Duration
    , regex "(이)?후"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
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
        tt . mkLatent $ hour True v
      _ -> Nothing
  }

ruleExactlyTimeofday :: Rule
ruleExactlyTimeofday = Rule
  { name = "exactly <time-of-day>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "정각"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleDayWithKoreanNumeral :: Rule
ruleDayWithKoreanNumeral = Rule
  { name = "day with korean number - 십일..삼십일일"
  , pattern =
    [ regex "((이|삼)?십(일|이|삼|사|오|육|칠|팔|구)?)일"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:m1:m2:_)):_) ->
        let dozens = case m1 of
              "이" -> 2
              "삼" -> 3
              _        -> 1
            units = case m2 of
              "일" -> 1
              "이" -> 2
              "삼" -> 3
              "사" -> 4
              "오" -> 5
              "육" -> 6
              "칠" -> 7
              "팔" -> 8
              "구" -> 9
              _        -> 1
        in tt . dayOfMonth $ 10 * dozens + units
      _ -> Nothing
  }

ruleDayWithKoreanNumeral2 :: Rule
ruleDayWithKoreanNumeral2 = Rule
  { name = "day with korean number - 일일..구일"
  , pattern =
    [ regex "(일|이|삼|사|오|육|칠|팔|구)일"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "일" -> tt $ dayOfMonth 1
        "이" -> tt $ dayOfMonth 2
        "삼" -> tt $ dayOfMonth 3
        "사" -> tt $ dayOfMonth 4
        "오" -> tt $ dayOfMonth 5
        "육" -> tt $ dayOfMonth 6
        "칠" -> tt $ dayOfMonth 7
        "팔" -> tt $ dayOfMonth 8
        "구" -> tt $ dayOfMonth 9
        _ -> Nothing
      _ -> Nothing
  }

ruleTimeofday2 :: Rule
ruleTimeofday2 = Rule
  { name = "<time-of-day>에"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "에"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ dimension Duration
    , regex "(이)?전"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "지난"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleTimeNthTime :: Rule
ruleTimeNthTime = Rule
  { name = "<time> nth <time> - 3월 첫째 화요일"
  , pattern =
    [ dimension Time
    , dimension Ordinal
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:
       Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td1 td2
      _ -> Nothing
  }

ruleWithinDuration :: Rule
ruleWithinDuration = Rule
  { name = "within <duration>"
  , pattern =
    [ dimension Duration
    , regex "이내(에)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        Token Time <$> interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }

ruleMidnighteodendOfDay :: Rule
ruleMidnighteodendOfDay = Rule
  { name = "midnight|EOD|end of day"
  , pattern =
    [ regex "자정"
    ]
  , prod = \_ -> tt $ hour False 0
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
    [ regex "대충|약"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleEndOfTime :: Rule
ruleEndOfTime = Rule
  { name = "end of <time>"
  , pattern =
    [ dimension Time
    , regex "말"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 False td
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
    [ regex "주말"
    ]
  , prod = \_ -> tt weekend
  }

ruleTimeDayofweek :: Rule
ruleTimeDayofweek = Rule
  { name = "<time> 마지막 <day-of-week>"
  , pattern =
    [ dimension Time
    , regex "마지막"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td2 td1
      _ -> Nothing
  }

ruleDate :: Rule
ruleDate = Rule
  { name = "<date>에"
  , pattern =
    [ dimension Time
    , regex "에"
    ]
  , prod = \tokens -> case tokens of
      (x:_) -> Just x
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "다음|오는"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleTimeCycle2 :: Rule
ruleTimeCycle2 = Rule
  { name = "<time> 마지막 <cycle>"
  , pattern =
    [ dimension Time
    , regex "마지막"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:Token TimeGrain grain:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleYyyymmdd :: Rule
ruleYyyymmdd = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})-(0?[1-9]|1[0-2])-(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleNextNCycle :: Rule
ruleNextNCycle = Rule
  { name = "next n <cycle>"
  , pattern =
    [ regex "다음"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "아침"
    ]
  , prod = \_ ->
      let from = hour False 4
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleThisCycle :: Rule
ruleThisCycle = Rule
  { name = "this <cycle>"
  , pattern =
    [ regex "이번|금|올"
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
    [ regex "이번"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleTimeofday3 :: Rule
ruleTimeofday3 = Rule
  { name = "<time-of-day> 정각"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "정각"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 999
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ year v
      _ -> Nothing
  }

ruleYesterday :: Rule
ruleYesterday = Rule
  { name = "yesterday"
  , pattern =
    [ regex "어제"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ Predicate $ or . sequence [isATimeOfDay, isAPartOfDay]
    , regex "지나서|(이)?후(에)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt . notLatent $ withDirection TTime.After td
      _ -> Nothing
  }

ruleTimeofdayAmpm :: Rule
ruleTimeofdayAmpm = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(in the )?([ap])(\\s|\\.)?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (_:ap:_)):_) ->
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
  }

ruleTimeCycle :: Rule
ruleTimeCycle = Rule
  { name = "<time> 마지막 <cycle> "
  , pattern =
    [ dimension Time
    , regex "마지막"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

-- (assoc (intersect (cycle-nth :day 0)
--                   (interval (hour (inc (:start %1)) false)
--                             (hour (inc (:end %1)) false) false))
--        :form :part-of-day)
ruleAfterPartofday :: Rule
ruleAfterPartofday = Rule
  { name = "after <part-of-day>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "지나서|후에"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time . partOfDay <$> intersect today td
      _ -> Nothing
  }

ruleTimeofday :: Rule
ruleTimeofday = Rule
  { name = "time-of-day"
  , pattern =
    [ Predicate $ isIntegerBetween 0 24
    , regex "시"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt $ hour True v
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
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleYearQuarter :: Rule
ruleYearQuarter = Rule
  { name = "<year> <1..4>quarter"
  , pattern =
    [ dimension Time
    , Predicate $ isIntegerBetween 1 4
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> do
        v <- getIntValue token
        tt $ cycleNthAfter False TG.Quarter (v - 1) td
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
        v <- getIntValue token
        tt $ year v
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

ruleByTime :: Rule
ruleByTime = Rule
  { name = "by <time> - 까지"
  , pattern =
    [ dimension Time
    , regex "까지"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleHhmmMilitaryAmpm :: Rule
ruleHhmmMilitaryAmpm = Rule
  { name = "hhmm (military) am|pm"
  , pattern =
    [ regex "((?:1[012]|0?\\d))([0-5]\\d)"
    , regex "([ap])\\.?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):
       Token RegexMatch (GroupMatch (ap:_)):
       _) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True h m
      _ -> Nothing
  }

ruleQuarter :: Rule
ruleQuarter = Rule
  { name = "<1..4> quarter"
  , pattern =
    [ Predicate $ isIntegerBetween 1 4
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . cycleNthAfter False TG.Quarter (v - 1) $
          cycleNth TG.Year 0
      _ -> Nothing
  }

ruleTimeofdayTimeofdayInterval :: Rule
ruleTimeofdayTimeofdayInterval = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\-|\\~|부터"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleEveningnight :: Rule
ruleEveningnight = Rule
  { name = "evening|night"
  , pattern =
    [ regex "저녁|밤"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . partOfDay . mkLatent <$>
           interval TTime.Open from to
  }

ruleMmddyyyy :: Rule
ruleMmddyyyy = Rule
  { name = "mm/dd/yyyy"
  , pattern =
    [ regex "(\\d{2,4})[-/](0?[1-9]|1[0-2])[/-](3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "내일|명일|낼"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleAmpmTimeofday :: Rule
ruleAmpmTimeofday = Rule
  { name = "am|pm <time-of-day>"
  , pattern =
    [ regex "(오전|아침|오후|저녁)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) -> tt $ timeOfDayAMPM (elem match ["오전", "아침"]) td
      _ -> Nothing
  }

ruleYear2 :: Rule
ruleYear2 = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isIntegerBetween 1 2100
    , regex "년"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt $ year v
      _ -> Nothing
  }

ruleHhmmss :: Rule
ruleHhmmss = Rule
  { name = "hh:mm:ss"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)[:.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        s <- parseInt ss
        tt $ hourMinuteSecond True h m s
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
  [ ruleAboutTimeofday
  , ruleAbsorptionOfAfterNamedDay
  , ruleAfterDuration
  , ruleAfterPartofday
  , ruleAfterTimeofday
  , ruleAfternoon
  , ruleAmpmTimeofday
  , ruleByTime
  , ruleDate
  , ruleDatetimeDatetimeInterval
  , ruleDay
  , ruleDayWithKoreanNumeral
  , ruleDayWithKoreanNumeral2
  , ruleDurationAgo
  , ruleDurationFromNow
  , ruleEndOfTime
  , ruleEveningnight
  , ruleExactlyTimeofday
  , ruleHhmm
  , ruleHhmmMilitaryAmpm
  , ruleHhmmss
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleHourofdayHalfAsRelativeMinutes
  , ruleInDuration
  , ruleInduringThePartofday
  , ruleIntegerHourofdayRelativeMinutes
  , ruleHalfHourofdayRelativeMinutes
  , ruleIntersect
  , ruleIntersectBy
  , ruleLastCycle
  , ruleLastNCycle
  , ruleLastTime
  , ruleLunch
  , ruleMidnighteodendOfDay
  , ruleMmdd
  , ruleMmddyyyy
  , ruleMonth
  , ruleMorning
  , ruleNamedday
  , ruleNamedmonth
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNextTime
  , ruleNoon
  , ruleNow
  , ruleQuarter
  , ruleSinceTimeofday
  , ruleTheDayAfterTomorrow
  , ruleTheDayBeforeYesterday
  , ruleThisCycle
  , ruleThisDayofweek
  , ruleThisTime
  , ruleTimeAfterNext
  , ruleTimeCycle
  , ruleTimeCycle2
  , ruleTimeDayofweek
  , ruleTimeNthTime
  , ruleTimeOrdinalCycle
  , ruleTimePartofday
  , ruleTimeofday
  , ruleTimeofday2
  , ruleTimeofday3
  , ruleTimeofday4
  , ruleTimeofdayAmpm
  , ruleTimeofdayApproximately
  , ruleTimeofdayLatent
  , ruleTimeofdayTimeofdayInterval
  , ruleToday
  , ruleTomorrow
  , ruleWeekend
  , ruleWithinDuration
  , ruleYear
  , ruleYear2
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYearQuarter
  , ruleYesterday
  , ruleYyyymmdd
  , ruleSeconds
  , ruleTimezone
  ]
  ++ ruleHolidays
  ++ ruleSeasons
  ++ ruleDaysOfWeek
