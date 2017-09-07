-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ZH.Rules
  ( rules ) where

import Control.Monad (liftM2)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import qualified Duckling.Ordinal.Types as TOrdinal
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types


ruleTheDayAfterTomorrow :: Rule
ruleTheDayAfterTomorrow = Rule
  { name = "the day after tomorrow"
  , pattern =
    [ regex "后天|後天|後日"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleRelativeMinutesTotillbeforeIntegerHourofday :: Rule
ruleRelativeMinutesTotillbeforeIntegerHourofday = Rule
  { name = "relative minutes to|till|before <integer> (hour-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(点|點)差"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleRelativeMinutesTotillbeforeNoonmidnight :: Rule
ruleRelativeMinutesTotillbeforeNoonmidnight = Rule
  { name = "relative minutes to|till|before noon|midnight"
  , pattern =
    [ Predicate isMidnightOrNoon
    , regex "差"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleRelativeMinutesAfterpastIntegerHourofday :: Rule
ruleRelativeMinutesAfterpastIntegerHourofday = Rule
  { name = "relative minutes after|past <integer> (hour-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "点|點"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute True hours n
      _ -> Nothing
  }

ruleRelativeMinutesAfterpastNoonmidnight :: Rule
ruleRelativeMinutesAfterpastNoonmidnight = Rule
  { name = "relative minutes after|past noon|midnight"
  , pattern =
    [ Predicate isMidnightOrNoon
    , regex "过|\x904e"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute True hours n
      _ -> Nothing
  }

ruleQuarterTotillbeforeIntegerHourofday :: Rule
ruleQuarterTotillbeforeIntegerHourofday = Rule
  { name = "quarter to|till|before <integer> (hour-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(点|點)差"
    , regex "一刻"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }
ruleQuarterTotillbeforeNoonmidnight :: Rule
ruleQuarterTotillbeforeNoonmidnight = Rule
  { name = "quarter to|till|before noon|midnight"
  , pattern =
    [ Predicate isMidnightOrNoon
    , regex "差"
    , regex "一刻"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }
ruleQuarterAfterpastIntegerHourofday :: Rule
ruleQuarterAfterpastIntegerHourofday = Rule
  { name = "quarter after|past <integer> (hour-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "点|點"
    , regex "一刻"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 15
      _ -> Nothing
  }
ruleQuarterAfterpastNoonmidnight :: Rule
ruleQuarterAfterpastNoonmidnight = Rule
  { name = "quarter after|past noon|midnight"
  , pattern =
    [ Predicate isMidnightOrNoon
    , regex "过"
    , regex "一刻"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 15
      _ -> Nothing
  }

ruleHalfTotillbeforeIntegerHourofday :: Rule
ruleHalfTotillbeforeIntegerHourofday = Rule
  { name = "half to|till|before <integer> (hour-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(点|點)差"
    , regex "半"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }
ruleHalfTotillbeforeNoonmidnight :: Rule
ruleHalfTotillbeforeNoonmidnight = Rule
  { name = "half to|till|before noon|midnight"
  , pattern =
    [ Predicate isMidnightOrNoon
    , regex "差"
    , regex "半"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }
ruleHalfAfterpastIntegerHourofday :: Rule
ruleHalfAfterpastIntegerHourofday = Rule
  { name = "half after|past <integer> (hour-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "点|點"
    , regex "半"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 30
      _ -> Nothing
  }
ruleHalfAfterpastNoonmidnight :: Rule
ruleHalfAfterpastNoonmidnight = Rule
  { name = "half after|past noon|midnight"
  , pattern =
    [ Predicate isMidnightOrNoon
    , regex "过"
    , regex "半"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 30
      _ -> Nothing
  }

ruleValentinesDay :: Rule
ruleValentinesDay = Rule
  { name = "valentine's day"
  , pattern =
    [ regex "情人(节|節)"
    ]
  , prod = \_ -> tt $ monthDay 2 14
  }

ruleHhmmTimeofday :: Rule
ruleHhmmTimeofday = Rule
  { name = "hh:mm (time-of-day)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3])):([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleThisDayofweek :: Rule
ruleThisDayofweek = Rule
  { name = "this <day-of-week>"
  , pattern =
    [ regex "这|這|今(个|個)?"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNthTimeOfTime2 :: Rule
ruleNthTimeOfTime2 = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Time
    , regex "的"
    , dimension Ordinal
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Ordinal od:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNewYearsDay :: Rule
ruleNewYearsDay = Rule
  { name = "new year's day"
  , pattern =
    [ regex "元旦(节|節)?"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "去|上(个|個)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "再"
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
    [ regex "现在|此时|此刻|当前|現在|此時|當前|\x5b9c\x5bb6|\x800c\x5bb6|\x4f9d\x5bb6"
    ]
  , prod = \_ -> tt $ cycleNth TG.Second 0
  }

ruleNationalDay :: Rule
ruleNationalDay = Rule
  { name = "national day"
  , pattern =
    [ regex "(国庆|國慶)(节|節)?"
    ]
  , prod = \_ -> tt $ monthDay 10 1
  }

ruleTheCycleAfterTime :: Rule
ruleTheCycleAfterTime = Rule
  { name = "the <cycle> after <time>"
  , pattern =
    [ regex "那"
    , dimension TimeGrain
    , regex "(之)?(后|後)"
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
    [ regex "那"
    , dimension TimeGrain
    , regex "(之)?前"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "中午"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleToday :: Rule
ruleToday = Rule
  { name = "today"
  , pattern =
    [ regex "今天|今日"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 0
  }

ruleNextDayofweek :: Rule
ruleNextDayofweek = Rule
  { name = "next <day-of-week>"
  , pattern =
    [ regex "明|下(个|個)?"
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
    [ regex "前天|前日"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
  }

ruleLaborDay :: Rule
ruleLaborDay = Rule
  { name = "labor day"
  , pattern =
    [ regex "劳动节|勞動節"
    ]
  , prod = \_ -> tt $ monthDay 5 1
  }

ruleNextCycle :: Rule
ruleNextCycle = Rule
  { name = "next <cycle>"
  , pattern =
    [ regex "下(个|個)?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleDurationFromNow :: Rule
ruleDurationFromNow = Rule
  { name = "<duration> from now"
  , pattern =
    [ dimension Duration
    , regex "后|後|之後"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleLastCycle :: Rule
ruleLastCycle = Rule
  { name = "last <cycle>"
  , pattern =
    [ regex "上(个|個)?"
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
    [ regex "下午|中午|\x664f\x665d"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "midnight"
  , pattern =
    [ regex "午夜|凌晨|半夜"
    ]
  , prod = \_ -> tt $ hour False 0
  }

ruleInduringThePartofday :: Rule
ruleInduringThePartofday = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "点|點"
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

ruleIntegerLatentTimeofday :: Rule
ruleIntegerLatentTimeofday = Rule
  { name = "<integer> (latent time-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ hour True v
      _ -> Nothing
  }

ruleYearNumericWithYearSymbol :: Rule
ruleYearNumericWithYearSymbol = Rule
  { name = "year (numeric with year symbol)"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 9999
    , regex "年"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt $ year v
      _ -> Nothing
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ dimension Duration
    , regex "(之)?前"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleHhmmMilitaryTimeofday :: Rule
ruleHhmmMilitaryTimeofday = Rule
  { name = "hhmm (military time-of-day)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . mkLatent $ hourMinute False h m
      _ -> Nothing
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "上|前"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleNCycleLast :: Rule
ruleNCycleLast = Rule
  { name = "n <cycle> last"
  , pattern =
    [ Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    , regex "(之)?前"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
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

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Time
    , dimension Ordinal
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Ordinal od:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "周末|週末"
    ]
  , prod = \_ -> tt weekend
  }

ruleLastYear :: Rule
ruleLastYear = Rule
  { name = "last year"
  , pattern =
    [ regex "去年|上年"
    ]
  , prod = \_ -> tt . cycleNth TG.Year $ - 1
  }

ruleDimTimePartofday :: Rule
ruleDimTimePartofday = Rule
  { name = "<dim time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "明|下(个|個)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 1 False td
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
    [ regex "下|后|後"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleNCycleNext :: Rule
ruleNCycleNext = Rule
  { name = "next n <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    , regex "下|(之)?后|(之)?後"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "早上|早晨|\x671d(\x982d)?早"
    ]
  , prod = \_ ->
      let from = hour False 4
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleNextYear :: Rule
ruleNextYear = Rule
  { name = "next year"
  , pattern =
    [ regex "明年|下年"
    ]
  , prod = \_ -> tt $ cycleNth TG.Year 1
  }

ruleThisCycle :: Rule
ruleThisCycle = Rule
  { name = "this <cycle>"
  , pattern =
    [ regex "(这|這)(一)?|今個"
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
    [ regex "今(个|個)?|这(个)?|這(個)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleYesterday :: Rule
ruleYesterday = Rule
  { name = "yesterday"
  , pattern =
    [ regex "昨天|昨日|\x5c0b日"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleChristmas :: Rule
ruleChristmas = Rule
  { name = "christmas"
  , pattern =
    [ regex "(圣诞|聖誕)(节|節)?"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleLastNight :: Rule
ruleLastNight = Rule
  { name = "last night"
  , pattern =
    [ regex "昨晚|昨天晚上|\x5c0b晚"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day $ - 1
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect td1 td2
  }

ruleTimeofdayAmpm :: Rule
ruleTimeofdayAmpm = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "([ap])(\\s|\\.)?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
        tt . timeOfDayAMPM td $ Text.toLower ap == "a"
      _ -> Nothing
  }

ruleArmysDay :: Rule
ruleArmysDay = Rule
  { name = "army's day"
  , pattern =
    [ regex "建(军节|軍節)"
    ]
  , prod = \_ -> tt $ monthDay 8 1
  }

ruleNamedmonthDayofmonth :: Rule
ruleNamedmonthDayofmonth = Rule
  { name = "<named-month> <day-of-month>"
  , pattern =
    [ Predicate isAMonth
    , dimension Numeral
    , regex "号|號|日"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleLastTuesdayLastJuly :: Rule
ruleLastTuesdayLastJuly = Rule
  { name = "last tuesday, last july"
  , pattern =
    [ regex "上"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

rulePartofdayDimTime :: Rule
rulePartofdayDimTime = Rule
  { name = "<part-of-day> <dim time>"
  , pattern =
    [ Predicate isAPartOfDay
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleMonthNumericWithMonthSymbol :: Rule
ruleMonthNumericWithMonthSymbol = Rule
  { name = "month (numeric with month symbol)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 12
    , regex "月"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ month v
      _ -> Nothing
  }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern =
    [ regex "今晚|今天晚上"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day 0
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect td1 td2
  }

ruleTomorrowNight :: Rule
ruleTomorrowNight = Rule
  { name = "tomorrow night"
  , pattern =
    [ regex "明晚|明天晚上|\x807d晚"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day 1
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect td1 td2
  }

ruleChildrensDay :: Rule
ruleChildrensDay = Rule
  { name = "children's day"
  , pattern =
    [ regex "(儿|兒)童(节|節)"
    ]
  , prod = \_ -> tt $ monthDay 6 1
  }

ruleThisYear :: Rule
ruleThisYear = Rule
  { name = "this year"
  , pattern =
    [ regex "今年"
    ]
  , prod = \_ -> tt $ cycleNth TG.Year 0
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

ruleWomensDay :: Rule
ruleWomensDay = Rule
  { name = "women's day"
  , pattern =
    [ regex "(妇|婦)女(节|節)"
    ]
  , prod = \_ -> tt $ monthDay 3 8
  }

ruleEveningnight :: Rule
ruleEveningnight = Rule
  { name = "evening|night"
  , pattern =
    [ regex "晚上|晚间"
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
    [ regex "(0?[1-9]|1[0-2])/(3[01]|[12]\\d|0?[1-9])/(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:yy:_)):_) -> do
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
    [ regex "明天|明日|\x807d日"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleTimeofdayOclock :: Rule
ruleTimeofdayOclock = Rule
  { name = "<time-of-day> o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "點|点|時"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ liftM2 (&&) isATimeOfDay isNotLatent
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone tz td
      _ -> Nothing
  }


daysOfWeek :: [(Text, String)]
daysOfWeek =
  [ ( "Monday", "星期一|周一|礼拜一|禮拜一|週一" )
  , ( "Tuesday", "星期二|周二|礼拜二|禮拜二|週二" )
  , ( "Wednesday", "星期三|周三|礼拜三|禮拜三|週三" )
  , ( "Thursday", "星期四|周四|礼拜四|禮拜四|週四" )
  , ( "Friday", "星期五|周五|礼拜五|禮拜五|週五" )
  , ( "Saturday", "星期六|周六|礼拜六|禮拜六|週六" )
  , ( "Sunday", "星期日|星期天|礼拜天|周日|禮拜天|週日|禮拜日" )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = zipWith go daysOfWeek [1..7]
  where
    go (name, regexPattern) i = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ dayOfWeek i
      }

months :: [(Text, String)]
months =
  [ ( "January", "一月(份)?" )
  , ( "February", "二月(份)?" )
  , ( "March", "三月(份)?" )
  , ( "April", "四月(份)?" )
  , ( "May", "五月(份)?" )
  , ( "June", "六月(份)?" )
  , ( "July", "七月(份)?" )
  , ( "August", "八月(份)?" )
  , ( "September", "九月(份)?" )
  , ( "October", "十月(份)?" )
  , ( "November", "十一月(份)?" )
  , ( "December", "十二月(份)?" )
  ]

ruleMonths :: [Rule]
ruleMonths = zipWith go months [1..12]
  where
    go (name, regexPattern) i = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ month i
      }

rules :: [Rule]
rules =
  [ ruleAbsorptionOfAfterNamedDay
  , ruleAfternoon
  , ruleArmysDay
  , ruleChildrensDay
  , ruleChristmas
  , ruleDimTimePartofday
  , ruleDurationAgo
  , ruleDurationFromNow
  , ruleEveningnight
  , ruleHhmmMilitaryTimeofday
  , ruleHhmmTimeofday
  , ruleInDuration
  , ruleInduringThePartofday
  , ruleIntegerLatentTimeofday
  , ruleIntersect
  , ruleIntersectBy
  , ruleLaborDay
  , ruleLastCycle
  , ruleLastNCycle
  , ruleNCycleLast
  , ruleLastNight
  , ruleLastTime
  , ruleLastTuesdayLastJuly
  , ruleLastYear
  , ruleMidnight
  , ruleMmdd
  , ruleMmddyyyy
  , ruleMonthNumericWithMonthSymbol
  , ruleMorning
  , ruleNamedmonthDayofmonth
  , ruleNationalDay
  , ruleNewYearsDay
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNCycleNext
  , ruleNextTime
  , ruleNextYear
  , ruleNoon
  , ruleNow
  , ruleNthTimeOfTime
  , ruleNthTimeOfTime2
  , rulePartofdayDimTime
  , ruleRelativeMinutesAfterpastIntegerHourofday
  , ruleRelativeMinutesAfterpastNoonmidnight
  , ruleRelativeMinutesTotillbeforeIntegerHourofday
  , ruleRelativeMinutesTotillbeforeNoonmidnight
  , ruleQuarterAfterpastIntegerHourofday
  , ruleQuarterAfterpastNoonmidnight
  , ruleQuarterTotillbeforeIntegerHourofday
  , ruleQuarterTotillbeforeNoonmidnight
  , ruleHalfAfterpastIntegerHourofday
  , ruleHalfAfterpastNoonmidnight
  , ruleHalfTotillbeforeIntegerHourofday
  , ruleHalfTotillbeforeNoonmidnight
  , ruleTheCycleAfterTime
  , ruleTheCycleBeforeTime
  , ruleTheDayAfterTomorrow
  , ruleTheDayBeforeYesterday
  , ruleThisCycle
  , ruleThisDayofweek
  , ruleThisTime
  , ruleThisYear
  , ruleNextDayofweek
  , ruleTimeofdayAmpm
  , ruleTimeofdayOclock
  , ruleToday
  , ruleTomorrow
  , ruleTomorrowNight
  , ruleTonight
  , ruleValentinesDay
  , ruleWeekend
  , ruleWomensDay
  , ruleYearNumericWithYearSymbol
  , ruleYesterday
  , ruleYyyymmdd
  , ruleTimezone
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
