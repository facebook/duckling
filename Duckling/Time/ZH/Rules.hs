-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ZH.Rules
  ( rules
  ) where

import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

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
        Token Time <$> minutesBefore n td
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
        Token Time <$> minutesBefore n td
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
      (_:Token Time td:_) -> tt $ predNth 0 False td
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

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "去|上(个|個)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (-1) False td
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
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "现在|此时|此刻|当前|現在|此時|當前|\x5b9c\x5bb6|\x800c\x5bb6|\x4f9d\x5bb6"
    ]
  , prod = \_ -> tt now
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
  , prod = \_ -> tt today
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
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
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
    , regex "月(份)?"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkOkForThisNext $ month v
      _ -> Nothing
  }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern =
    [ regex "今晚|今天晚上"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect today td2
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
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday", "星期一|周一|礼拜一|禮拜一|週一" )
  , ( "Tuesday", "星期二|周二|礼拜二|禮拜二|週二" )
  , ( "Wednesday", "星期三|周三|礼拜三|禮拜三|週三" )
  , ( "Thursday", "星期四|周四|礼拜四|禮拜四|週四" )
  , ( "Friday", "星期五|周五|礼拜五|禮拜五|週五" )
  , ( "Saturday", "星期六|周六|礼拜六|禮拜六|週六" )
  , ( "Sunday", "星期日|星期天|礼拜天|周日|禮拜天|週日|禮拜日" )
  ]

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "中国共产党的生日", "中(国共产党诞|國共產黨誕)生日|(党|黨)的生日", monthDay 7 1 )
  , ( "愚人节", "愚人(节|節)", monthDay 4 1 )
  , ( "建军节", "(中国人民解放(军|軍)|八一)?建(军节|軍節)", monthDay 8 1 )
  , ( "植树节", "中(国植树节|國植樹節)", monthDay 3 12 )
  , ( "五四青年节", "(中(国|國))?(五四|54)?青年(节|節)", monthDay 5 4 )
  , ( "圣诞节", "(圣诞|聖誕)(节|節)?", monthDay 12 25 )
  , ( "平安夜", "(平安|聖誕)夜", monthDay 12 24 )
  , ( "哥伦布日", "哥(伦|倫)布日", monthDay 10 12 )
  , ( "双十一", "(双|雙)(十一|11)", monthDay 11 11 )
  , ( "万圣节", "万圣节|萬聖節", monthDay 10 31 )
  , ( "香港回归纪念日", "香港回(归纪|歸紀)念日", monthDay 7 1 )
  , ( "人权日", "人(权|權)日", monthDay 12 10 )
  , ( "美国独立日", "(美国)?(独|獨)立日", monthDay 7 4 )
  , ( "儿童节", "(国际|國際)?(六一|61)?(儿|兒)童(节|節)", monthDay 6 1 )
  , ( "国际慈善日", "(国际|國際)慈善日", monthDay 9 5 )
  , ( "国际瑜伽日", "(国际|國際)瑜伽日", monthDay 6 21 )
  , ( "国际爵士日", "(国际|國際)爵士日", monthDay 4 30 )
  , ( "国际奥林匹克日", "(国际|國際)奥林匹克日", monthDay 6 23 )
  , ( "妇女节", "(国际劳动|國際勞動|三八)?(妇|婦)女(节|節)", monthDay 3 8 )
  , ( "劳动节", "(五一|51)?(国际|國際)?(劳动|勞動)(节|節)", monthDay 5 1 )
  , ( "国际青年节", "(国际|國際)青年(节|節)", monthDay 8 12 )
  , ( "澳门回归纪念日", "澳(门|門)回(归纪|歸紀)念日", monthDay 12 20 )
  , ( "全国爱牙日", "全(国爱|國愛)牙日", monthDay 9 20 )
  , ( "全国爱耳日", "全(国爱|國愛)耳日", monthDay 3 3 )
  , ( "全国爱眼日", "全(国爱|國愛)眼日", monthDay 6 6 )
  , ( "南京大屠杀纪念日", "南京大屠(杀纪|殺紀)念日", monthDay 12 13 )
  , ( "辛亥革命纪念日", "辛亥革命(纪|紀)念日", monthDay 10 10 )
  , ( "元旦", "元旦(节|節)?|((公|(阳|陽))(历|曆))?新年", monthDay 1 1 )
  , ( "新年夜", "新年夜", monthDay 12 31 )
  , ( "情人节", "(情人|(圣瓦伦丁|聖瓦倫丁))(节|節)", monthDay 2 14 )
  , ( "清明节", "清明(节|節)", monthDay 4 5 )
  , ( "光棍节", "光棍(节|節)", monthDay 11 11 )
  , ( "圣帕特里克节", "圣帕特里克节|聖帕特裏克節", monthDay 3 17 )
  , ( "教师节", "(中(国|國))?教师(节|節)", monthDay 9 10 )
  , ( "退伍军人节", "(退伍(军|軍)人|老兵)(节|節)", monthDay 11 11 )
  , ( "白色情人节", "白色情人(节|節)", monthDay 3 14 )
  , ( "世界艾滋病日", "世界艾滋病日", monthDay 12 1 )
  , ( "世界献血日", "世界(献|獻)血日", monthDay 6 14 )
  , ( "世界癌症日", "世界癌(症|癥)日", monthDay 2 4 )
  , ( "国际消费者权益日", "(国际|世界)?(消费者权益|消費者權益)日|三一五", monthDay 3 15 )
  , ( "世界糖尿病日", "世界糖尿病日", monthDay 11 14 )
  , ( "世界环境日", "世界(环|環)境日", monthDay 6 5 )
  , ( "世界粮食日", "世界((粮|糧)食|食物)日", monthDay 10 16 )
  , ( "世界心脏日", "世界心(脏|臟)日", monthDay 9 29 )
  , ( "世界海洋日", "世界海洋日", monthDay 6 8 )
  , ( "世界诗歌日", "世界(诗|詩)歌日", monthDay 3 21 )
  , ( "世界人口日", "世界人口日", monthDay 7 11 )
  , ( "世界难民日", "世界(难|難)民日", monthDay 6 20 )
  , ( "世界教师日", "世界教师日", monthDay 10 5 )
  , ( "世界旅游日", "世界旅游日", monthDay 9 27 )

  -- Fixed day/week/month, year over year
  , ( "父亲节", "父(亲节|親節)", nthDOWOfMonth 3 7 6 )
  , ( "马丁路德金日", "(马|馬)丁路德金((纪|紀)念)?日", nthDOWOfMonth 3 1 1)
  , ( "母亲节", "母(亲节|親節)", nthDOWOfMonth 2 7 5 )
  ]

rules :: [Rule]
rules =
  [ ruleAbsorptionOfAfterNamedDay
  , ruleAfternoon
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
  , ruleLastCycle
  , ruleLastNCycle
  , ruleNCycleLast
  , ruleLastNight
  , ruleLastTime
  , ruleLastYear
  , ruleMidnight
  , ruleMmdd
  , ruleMmddyyyy
  , ruleMonthNumericWithMonthSymbol
  , ruleMorning
  , ruleNamedmonthDayofmonth
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
  , ruleWeekend
  , ruleYearNumericWithYearSymbol
  , ruleYesterday
  , ruleYyyymmdd
  , ruleTimezone
  ]
  ++ ruleDaysOfWeek
  ++ rulePeriodicHolidays
