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

ruleNamedday :: Rule
ruleNamedday = Rule
  { name = "named-day"
  , pattern =
    [ regex "\x661f\x671f\x4e00|\x5468\x4e00|\x793c\x62dc\x4e00|\x79ae\x62dc\x4e00|\x9031\x4e00"
    ]
  , prod = \_ -> tt $ dayOfWeek 1
  }

ruleTheDayAfterTomorrow :: Rule
ruleTheDayAfterTomorrow = Rule
  { name = "the day after tomorrow"
  , pattern =
    [ regex "\x540e\x5929|\x5f8c\x5929|\x5f8c\x65e5"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleNamedmonth12 :: Rule
ruleNamedmonth12 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x5341\x4e8c\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 12
  }

ruleRelativeMinutesTotillbeforeIntegerHourofday :: Rule
ruleRelativeMinutesTotillbeforeIntegerHourofday = Rule
  { name = "relative minutes to|till|before <integer> (hour-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(\x70b9|\x9ede)\x5dee"
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
    , regex "\x5dee"
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
    , regex "\x70b9|\x9ede"
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
    , regex "\x8fc7|\x904e"
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
    , regex "(\x70b9|\x9ede)\x5dee"
    , regex "\x4e00\x523b"
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
    , regex "\x5dee"
    , regex "\x4e00\x523b"
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
    , regex "\x70b9|\x9ede"
    , regex "\x4e00\x523b"
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
    , regex "\x8fc7"
    , regex "\x4e00\x523b"
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
    , regex "(\x70b9|\x9ede)\x5dee"
    , regex "\x534a"
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
    , regex "\x5dee"
    , regex "\x534a"
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
    , regex "\x70b9|\x9ede"
    , regex "\x534a"
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
    , regex "\x8fc7"
    , regex "\x534a"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) -> tt $ hourMinute True hours 30
      _ -> Nothing
  }

ruleNamedday2 :: Rule
ruleNamedday2 = Rule
  { name = "named-day"
  , pattern =
    [ regex "\x661f\x671f\x4e8c|\x5468\x4e8c|\x793c\x62dc\x4e8c|\x79ae\x62dc\x4e8c|\x9031\x4e8c"
    ]
  , prod = \_ -> tt $ dayOfWeek 2
  }

ruleValentinesDay :: Rule
ruleValentinesDay = Rule
  { name = "valentine's day"
  , pattern =
    [ regex "\x60c5\x4eba(\x8282|\x7bc0)"
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
    [ regex "\x8fd9|\x9019|\x4eca(\x4e2a|\x500b)"
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
    , regex "\x7684"
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
    [ regex "\x5143\x65e6(\x8282|\x7bc0)?"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "\x53bb|\x4e0a(\x4e2a|\x500b)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleNamedday6 :: Rule
ruleNamedday6 = Rule
  { name = "named-day"
  , pattern =
    [ regex "\x661f\x671f\x516d|\x5468\x516d|\x793c\x62dc\x516d|\x79ae\x62dc\x516d|\x9031\x516d"
    ]
  , prod = \_ -> tt $ dayOfWeek 6
  }

ruleNamedmonth7 :: Rule
ruleNamedmonth7 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x4e03\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 7
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "\x518d"
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
    [ regex "\x73b0\x5728|\x6b64\x65f6|\x6b64\x523b|\x5f53\x524d|\x73fe\x5728|\x6b64\x6642|\x7576\x524d|\x5b9c\x5bb6|\x800c\x5bb6|\x4f9d\x5bb6"
    ]
  , prod = \_ -> tt $ cycleNth TG.Second 0
  }

ruleNationalDay :: Rule
ruleNationalDay = Rule
  { name = "national day"
  , pattern =
    [ regex "(\x56fd\x5e86|\x570b\x6176)(\x8282|\x7bc0)?"
    ]
  , prod = \_ -> tt $ monthDay 10 1
  }

ruleNamedday4 :: Rule
ruleNamedday4 = Rule
  { name = "named-day"
  , pattern =
    [ regex "\x661f\x671f\x56db|\x5468\x56db|\x793c\x62dc\x56db|\x79ae\x62dc\x56db|\x9031\x56db"
    ]
  , prod = \_ -> tt $ dayOfWeek 4
  }

ruleTheCycleAfterTime :: Rule
ruleTheCycleAfterTime = Rule
  { name = "the <cycle> after <time>"
  , pattern =
    [ regex "\x90a3"
    , dimension TimeGrain
    , regex "(\x4e4b)?(\x540e|\x5f8c)"
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
    [ regex "\x90a3"
    , dimension TimeGrain
    , regex "(\x4e4b)?\x524d"
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
    [ regex "\x4e2d\x5348"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleToday :: Rule
ruleToday = Rule
  { name = "today"
  , pattern =
    [ regex "\x4eca\x5929|\x4eca\x65e5"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 0
  }

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "\x4eca(\x4e2a|\x500b)?|\x660e|\x4e0b(\x4e2a|\x500b)?"
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
    [ regex "\x524d\x5929|\x524d\x65e5"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
  }

ruleLaborDay :: Rule
ruleLaborDay = Rule
  { name = "labor day"
  , pattern =
    [ regex "\x52b3\x52a8\x8282|\x52de\x52d5\x7bc0"
    ]
  , prod = \_ -> tt $ monthDay 5 1
  }

ruleNextCycle :: Rule
ruleNextCycle = Rule
  { name = "next <cycle>"
  , pattern =
    [ regex "\x4e0b(\x4e2a|\x500b)?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleNamedmonth :: Rule
ruleNamedmonth = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x4e00\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 1
  }

ruleNamedmonth3 :: Rule
ruleNamedmonth3 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x4e09\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 3
  }

ruleDurationFromNow :: Rule
ruleDurationFromNow = Rule
  { name = "<duration> from now"
  , pattern =
    [ dimension Duration
    , regex "\x540e|\x5f8c|\x4e4b\x5f8c"
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
    [ regex "\x4e0a(\x4e2a|\x500b)?"
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
    [ regex "\x4e0b\x5348|\x4e2d\x5348|\x664f\x665d"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleNamedmonth4 :: Rule
ruleNamedmonth4 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x56db\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 4
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "midnight"
  , pattern =
    [ regex "\x5348\x591c|\x51cc\x6668|\x534a\x591c"
    ]
  , prod = \_ -> tt $ hour False 0
  }

ruleInduringThePartofday :: Rule
ruleInduringThePartofday = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "\x70b9|\x9ede"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleNamedday5 :: Rule
ruleNamedday5 = Rule
  { name = "named-day"
  , pattern =
    [ regex "\x661f\x671f\x4e94|\x5468\x4e94|\x793c\x62dc\x4e94|\x79ae\x62dc\x4e94|\x9031\x4e94"
    ]
  , prod = \_ -> tt $ dayOfWeek 5
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

ruleNamedmonth2 :: Rule
ruleNamedmonth2 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x4e8c\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 2
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
    , regex "\x5e74"
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
    , regex "(\x4e4b)?\x524d"
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
    [ regex "\x4e0a|\x524d"
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
    , regex "(\x4e4b)?\x524d"
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

ruleNamedmonth6 :: Rule
ruleNamedmonth6 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x516d\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 6
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

ruleNamedmonth8 :: Rule
ruleNamedmonth8 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x516b\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 8
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "\x5468\x672b|\x9031\x672b"
    ]
  , prod = \_ -> do
      from <- intersect (dayOfWeek 5) (hour False 18)
      to <- intersect (dayOfWeek 1) (hour False 0)
      Token Time <$> interval TTime.Open from to
  }

ruleLastYear :: Rule
ruleLastYear = Rule
  { name = "last year"
  , pattern =
    [ regex "\x53bb\x5e74|\x4e0a\x5e74"
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
    [ regex "\x660e|\x4e0b(\x4e2a|\x500b)?"
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
    [ regex "\x4e0b|\x540e|\x5f8c"
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
    , regex "\x4e0b|(\x4e4b)?\x540e|(\x4e4b)?\x5f8c"
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
    [ regex "\x65e9\x4e0a|\x65e9\x6668|\x671d(\x982d)?\x65e9"
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
    [ regex "\x660e\x5e74|\x4e0b\x5e74"
    ]
  , prod = \_ -> tt $ cycleNth TG.Year 1
  }

ruleThisCycle :: Rule
ruleThisCycle = Rule
  { name = "this <cycle>"
  , pattern =
    [ regex "(\x8fd9|\x9019)(\x4e00)?|\x4eca\x500b"
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
    [ regex "\x4eca(\x4e2a|\x500b)?|\x8fd9(\x4e2a)?|\x9019(\x500b)?"
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
    [ regex "\x6628\x5929|\x6628\x65e5|\x5c0b\x65e5"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleChristmas :: Rule
ruleChristmas = Rule
  { name = "christmas"
  , pattern =
    [ regex "(\x5723\x8bde|\x8056\x8a95)(\x8282|\x7bc0)?"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleLastNight :: Rule
ruleLastNight = Rule
  { name = "last night"
  , pattern =
    [ regex "\x6628\x665a|\x6628\x5929\x665a\x4e0a|\x5c0b\x665a"
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
    [ regex "\x5efa(\x519b\x8282|\x8ecd\x7bc0)"
    ]
  , prod = \_ -> tt $ monthDay 8 1
  }

ruleNamedmonthDayofmonth :: Rule
ruleNamedmonthDayofmonth = Rule
  { name = "<named-month> <day-of-month>"
  , pattern =
    [ Predicate isAMonth
    , dimension Numeral
    , regex "\x53f7|\x865f|\x65e5"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleLastTuesdayLastJuly :: Rule
ruleLastTuesdayLastJuly = Rule
  { name = "last tuesday, last july"
  , pattern =
    [ regex "\x4e0a"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleNamedmonth5 :: Rule
ruleNamedmonth5 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x4e94\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 5
  }

ruleNamedday7 :: Rule
ruleNamedday7 = Rule
  { name = "named-day"
  , pattern =
    [ regex "\x661f\x671f\x65e5|\x661f\x671f\x5929|\x793c\x62dc\x5929|\x5468\x65e5|\x79ae\x62dc\x5929|\x9031\x65e5|\x79ae\x62dc\x65e5"
    ]
  , prod = \_ -> tt $ dayOfWeek 7
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
    , regex "\x6708"
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
    [ regex "\x4eca\x665a|\x4eca\x5929\x665a\x4e0a"
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
    [ regex "\x660e\x665a|\x660e\x5929\x665a\x4e0a|\x807d\x665a"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day 1
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect td1 td2
  }

ruleNamedmonth10 :: Rule
ruleNamedmonth10 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x5341\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 10
  }

ruleChildrensDay :: Rule
ruleChildrensDay = Rule
  { name = "children's day"
  , pattern =
    [ regex "(\x513f|\x5152)\x7ae5(\x8282|\x7bc0)"
    ]
  , prod = \_ -> tt $ monthDay 6 1
  }

ruleThisYear :: Rule
ruleThisYear = Rule
  { name = "this year"
  , pattern =
    [ regex "\x4eca\x5e74"
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

ruleNamedmonth11 :: Rule
ruleNamedmonth11 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x5341\x4e00\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 11
  }

ruleWomensDay :: Rule
ruleWomensDay = Rule
  { name = "women's day"
  , pattern =
    [ regex "(\x5987|\x5a66)\x5973(\x8282|\x7bc0)"
    ]
  , prod = \_ -> tt $ monthDay 3 8
  }

ruleEveningnight :: Rule
ruleEveningnight = Rule
  { name = "evening|night"
  , pattern =
    [ regex "\x665a\x4e0a|\x665a\x95f4"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . partOfDay . mkLatent <$>
           interval TTime.Open from to
  }

ruleNamedday3 :: Rule
ruleNamedday3 = Rule
  { name = "named-day"
  , pattern =
    [ regex "\x661f\x671f\x4e09|\x5468\x4e09|\x793c\x62dc\x4e09|\x79ae\x62dc\x4e09|\x9031\x4e09"
    ]
  , prod = \_ -> tt $ dayOfWeek 3
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
    [ regex "\x660e\x5929|\x660e\x65e5|\x807d\x65e5"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleTimeofdayOclock :: Rule
ruleTimeofdayOclock = Rule
  { name = "<time-of-day> o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "\x9ede|\x70b9|\x6642"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleNamedmonth9 :: Rule
ruleNamedmonth9 = Rule
  { name = "named-month"
  , pattern =
    [ regex "\x4e5d\x6708(\x4efd)?"
    ]
  , prod = \_ -> tt $ month 9
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
  , ruleNamedday
  , ruleNamedday2
  , ruleNamedday3
  , ruleNamedday4
  , ruleNamedday5
  , ruleNamedday6
  , ruleNamedday7
  , ruleNamedmonth
  , ruleNamedmonth10
  , ruleNamedmonth11
  , ruleNamedmonth12
  , ruleNamedmonth2
  , ruleNamedmonth3
  , ruleNamedmonth4
  , ruleNamedmonth5
  , ruleNamedmonth6
  , ruleNamedmonth7
  , ruleNamedmonth8
  , ruleNamedmonth9
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
  , ruleThisnextDayofweek
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
