-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.VI.Rules
  ( rules ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Ordinal.Types (OrdinalData(..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleTtDng :: Rule
ruleTtDng = Rule
  { name = "tết dương"
  , pattern =
    [ regex "(t(ế)t d(ư)(ơ)ng)(l(ị)ch)?"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "th(ứ) (2|hai)"                  )
  , ( "Tuesday"  , "th(ứ) (3|ba)"                   )
  , ( "Wednesday", "th(ứ) 4|th(ứ) b(ố)n|th(ứ) t(ư)" )
  , ( "Thursday" , "th(ứ) (5|n(ă)m)"                )
  , ( "Friday"   , "th(ứ) 6|th(ứ) s(á)u"            )
  , ( "Saturday" , "th(ứ) (7|b((ả)|(ẩ))y)"          )
  , ( "Sunday"   , "ch(ủ) nh(ậ)t"                   )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"  , "th(á)ng (gi(ê)ng|m(ộ)t)"    )
  , ( "February" , "th(á)ng hai"                )
  , ( "March"    , "th(á)ng ba"                 )
  , ( "April"    , "th(á)ng t(ư)|th(á)ng b(ố)n" )
  , ( "May"      , "th(á)ng n(ă)m"              )
  , ( "June"     , "th(á)ng s(á)u"              )
  , ( "July"     , "th(á)ng b(ả)y"              )
  , ( "August"   , "th(á)ng t(á)m"              )
  , ( "September", "th(á)ng ch(í)n"             )
  , ( "October"  , "th(á)ng m(ư)(ờ)i"           )
  , ( "November" , "th(á)ng m(ư)(ờ)i m(ộ)t"     )
  , ( "December" , "th(á)ng m(ư)(ờ)i hai"       )
  ]

ruleDayofmonthNamedmonth :: Rule
ruleDayofmonthNamedmonth = Rule
  { name = "<day-of-month> (numeric with day symbol) <named-month>"
  , pattern =
    [ regex "(ng(à)y|m(ồ)ng)( m(ồ)ng)?"
    , Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDayofweekCuiCngCaTime :: Rule
ruleDayofweekCuiCngCaTime = Rule
  { name = "<day-of-week> cuối cùng của <time>"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "cu(ố)i c(ù)ng|cu(ố)i"
    , regex "c(ủ)a|trong|v(à)o"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:_:Token Time td2:_) -> tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleTimeTi :: Rule
ruleTimeTi = Rule
  { name = "<time> tới"
  , pattern =
    [ Predicate isNotLatent
    , regex "t(ớ)i"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleCycleCuiCngCaTime :: Rule
ruleCycleCuiCngCaTime = Rule
  { name = "<cycle> cuối cùng của <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "cu(ố)i c(ù)ng|cu(ố)i"
    , regex "c(ủ)a|trong|v(à)o"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleTimeofdayLatent2 :: Rule
ruleTimeofdayLatent2 = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ regex "(l(ú)c|v(à)o)( l(ú)c)?"
    , Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ hour False v
      _ -> Nothing
  }

ruleCchMngThng :: Rule
ruleCchMngThng = Rule
  { name = "cách mạng tháng 8"
  , pattern =
    [ regex "c(á)ch m(ạ)ng th(á)ng (8|t(á)m)"
    ]
  , prod = \_ -> tt $ monthDay 8 19
  }

ruleTimeofdayGiNg :: Rule
ruleTimeofdayGiNg = Rule
  { name = "<time-of-day> giờ đúng"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "gi(ờ) (đ)(ú)ng"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePartofdayHmNay :: Rule
rulePartofdayHmNay = Rule
  { name = "<part-of-day> (hôm )?nay"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "(h(ô)m )?nay"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time . partOfDay <$> intersect today td
      _ -> Nothing
  }

ruleNgyDdmmyyyy :: Rule
ruleNgyDdmmyyyy = Rule
  { name = "ngày dd/mm/yyyy"
  , pattern =
    [ regex "ng(à)y"
    , regex "(3[01]|[12]\\d|0?[1-9])[-/](1[0-2]|0?[1-9])[/-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (_:Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m3
        m <- parseInt m2
        d <- parseInt m1
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

rulePartofdayTime :: Rule
rulePartofdayTime = Rule
  { name = "<part-of-day> <time>"
  , pattern =
    [ Predicate isAPartOfDay
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleQuarterNumber :: Rule
ruleQuarterNumber = Rule
  { name = "quarter <number>"
  , pattern =
    [ Predicate $ isGrain TG.Quarter
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True TG.Quarter (n - 1) $
          cycleNth TG.Year 0
      _ -> Nothing
  }

ruleSeason4 :: Rule
ruleSeason4 = Rule
  { name = "season"
  , pattern =
    [ regex "m(ù)a? xu(â)n"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 3 20) (monthDay 6 21)
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "(bu(ổ)i )?(t(ố)i|(đ)(ê)m)"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleQucTLaoNg :: Rule
ruleQucTLaoNg = Rule
  { name = "quốc tế lao động"
  , pattern =
    [ regex "(ng(à)y )?qu(ố)c t(ế) lao (đ)(ô)ng"
    ]
  , prod = \_ -> tt $ monthDay 5 1
  }

ruleNextCycle :: Rule
ruleNextCycle = Rule
  { name = "next <cycle>"
  , pattern =
    [ dimension TimeGrain
    , regex "(t(ớ)i|k(ế)|sau|ti(ế)p)( theo)?( ti(ế)p)?"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleTimeofdayApproximately :: Rule
ruleTimeofdayApproximately = Rule
  { name = "<time-of-day> approximately"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "g(ầ)n"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleQuarterNumberOfYear :: Rule
ruleQuarterNumberOfYear = Rule
  { name = "quarter <number> of <year>"
  , pattern =
    [ Predicate $ isGrain TG.Quarter
    , dimension Numeral
    , regex "c(ủ)a|trong"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter False TG.Quarter (n - 1) td
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "(bu(ổ)i )?tr(ư)a"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 12) (hour False 14)
  }

ruleLastCycle :: Rule
ruleLastCycle = Rule
  { name = "last <cycle>"
  , pattern =
    [ dimension TimeGrain
    , regex "tr(ư)(ớ)c|qua|v(ừ)a r(ồ)i|ngo(á)i"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd/mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])/(0?[1-9]|1[0-2])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
        tt $ monthDay m d
      _ -> Nothing
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "(bu(ổ)i )?chi(ề)u"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 12) (hour False 19)
  }

ruleHmNay :: Rule
ruleHmNay = Rule
  { name = "hôm nay"
  , pattern =
    [ regex "((ngay )?h(ô)m|b(ữ)a) nay"
    ]
  , prod = \_ -> tt today
  }

ruleAtHhmm :: Rule
ruleAtHhmm = Rule
  { name = "at hh:mm"
  , pattern =
    [ regex "(l(ú)c|v(à)o)( l(ú)c)?"
    , regex "((?:[01]?\\d)|(?:2[0-3]))[:.hg]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (_:Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleHourofdayInteger :: Rule
ruleHourofdayInteger = Rule
  { name = "<hour-of-day> <integer>"
  , pattern =
    [ Predicate isAnHourOfDay
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

ruleHourofdayQuarter :: Rule
ruleHourofdayQuarter = Rule
  { name = "(hour-of-day) quarter"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "k(é)m"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }

ruleHourofdayHalf :: Rule
ruleHourofdayHalf = Rule
  { name = "(hour-of-day) half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "r(ư)(ỡ)i"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHourofdayIntegerPht :: Rule
ruleHourofdayIntegerPht = Rule
  { name = "<hour-of-day> <integer> phút"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    , regex "ph(ú)t"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleCuiNm :: Rule
ruleCuiNm = Rule
  { name = "cuối năm"
  , pattern =
    [ regex "cu(ố)i n(ă)m"
    ]
  , prod = \_ -> tt $ cycleNth TG.Year 1
  }

ruleQuarterNumberYear :: Rule
ruleQuarterNumberYear = Rule
  { name = "quarter <number> <year>"
  , pattern =
    [ Predicate $ isGrain TG.Quarter
    , dimension Numeral
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token Time td:_) -> do
        v <- getIntValue token
        tt $ cycleNthAfter False TG.Quarter (v - 1) td
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
        tt . mkLatent $ hour True n
      _ -> Nothing
  }

ruleExactlyTimeofday :: Rule
ruleExactlyTimeofday = Rule
  { name = "exactly <time-of-day>"
  , pattern =
    [ regex "(v(à)o )?(đ)(ú)ng"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleNgyHmQua :: Rule
ruleNgyHmQua = Rule
  { name = "ngày hôm qua"
  , pattern =
    [ regex "(ng(à)y )?(h(ô)m )?qua"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleSeason3 :: Rule
ruleSeason3 = Rule
  { name = "season"
  , pattern =
    [ regex "m(ù)a? (đ)(ô)ng"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 12 21) (monthDay 3 20)
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "season"
  , pattern =
    [ regex "m(ù)a? (h(è)|h(ạ))"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 6 21) (monthDay 9 23)
  }

ruleDayofmonthNamedmonth2 :: Rule
ruleDayofmonthNamedmonth2 = Rule
  { name = "<day-of-month> <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleYearNumericWithYearSymbol :: Rule
ruleYearNumericWithYearSymbol = Rule
  { name = "year (numeric with year symbol)"
  , pattern =
    [ regex "n(ă)m"
    , Predicate $ isIntegerBetween 1000 9999
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt $ year n
      _ -> Nothing
  }

ruleAfterWork :: Rule
ruleAfterWork = Rule
  { name = "after work"
  , pattern =
    [ regex "(sau gi(ờ) l(à)m|sau gi(ờ) tan t(ầ)m|l(ú)c tan t(ầ)m)"
    ]
  , prod = \_ -> do
      td <- interval TTime.Open (hour False 17) (hour False 21)
      Token Time . partOfDay . notLatent <$> intersect today td
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    , regex "tr(ư)(ớ)c|qua|v(ừ)a r(ồ)i|ngo(á)i|v(ừ)a qua"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleN True grain $ - n
      _ -> Nothing
  }

ruleTimeofdaySharp :: Rule
ruleTimeofdaySharp = Rule
  { name = "<time-of-day> sharp"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(đ)(ú)ng"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
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
      (Token Time td1:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleAboutTimeofday :: Rule
ruleAboutTimeofday = Rule
  { name = "about <time-of-day>"
  , pattern =
    [ regex "(v(à)o )?kho(ả)ng"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleLTnhNhn :: Rule
ruleLTnhNhn = Rule
  { name = "lễ tình nhân"
  , pattern =
    [ regex "(ng(à)y )?(l(ễ))? t(ì)nh nh(â)n"
    ]
  , prod = \_ -> tt $ monthDay 2 14
  }

ruleCuiThng :: Rule
ruleCuiThng = Rule
  { name = "cuối tháng"
  , pattern =
    [ regex "cu(ố)i th(á)ng"
    ]
  , prod = \_ -> tt $ cycleNth TG.Month 1
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "(cu(ố)i|h(ế)t) tu(ầ)n"
    ]
  , prod = \_ -> tt weekend
  }

ruleTimeofdaySngchiuti :: Rule
ruleTimeofdaySngchiuti = Rule
  { name = "<time-of-day> sáng|chiều|tối"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(s(á)ng|chi(ề)u|t(ố)i)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
        tt $ timeOfDayAMPM (Text.toLower ap == "s\225ng") td
      _ -> Nothing
  }

ruleTimeTrc :: Rule
ruleTimeTrc = Rule
  { name = "<time> trước"
  , pattern =
    [ Predicate $ not . isGrainFinerThan TG.Day
    , regex "tr(ư)(ớ)c|v(ừ)a r(ồ)i"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleTimeofdayGi :: Rule
ruleTimeofdayGi = Rule
  { name = "time-of-day giờ"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "gi(ờ)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleQucKhnh :: Rule
ruleQucKhnh = Rule
  { name = "quốc khánh"
  , pattern =
    [ regex "qu(ố)c kh(á)nh"
    ]
  , prod = \_ -> tt $ monthDay 9 3
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

ruleIntersectByOfFromS :: Rule
ruleIntersectByOfFromS = Rule
  { name = "intersect by \"of\", \"from\", \"'s\""
  , pattern =
    [ Predicate isNotLatent
    , regex "c(ủ)a|trong"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleNextNCycle :: Rule
ruleNextNCycle = Rule
  { name = "next n <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    , regex "(t(ớ)i|k(ế)|sau|ti(ế)p)( theo)?( ti(ế)p)?"
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
    [ regex "(bu(ổ)i )?s(á)ng"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 4) (hour False 12)
  }

ruleThisCycle :: Rule
ruleThisCycle = Rule
  { name = "this <cycle>"
  , pattern =
    [ dimension TimeGrain
    , regex "nay|n(à)y|hi(ệ)n t(ạ)i|h(ô)m nay|n(ă)m nay"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleNoon2 :: Rule
ruleNoon2 = Rule
  { name = "noon"
  , pattern =
    [ regex "(bu(ổ)i )?(t(ố)i|(đ)(ê)m)"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 18) (hour False 0)
  }

ruleAfterLunch :: Rule
ruleAfterLunch = Rule
  { name = "after lunch"
  , pattern =
    [ regex "(sau|qua) (bu(ổ)i |b(ữ)a )?tr(ư)a"
    ]
  , prod = \_ -> do
      td <- interval TTime.Open (hour False 13) (hour False 17)
      Token Time . partOfDay . notLatent <$> intersect today td
  }

ruleSeason2 :: Rule
ruleSeason2 = Rule
  { name = "season"
  , pattern =
    [ regex "m(ù)a? thu"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 9 23) (monthDay 12 21)
  }

ruleTimeNy :: Rule
ruleTimeNy = Rule
  { name = "<time> này"
  , pattern =
    [ dimension Time
    , regex "n(à)y|nay"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ predNth 0 False td
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

ruleNgyMai :: Rule
ruleNgyMai = Rule
  { name = "ngày mai"
  , pattern =
    [ regex "(ng(à)y )?mai"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleOrdinalCycleOfTime :: Rule
ruleOrdinalCycleOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension TimeGrain
    , dimension Ordinal
    , regex "c(ủ)a|trong"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:Token Ordinal od:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleMonthNumericWithMonthSymbol :: Rule
ruleMonthNumericWithMonthSymbol = Rule
  { name = "month (numeric with month symbol)"
  , pattern =
    [ regex "th(á)ng"
    , Predicate $ isIntegerBetween 1 12
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt $ month v
      _ -> Nothing
  }

ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.hg]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern =
    [ regex "(t(ố)i|(đ)(ê)m)( h(ô)m)? nay"
    ]
  , prod = \_ -> do
      evening <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay . notLatent <$> intersect today evening
  }

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isGrainFinerThan TG.Day]
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleByGi :: Rule
ruleByGi = Rule
  { name = "bây giờ"
  , pattern =
    [ regex "(ngay )?(b(â)y gi(ờ)|l(ú)c n(à)y)"
    ]
  , prod = \_ -> tt now
  }

ruleNgyDdmm :: Rule
ruleNgyDdmm = Rule
  { name = "ngày dd/mm"
  , pattern =
    [ regex "ng(à)y"
    , regex "(3[01]|[12]\\d|0?[1-9])/(1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (_:Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
        tt $ monthDay m d
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

ruleHhmmMilitarySngchiuti :: Rule
ruleHhmmMilitarySngchiuti = Rule
  { name = "hhmm (military) sáng|chiều|tối"
  , pattern =
    [ regex "((?:1[012]|0?\\d))([0-5]\\d)"
    , regex "(s(á)ng|chi(ề)u|t(ố)i)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):
       Token RegexMatch (GroupMatch (ap:_)):
       _) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . timeOfDayAMPM (Text.toLower ap == "s\225ng") $ hourMinute True h m
      _ -> Nothing
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[-/](0?[1-9]|1[0-2])[/-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleGingSinh :: Rule
ruleGingSinh = Rule
  { name = "giáng sinh"
  , pattern =
    [ regex "(ngày )?(xmas|christmas|giáng sinh)"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleNgyHmKia :: Rule
ruleNgyHmKia = Rule
  { name = "ngày hôm kia"
  , pattern =
    [ regex "(ngày )?hôm kia"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
  }

ruleDayofweekTi :: Rule
ruleDayofweekTi = Rule
  { name = "<day-of-week> tới"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "t(ớ)i"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 0 True td
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

rules :: [Rule]
rules =
  [ ruleAboutTimeofday
  , ruleAfterLunch
  , ruleAfterWork
  , ruleAfternoon
  , ruleAtHhmm
  , ruleByGi
  , ruleCchMngThng
  , ruleCuiNm
  , ruleCuiThng
  , ruleCycleCuiCngCaTime
  , ruleDayofmonthNamedmonth
  , ruleDayofmonthNamedmonth2
  , ruleDayofweekCuiCngCaTime
  , ruleDayofweekTi
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleExactlyTimeofday
  , ruleGingSinh
  , ruleHhmm
  , ruleHhmmMilitaryAmpm
  , ruleHhmmMilitarySngchiuti
  , ruleHhmmss
  , ruleHmNay
  , ruleHourofdayHalf
  , ruleHourofdayInteger
  , ruleHourofdayIntegerPht
  , ruleHourofdayQuarter
  , ruleIntersect
  , ruleIntersectByOfFromS
  , ruleLTnhNhn
  , ruleLastCycle
  , ruleLastNCycle
  , ruleLunch
  , ruleMonthNumericWithMonthSymbol
  , ruleMorning
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNgyDdmm
  , ruleNgyDdmmyyyy
  , ruleNgyHmKia
  , ruleNgyHmQua
  , ruleNgyMai
  , ruleNoon
  , ruleNoon2
  , ruleOrdinalCycleOfTime
  , rulePartofdayHmNay
  , rulePartofdayTime
  , ruleQuarterNumber
  , ruleQuarterNumberOfYear
  , ruleQuarterNumberYear
  , ruleQucKhnh
  , ruleQucTLaoNg
  , ruleSeason
  , ruleSeason2
  , ruleSeason3
  , ruleSeason4
  , ruleThisCycle
  , ruleTimeNy
  , ruleTimeTi
  , ruleTimeTrc
  , ruleTimeofdayAmpm
  , ruleTimeofdayApproximately
  , ruleTimeofdayGi
  , ruleTimeofdayGiNg
  , ruleTimeofdayLatent
  , ruleTimeofdayLatent2
  , ruleTimeofdaySharp
  , ruleTimeofdaySngchiuti
  , ruleTimezone
  , ruleTonight
  , ruleTtDng
  , ruleWeekend
  , ruleYearNumericWithYearSymbol
  , ruleYyyymmdd
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
