-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HR.Rules
  ( rules ) where

import Control.Monad (join, liftM2)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
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

ruleNamedday :: Rule
ruleNamedday = Rule
  { name = "named-day"
  , pattern =
    [ regex "ponedjelja?ka?|pon\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 1
  }

ruleNamedmonth12 :: Rule
ruleNamedmonth12 = Rule
  { name = "named-month"
  , pattern =
    [ regex "prosina?c(a|u)?|decemba?r(a|u)?|dec\\.?|pros?\\.?|dvanaest(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 12
  }

ruleHalfIntegerHrStyleHourofday :: Rule
ruleHalfIntegerHrStyleHourofday = Rule
  { name = "half <integer> (HR style hour-of-day)"
  , pattern =
    [ regex "pol?a?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleNumeralTotillbeforeIntegerHourofday :: Rule
ruleNumeralTotillbeforeIntegerHourofday = Rule
  { name = "numeral to|till|before <integer> (hour-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "do"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleQuarterTotillbeforeIntegerHourofday :: Rule
ruleQuarterTotillbeforeIntegerHourofday = Rule
  { name = "quarter to|till|before <integer> (hour-of-day)"
  , pattern =
    [ regex "(kvarata?|(c|\x010d)etvrt|frtalj)\\s+do"
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
    [ regex "di pol?a?\\s+do"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleNumeralAfterpastHourofday :: Rule
ruleNumeralAfterpastHourofday = Rule
  { name = "numeral after|past (hour-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "poslije|nakon"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        t <- minutesAfter n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleQuarterAfterpastHourofday :: Rule
ruleQuarterAfterpastHourofday = Rule
  { name = "quarter after|past (hour-of-day)"
  , pattern =
    [ regex "(kvarata?|(c|\x010d)etvrt|frtalj)\\s+(poslije|nakon)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 15 td
      _ -> Nothing
  }

ruleHalfAfterpastHourofday :: Rule
ruleHalfAfterpastHourofday = Rule
  { name = "half after|past (hour-of-day)"
  , pattern =
    [ regex "(i pol?a?)\\s+(poslije|nakon)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 30 td
      _ -> Nothing
  }

ruleHourofdayNumeral :: Rule
ruleHourofdayNumeral = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
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
  { name = "<hour-of-day> quarter"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "kvarata?|(c|\x010d)etvrt|frtalj"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHourofdayHalf :: Rule
ruleHourofdayHalf = Rule
  { name = "<hour-of-day> half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "i pol?a?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleZaNumeralHourofday :: Rule
ruleZaNumeralHourofday = Rule
  { name = "za <integer> (hour-of-day)"
  , pattern =
    [ regex "za"
    , Predicate $ isIntegerBetween 1 59
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:
       token:
       Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleZaQuarterHourofday :: Rule
ruleZaQuarterHourofday = Rule
  { name = "za quarter (hour-of-day)"
  , pattern =
    [ regex "za\\s+(kvarata?|(c|\x010d)etvrt|frtalj)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleZaHalfHourofday :: Rule
ruleZaHalfHourofday = Rule
  { name = "za half (hour-of-day)"
  , pattern =
    [ regex "za\\s+(i pol?a?)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleNamedday2 :: Rule
ruleNamedday2 = Rule
  { name = "named-day"
  , pattern =
    [ regex "utora?ka?|uto?\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 2
  }

ruleValentinesDay :: Rule
ruleValentinesDay = Rule
  { name = "valentine's day"
  , pattern =
    [ regex "valentinov(og?|a|)"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 2 14
  }

ruleSinceTimeofday :: Rule
ruleSinceTimeofday = Rule
  { name = "since <time-of-day>"
  , pattern =
    [ regex "od"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleNewYearsDay :: Rule
ruleNewYearsDay = Rule
  { name = "new year's day"
  , pattern =
    [ regex "nov(a|u|e) godin(a|e|u)"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 1 1
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "(prethodn(i|u|a|e|o(ga?)?)|pro(s|\x0161)l(ih?|u|a|e|o(ga?)?))"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time $ predNth (-1) False td
      _ -> Nothing
  }

ruleNamedday6 :: Rule
ruleNamedday6 = Rule
  { name = "named-day"
  , pattern =
    [ regex "subot(a|e|u)|sub?\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 6
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNamedmonth7 :: Rule
ruleNamedmonth7 = Rule
  { name = "named-month"
  , pattern =
    [ regex "srpa?nj(a|u)?|jul(i|u|a)?|jul\\.?|srp\\.?|sedm(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 7
  }

ruleCycleAfterTime :: Rule
ruleCycleAfterTime = Rule
  { name = "<cycle> after <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "nakon"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        Just . Token Time $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "za"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> Just . Token Time $ inDuration dd
      _ -> Nothing
  }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "((upravo|ov(aj?|og?|e|i))? ?sada?|trenutak|upravo|trenutno)|(ov(aj|og) trena?)"
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Second 0
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "zadnj(ih?|a|e(ga?)?)"
    , dimension TimeGrain
    , regex "u"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        Just . Token Time $ cycleLastOf grain td
      _ -> Nothing
  }

ruleFromDatetimeDatetimeInterval :: Rule
ruleFromDatetimeDatetimeInterval = Rule
  { name = "from <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "od|izme(dj|\x0111)u"
    , dimension Time
    , regex "\\-"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleMonthDdddInterval :: Rule
ruleMonthDdddInterval = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ Predicate isAMonth
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-|do"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (m1:_)):
       _:
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
        d1 <- parseInt m1
        d2 <- parseInt m2
        from <- intersect (dayOfMonth d1) td
        to <- intersect (dayOfMonth d2) td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleNamedday4 :: Rule
ruleNamedday4 = Rule
  { name = "named-day"
  , pattern =
    [ regex "(\x010d|c)etvrta?ka?|(\x010d|c)et\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 4
  }

ruleSeason4 :: Rule
ruleSeason4 = Rule
  { name = "season"
  , pattern =
    [ regex "prolje(c|\x0107)(e|a)"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 3 20) (monthDay 6 21)
  }

ruleYearLatent2 :: Rule
ruleYearLatent2 = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 2101 10000
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleTimeAfterNext :: Rule
ruleTimeAfterNext = Rule
  { name = "<time> after next"
  , pattern =
    [ dimension Time
    , regex "nakon sljede(\x0107|c)(i|e|a)(ga?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Just . Token Time $ predNth 1 True td
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "(u )?podne(va)?"
    ]
  , prod = \_ -> Just . Token Time $ hour False 12
  }

ruleEarlyMorning :: Rule
ruleEarlyMorning = Rule
  { name = "early morning"
  , pattern =
    [ regex "(rano( (u )?u?jutros?)?)|(u ran(im|e) jutarnj(im|e) sat(ima|e))"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 3) (hour False 9)
  }

ruleToday :: Rule
ruleToday = Rule
  { name = "today"
  , pattern =
    [ regex "danas?|(dana(s|\x0161)nj(i|eg) dana?) "
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Day 0
  }

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "ov(aj?|og?|e)|sljede(c|\x0107)(i|u|a|e(ga?)?)"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Just . Token Time $ predNth 0 True td
      _ -> Nothing
  }

ruleBetweenTimeofdayAndTimeofdayInterval :: Rule
ruleBetweenTimeofdayAndTimeofdayInterval = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "od|izme(dj|\x0111)u"
    , Predicate isATimeOfDay
    , regex "do|i"
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
    [ regex "sljede(c|\x0107)(i|a|u|e(ga?)?)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> Just . Token Time $ cycleNth grain 1
      _ -> Nothing
  }

ruleNamedmonth :: Rule
ruleNamedmonth = Rule
  { name = "named-month"
  , pattern =
    [ regex "sije(c|\x010d)a?nj(a|u)?|januar(a|u)?|jan\\.?|sij?\\.?|prv(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 1
  }

ruleTimeofdayApproximately :: Rule
ruleTimeofdayApproximately = Rule
  { name = "<time-of-day> approximately"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(-?ak)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleNamedmonth3 :: Rule
ruleNamedmonth3 = Rule
  { name = "named-month"
  , pattern =
    [ regex "o(z|\x017e)uja?k(a|u)?|mart(a|u)?|mar\\.?|o(z|\x017e)u?\\.?|tre(c|\x0107)(i|a|e(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 3
  }

ruleForDuration :: Rule
ruleForDuration = Rule
  { name = "for <duration>"
  , pattern =
    [ regex "za( jo(s|\x0161))?|u"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> Just . Token Time $ inDuration dd
      _ -> Nothing
  }

ruleLastDayofweekTime :: Rule
ruleLastDayofweekTime = Rule
  { name = "last <day-of-week> <time>"
  , pattern =
    [ regex "zadnj(ih?|a|e(ga?)?)"
    , Predicate isADayOfWeek
    , regex "u"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Just . Token Time $ predLastOf td1 td2
      _ -> Nothing
  }

ruleDurationFromNow :: Rule
ruleDurationFromNow = Rule
  { name = "<duration> from now"
  , pattern =
    [ dimension Duration
    , regex "od (sada?|ovog trenutka|dana(s|\x0161)nj(i|eg) dana?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> Just . Token Time $ inDuration dd
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "(((za )|(u vrijeme )) )?ru(c|\x010d)a?k(a|om)?"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 12) (hour False 14)
  }

ruleLastCycle :: Rule
ruleLastCycle = Rule
  { name = "last <cycle>"
  , pattern =
    [ regex "prethodn(i|a|e|u)|pro(s|\x0161)l(i|a|e|u|o(ga?)?)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> Just . Token Time . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd.mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\/(0?[1-9]|1[0-2])"
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
    [ regex "po(slije)?podne"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 12) (hour False 20)
  }

ruleNamedmonth4 :: Rule
ruleNamedmonth4 = Rule
  { name = "named-month"
  , pattern =
    [ regex "trava?nj(a|u)?|april(a|u)?|apr\\.?|tra\\.?|(\x010d|c)etvrt(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 4
  }

ruleTimeBeforeLast :: Rule
ruleTimeBeforeLast = Rule
  { name = "<time> before last"
  , pattern =
    [ dimension Time
    , regex "prije (prethodn(e|o(ga?)?)|pro(s|\x0161)l(ih?|u|a|e|o(ga?)?))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        Just . Token Time $ predNth (-2) False td
      _ -> Nothing
  }

ruleLateNight :: Rule
ruleLateNight = Rule
  { name = "late night"
  , pattern =
    [ regex "(((u|po)\\s)?no(c|\x0107)(i|as|u)?|u?jutros?)"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 0) (hour False 4)
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
    [ regex "badnjaka?"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 12 24
  }

ruleInduringThePartofday :: Rule
ruleInduringThePartofday = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "(u|tokom)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Just . Token Time $ notLatent td
      _ -> Nothing
  }

ruleNamedday5 :: Rule
ruleNamedday5 = Rule
  { name = "named-day"
  , pattern =
    [ regex "peta?ka?|pet\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 5
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
      (Token Time td1:_:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "poslije|nakon|iza"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleAfterDuration :: Rule
ruleAfterDuration = Rule
  { name = "after <duration>"
  , pattern =
    [ regex "(nakon|poslije)( jo(s|\x0161))?"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt . withDirection TTime.After $ inDuration dd
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

ruleDayofmonthOrdinalOfNamedmonth :: Rule
ruleDayofmonthOrdinalOfNamedmonth = Rule
  { name = "<day-of-month> (ordinal) of <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , regex "of|in"
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
    [ regex "od"
    , Predicate isATimeOfDay
    , regex "\\-"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNamedmonth2 :: Rule
ruleNamedmonth2 = Rule
  { name = "named-month"
  , pattern =
    [ regex "(ve)?lja(c|\x010d)(a|e|i)|februar(a|u)?|feb\\.?|ve(lj)?\\.?|drug(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 2
  }

ruleExactlyTimeofday :: Rule
ruleExactlyTimeofday = Rule
  { name = "exactly <time-of-day>"
  , pattern =
    [ regex "to(c|\x010d)no( u)?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleSeason3 :: Rule
ruleSeason3 = Rule
  { name = "season"
  , pattern =
    [ regex "zim(a|e|us)?"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 12 21) (monthDay 3 20)
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "season"
  , pattern =
    [ regex "ljet(os?|a)"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 6 21) (monthDay 9 23)
  }

ruleUNamedday :: Rule
ruleUNamedday = Rule
  { name = "u <named-day>"
  , pattern =
    [ regex "u"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleBetweenDatetimeAndDatetimeInterval :: Rule
ruleBetweenDatetimeAndDatetimeInterval = Rule
  { name = "between <datetime> and <datetime> (interval)"
  , pattern =
    [ regex "od|izme(dj|\x0111)u"
    , dimension Time
    , regex "do|i"
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
    [ regex "star(a|u|e) godin(a|e|u)"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 12 31
  }

ruleByTheEndOfTime :: Rule
ruleByTheEndOfTime = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "(do )(kraja|isteka)? "
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Token Time <$> interval TTime.Closed (cycleNth TG.Second 0) td
      _ -> Nothing
  }

ruleAfterWork :: Rule
ruleAfterWork = Rule
  { name = "after work"
  , pattern =
    [ regex "poslije (posla|kraja radnog vremena)"
    ]
  , prod = \_ -> do
      td <- interval TTime.Open (hour False 17) (hour False 21)
      Token Time . partOfDay . notLatent <$>
        intersect (cycleNth TG.Day 0) td
  }

ruleLastDayofweekTime2 :: Rule
ruleLastDayofweekTime2 = Rule
  { name = "last <day-of-week> <time>"
  , pattern =
    [ regex "zadnj(ih?|a|e(ga?)?)"
    , Predicate isADayOfWeek
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:Token Time td2:_) ->
        Just . Token Time $ predLastOf td1 td2
      _ -> Nothing
  }

ruleLastNCycle :: Rule
ruleLastNCycle = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "prethodn(ih?|a|e)|pro(s|\x0161)l(a|e|ih?)"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleN True grain $ - n
      _ -> Nothing
  }

ruleWithinDuration :: Rule
ruleWithinDuration = Rule
  { name = "within <duration>"
  , pattern =
    [ regex "(u|za )vrijeme"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> Token Time <$>
        interval TTime.Open (cycleNth TG.Second 0) (inDuration dd)
      _ -> Nothing
  }

ruleMidnighteodendOfDay :: Rule
ruleMidnighteodendOfDay = Rule
  { name = "midnight|EOD|end of day"
  , pattern =
    [ regex "(u )?pono(c|\x0107)i?|(the )?(EOD|((do )? kraja? dana))"
    ]
  , prod = \_ -> Just . Token Time $ hour False 0
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
      (Token Time td1:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleAboutTimeofday :: Rule
ruleAboutTimeofday = Rule
  { name = "about <time-of-day>"
  , pattern =
    [ regex "(oko|cca|otprilike)"
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
    [ regex "((nekad|najkasnije) )?(prije|do)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Just . Token Time $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "u|@"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Just . Token Time $ notLatent td
      _ -> Nothing
  }

ruleNamedmonth6 :: Rule
ruleNamedmonth6 = Rule
  { name = "named-month"
  , pattern =
    [ regex "lipa?nj(a|u)?|jun(i|u|a)?|jun\\.?|lip?\\.?|(\x0161|s)est(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 6
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "u"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNamedmonth8 :: Rule
ruleNamedmonth8 = Rule
  { name = "named-month"
  , pattern =
    [ regex "kolovoz(a|u)?|august(a|u)?|aug\\.?|kol\\.?|osm(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 8
  }

ruleTimePartofday :: Rule
ruleTimePartofday = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Time pod:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "(za )?vikenda?"
    ]
  , prod = \_ -> do
      fri <- intersect (dayOfWeek 5) (hour False 18)
      mon <- intersect (dayOfWeek 1) (hour False 0)
      Token Time <$> interval TTime.Open fri mon
  }

rulePrijeDuration :: Rule
rulePrijeDuration = Rule
  { name = "prije <duration>"
  , pattern =
    [ regex "prije"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> Just . Token Time $ durationAgo dd
      _ -> Nothing
  }

ruleAboutDuration :: Rule
ruleAboutDuration = Rule
  { name = "about <duration>"
  , pattern =
    [ regex "oko"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> Just . Token Time $ inDuration dd
      _ -> Nothing
  }

ruleEomendOfMonth :: Rule
ruleEomendOfMonth = Rule
  { name = "EOM|End of month"
  , pattern =
    [ regex "(the )?(EOM|(do )? kraja mjeseca)"
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Month 1
  }

ruleNameddayDayofmonthOrdinal :: Rule
ruleNameddayDayofmonthOrdinal = Rule
  { name = "<named-day> <day-of-month> (ordinal)"
  , pattern =
    [ Predicate isADayOfWeek
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "sljede(c|\x0107)(i|u|a|e(ga?)?)"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Just . Token Time $ predNth 0 True td
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
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter False TG.Quarter (n - 1) td
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
    [ regex "(u )?(sljede(c|\x0107)(ih?|a|eg?))"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain n
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "(u )?u?jutros?"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 4) (hour False 12)
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "ov(aj?|og?|i|e)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay . notLatent <$>
        intersect (cycleNth TG.Day 0) td
      _ -> Nothing
  }

ruleThisCycle :: Rule
ruleThisCycle = Rule
  { name = "this <cycle>"
  , pattern =
    [ regex "ov(aj?|og?|e|i)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> Just . Token Time $ cycleNth grain 0
      _ -> Nothing
  }

ruleUNamedmonth :: Rule
ruleUNamedmonth = Rule
  { name = "u <named-month>"
  , pattern =
    [ regex "u"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "ov(aj?|og?|e)(sad)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Just . Token Time $ predNth 0 False td
      _ -> Nothing
  }

ruleDayofmonthNonOrdinalOfNamedmonth :: Rule
ruleDayofmonthNonOrdinalOfNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) of <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , regex "of|in"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleHhmmMilitaryPrefixedWithMToAvoidAmbiguityWithYears :: Rule
ruleHhmmMilitaryPrefixedWithMToAvoidAmbiguityWithYears = Rule
  { name = "hhmm (military, prefixed with m to avoid ambiguity with years)"
  , pattern =
    [ regex "m((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ timeOfDayAMPM (hourMinute False h m) False
      _ -> Nothing
  }

ruleDayBeforeYesterday :: Rule
ruleDayBeforeYesterday = Rule
  { name = "day before yesterday"
  , pattern =
    [ regex "(prekju(c|\x010d)er)"
    ]
  , prod = \_ -> Just . Token Time . cycleNth TG.Day $ - 2
  }

ruleAfterLunch :: Rule
ruleAfterLunch = Rule
  { name = "after lunch"
  , pattern =
    [ regex "poslije ru(c|\x010d)ka"
    ]
  , prod = \_ -> do
      td <- interval TTime.Open (hour False 13) (hour False 17)
      Token Time . partOfDay . notLatent <$>
        intersect (cycleNth TG.Day 0) td
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $
       liftM2 (||) (isIntegerBetween (- 10000) 0) (isIntegerBetween 25 999)
    ]
  , prod = \tokens -> case tokens of
     (token:_) -> do
       n <- getIntValue token
       tt . mkLatent $ year n
     _ -> Nothing
  }

ruleYesterday :: Rule
ruleYesterday = Rule
  { name = "yesterday"
  , pattern =
    [ regex "(ju(c|\x010d)er)"
    ]
  , prod = \_ -> Just . Token Time . cycleNth TG.Day $ - 1
  }

ruleSeason2 :: Rule
ruleSeason2 = Rule
  { name = "season"
  , pattern =
    [ regex "jesen(i|as)?"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 9 23) (monthDay 12 21)
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "(nekad |najranije )?(prije|od)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Just . Token Time $ withDirection TTime.After td
      _ -> Nothing
  }

ruleChristmas :: Rule
ruleChristmas = Rule
  { name = "christmas"
  , pattern =
    [ regex "(zi(c|\x0107)bo|bo(z|\x017e)i(c|\x0107))(a|u|ni|na)?"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 12 25
  }

ruleDayofmonthOrdinal :: Rule
ruleDayofmonthOrdinal = Rule
  { name = "<day-of-month>. (ordinal)"
  , pattern =
    [ Predicate isDOMOrdinal
    , regex "\\.|i|og"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleOrdinalCycleAfterTime :: Rule
ruleOrdinalCycleAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "nakon"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        Just . Token Time $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleOrdinalCycleOfTime :: Rule
ruleOrdinalCycleOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "od|u"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        Just . Token Time $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleAfterNextTime :: Rule
ruleAfterNextTime = Rule
  { name = "after next <time>"
  , pattern =
    [ regex "nakon sljede(\x0107|c)(i|e|a)(ga?)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Just . Token Time $ predNth 1 True td
      _ -> Nothing
  }

ruleNamedmonth5 :: Rule
ruleNamedmonth5 = Rule
  { name = "named-month"
  , pattern =
    [ regex "sviba?nj(a|u)?|maj|svi\\.?|pet(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 5
  }

ruleNamedday7 :: Rule
ruleNamedday7 = Rule
  { name = "named-day"
  , pattern =
    [ regex "nedjelj(a|e|u)|ned\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 7
  }

ruleDayAfterTomorrow :: Rule
ruleDayAfterTomorrow = Rule
  { name = "day after tomorrow"
  , pattern =
    [ regex "(preko?sutra)"
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Day 2
  }

ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[(:.\\si]+([0-5]\\d)"
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
    [ regex "(na)?ve(c|\x010d)er(as)?"
    ]
  , prod = \_ -> do
      let today = cycleNth TG.Day 0
      evening <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay . notLatent <$> intersect today evening
  }

ruleBeforeLasttime :: Rule
ruleBeforeLasttime = Rule
  { name = "before last<time>"
  , pattern =
    [ regex "prije (prethodn(i|u|a|e|o(ga?)?)|pro(s|\x0161)l(ih?|a|e|o(ga?)?))"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Just . Token Time $ predNth (-2) False td
      _ -> Nothing
  }

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ liftM2 (&&) (isGrainFinerThan TG.Day) isNotLatent
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone tz td
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
        n <- getIntValue token
        tt $ year n
      _ -> Nothing
  }

ruleNamedmonth10 :: Rule
ruleNamedmonth10 = Rule
  { name = "named-month"
  , pattern =
    [ regex "listopad(a|u)?|oktobar(a|u)?|okt\\.?|lis\\.?|deset(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 10
  }

ruleHalloweenDay :: Rule
ruleHalloweenDay = Rule
  { name = "halloween day"
  , pattern =
    [ regex "no(c|\x0107) vje(s|\x0161)tica"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 10 31
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

ruleFathersDay :: Rule
ruleFathersDay = Rule
  { name = "Father's Day"
  , pattern =
    [ regex "dan (o(c|\x010d)eva|tata)"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 2 7 6
  }

ruleByTime :: Rule
ruleByTime = Rule
  { name = "by <time>"
  , pattern =
    [ regex "do"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Token Time <$> interval TTime.Open (cycleNth TG.Second 0) td
      _ -> Nothing
  }

ruleCycleBeforeTime :: Rule
ruleCycleBeforeTime = Rule
  { name = "<cycle> before <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "prije"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        Just . Token Time $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[\\/](0?[1-9]|1[0-2])[\\/](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTimeofdayTimeofdayInterval :: Rule
ruleTimeofdayTimeofdayInterval = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ liftM2 (&&) isNotLatent isATimeOfDay
    , regex "\\-|:"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNamedmonth11 :: Rule
ruleNamedmonth11 = Rule
  { name = "named-month"
  , pattern =
    [ regex "studen(i|oga?|om)|novemba?r(a|u)?|nov\\.?|stu\\.?|jedanaest(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 11
  }

ruleDurationAfterTime :: Rule
ruleDurationAfterTime = Rule
  { name = "<duration> after <time>"
  , pattern =
    [ dimension Duration
    , regex "nakon|poslije|od"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        Just . Token Time $ durationAfter dd td
      _ -> Nothing
  }

ruleEveningnight :: Rule
ruleEveningnight = Rule
  { name = "evening|night"
  , pattern =
    [ regex "(na)?ve(c|\x010d)er(i|as)?"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 18) (hour False 0)
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
        tt . cycleNthAfter True TG.Quarter (n - 1) $
          cycleNth TG.Year 0
      _ -> Nothing
  }

ruleDayBeforeDayBeforeYesterday :: Rule
ruleDayBeforeDayBeforeYesterday = Rule
  { name = "day before day before yesterday"
  , pattern =
    [ regex "(prek\\s?prekju(c|\x010d)er)"
    ]
  , prod = \_ -> Just . Token Time . cycleNth TG.Day $ - 3
  }

ruleNamedday3 :: Rule
ruleNamedday3 = Rule
  { name = "named-day"
  , pattern =
    [ regex "srijed(a|e|u)|sri\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 3
  }

ruleDurationBeforeTime :: Rule
ruleDurationBeforeTime = Rule
  { name = "<duration> before <time>"
  , pattern =
    [ dimension Duration
    , regex "prije"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        Just . Token Time $ durationBefore dd td
      _ -> Nothing
  }

ruleEoyendOfYear :: Rule
ruleEoyendOfYear = Rule
  { name = "EOY|End of year"
  , pattern =
    [ regex "(the )?(EOY|(do )? kraja? godine)"
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Year 1
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "(sutra(dan)?)"
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Day 1
  }

ruleMothersDay :: Rule
ruleMothersDay = Rule
  { name = "Mother's Day"
  , pattern =
    [ regex "maj(c|\x010d)in dan"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 1 7 5
  }

ruleTimeofdayOclock :: Rule
ruleTimeofdayOclock = Rule
  { name = "<time-of-day> o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "sat(i|a)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Just . Token Time $ notLatent td
      _ -> Nothing
  }

ruleYear2 :: Rule
ruleYear2 = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isOrdinalBetween 1000 2100
    , regex "\\.|e"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt $ year v
      _ -> Nothing
  }

ruleNamedmonth9 :: Rule
ruleNamedmonth9 = Rule
  { name = "named-month"
  , pattern =
    [ regex "ruja?n(a|u)?|septemba?r(a|u)?|sept?\\.?|ruj\\.?|devet(i|a|o(ga?)?)"
    ]
  , prod = \_ -> Just . Token Time $ month 9
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
      (token:Token Time td:Token RegexMatch (GroupMatch (match:_)):_) -> do
        intVal <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year intVal)
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
  [ ruleAboutDuration
  , ruleAboutTimeofday
  , ruleAbsorptionOfAfterNamedDay
  , ruleAfterDuration
  , ruleAfterLunch
  , ruleAfterNextTime
  , ruleAfterTimeofday
  , ruleAfterWork
  , ruleAfternoon
  , ruleAtTimeofday
  , ruleBeforeLasttime
  , ruleBetweenDatetimeAndDatetimeInterval
  , ruleBetweenTimeofdayAndTimeofdayInterval
  , ruleByTheEndOfTime
  , ruleByTime
  , ruleChristmas
  , ruleChristmasEve
  , ruleCycleAfterTime
  , ruleCycleBeforeTime
  , ruleDatetimeDatetimeInterval
  , ruleDayAfterTomorrow
  , ruleDayBeforeDayBeforeYesterday
  , ruleDayBeforeYesterday
  , ruleDayofmonthNonOrdinalNamedmonth
  , ruleDayofmonthNonOrdinalOfNamedmonth
  , ruleDayofmonthOrdinal
  , ruleDayofmonthOrdinalOfNamedmonth
  , ruleDayofmonthordinalNamedmonth
  , ruleDayofmonthordinalNamedmonthYear
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleDurationAfterTime
  , ruleDurationBeforeTime
  , ruleDurationFromNow
  , ruleEarlyMorning
  , ruleEomendOfMonth
  , ruleEoyendOfYear
  , ruleEveningnight
  , ruleExactlyTimeofday
  , ruleFathersDay
  , ruleForDuration
  , ruleFromDatetimeDatetimeInterval
  , ruleFromTimeofdayTimeofdayInterval
  , ruleHalfIntegerHrStyleHourofday
  , ruleHalloweenDay
  , ruleHhmm
  , ruleHhmmMilitaryPrefixedWithMToAvoidAmbiguityWithYears
  , ruleHhmmss
  , ruleInDuration
  , ruleInduringThePartofday
  , ruleIntersect
  , ruleIntersectBy
  , ruleLastCycle
  , ruleLastCycleOfTime
  , ruleLastDayofweekTime
  , ruleLastDayofweekTime2
  , ruleLastNCycle
  , ruleLastTime
  , ruleLateNight
  , ruleLunch
  , ruleMidnighteodendOfDay
  , ruleMonthDdddInterval
  , ruleMorning
  , ruleMothersDay
  , ruleNamedday
  , ruleNamedday2
  , ruleNamedday3
  , ruleNamedday4
  , ruleNamedday5
  , ruleNamedday6
  , ruleNamedday7
  , ruleNameddayDayofmonthOrdinal
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
  , ruleNamedmonthDayofmonthNonOrdinal
  , ruleNamedmonthDayofmonthOrdinal
  , ruleNewYearsDay
  , ruleNewYearsEve
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNextTime
  , ruleNoon
  , ruleNow
  , ruleNthTimeAfterTime
  , ruleNthTimeOfTime
  , ruleOrdinalCycleAfterTime
  , ruleOrdinalCycleOfTime
  , ruleOrdinalQuarter
  , ruleOrdinalQuarterYear
  , rulePrijeDuration
  , ruleNumeralAfterpastHourofday
  , ruleQuarterAfterpastHourofday
  , ruleHalfAfterpastHourofday
  , ruleNumeralTotillbeforeIntegerHourofday
  , ruleQuarterTotillbeforeIntegerHourofday
  , ruleHalfTotillbeforeIntegerHourofday
  , ruleHourofdayNumeral
  , ruleHourofdayQuarter
  , ruleHourofdayHalf
  , ruleSeason
  , ruleSeason2
  , ruleSeason3
  , ruleSeason4
  , ruleSinceTimeofday
  , ruleThisCycle
  , ruleThisPartofday
  , ruleThisTime
  , ruleThisnextDayofweek
  , ruleTimeAfterNext
  , ruleTimeBeforeLast
  , ruleTimePartofday
  , ruleTimezone
  , ruleTimeofdayApproximately
  , ruleTimeofdayLatent
  , ruleTimeofdayOclock
  , ruleTimeofdayTimeofdayInterval
  , ruleToday
  , ruleTomorrow
  , ruleTonight
  , ruleUNamedday
  , ruleUNamedmonth
  , ruleUntilTimeofday
  , ruleValentinesDay
  , ruleWeekend
  , ruleWithinDuration
  , ruleYear
  , ruleYear2
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYesterday
  , ruleYyyymmdd
  , ruleZaNumeralHourofday
  , ruleZaQuarterHourofday
  , ruleZaHalfHourofday
  ]
