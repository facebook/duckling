-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.AR.Rules
  ( rules
  ) where

import Data.Maybe
import Data.Text (Text)
import Prelude
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Time.Computed
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

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

ruleIntersectOf :: Rule
ruleIntersectOf = Rule
  { name = "intersect by \",\", \"of\", \"from\", \"'s\""
  , pattern =
    [ Predicate isNotLatent
    , regex "من|,|،|لـ?|بـ?|الموافق( ل)?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleAbsorbOnTime :: Rule
ruleAbsorbOnTime = Rule
  { name = "on <date>"
  , pattern =
    [ regex "في"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbOnADOW :: Rule
ruleAbsorbOnADOW = Rule
  { name = "on a <named-day>"
  , pattern =
    [ regex "يوم"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbInMonth :: Rule
ruleAbsorbInMonth = Rule
  { name = "in <named-month>"
  , pattern =
    [ regex "شهر"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbCommaTOD :: Rule
ruleAbsorbCommaTOD = Rule
  { name = "absorption of , after named day"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "[,،]"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> Just token
      _ -> Nothing
  }

instants :: [(Text, String, TG.Grain, Int)]
instants =
  [ ("right now", "الان|حالا|(في )?هذه اللحظ[ةه]", TG.Second, 0)
  , ("today", "اليوم", TG.Day, 0)
  , ("tomorrow", "((يوم )?(غدا?|الغد))|(بكر[اةه])", TG.Day, 1)
  -- TODO: add regex for more variation of yesterday
  , ("yesterday", "[أا]مس|(ال|ام)بارح[ةه]?", TG.Day, - 1)
  , ("end of month", "(نهاي[ةه] الشهر)", TG.Month, 1)
  , ("end of year", "(نهاي[ةه] (السن[ةه]|العام))", TG.Year, 1)
  ]

ruleInstants :: [Rule]
ruleInstants = map go instants
  where
    go (name, regexPattern, grain, n) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ cycleNth grain n
      }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "ال[اآ]ن|حالا"
    ]
  , prod = \_ -> tt now
  }

ruleThisDOW :: Rule
ruleThisDOW = Rule
  { name = "this <day-of-week>"
  , pattern =
    [ regex "هذا"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleDOWNext :: Rule
ruleDOWNext = Rule
  { name = "<day-of-week> next"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "القادم|التالي|ال[آا]تي?|القادم"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleDOWLast :: Rule
ruleDOWLast = Rule
  { name = "<day-of-week> last"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "السابق|الماضي|المنصرم|الفا[يئ]ت"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ predNth (- 1) True td
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "هذا"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "<time> next"
  , pattern =
    [ Predicate isNotLatent
    , regex "التالي"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "([اآ]خر)"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (- 1) False td
      _ -> Nothing
  }

-- to match 'عام سابق', 'عامان سابقان' or 'سابقة' and the same for 'منصرم' and 'ماضي'
ruleLastTime2 :: Rule
ruleLastTime2 = Rule
  { name = "<time> last"
  , pattern =
    [ Predicate isNotLatent
    , regex "((ال)?ماضيت?(ان|ين|ة|ه)?|(ال)?منصرمت?(ان|ين|ة|ه)?|(ال)?سابقت?(ان|ين|ة|ه)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ predNth (- 1) False td
      _ -> Nothing
  }

ruleLastDOWOfTime :: Rule
ruleLastDOWOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "[اآ]خر"
    , Predicate isADayOfWeek
    , regex "من|في"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleDOWTheLastOfTime :: Rule
ruleDOWTheLastOfTime = Rule
  { name = "<day-of-week> the last of <time>"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "(ال[اأآ]خي?رة?) (من|في|ب|ل)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "[اآ]خر"
    , dimension TimeGrain
    , regex "(من|في|ل|ب)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleCycleLastOfTime :: Rule
ruleCycleLastOfTime = Rule
  { name = "<cycle> last of <time>"
  , pattern =
    [ regex "ال"
    , dimension TimeGrain
    , regex "ال[اأ]خيرة? (من|في|ل|ب)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "في|من|ب"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleTimeNthOfTime :: Rule
ruleTimeNthOfTime = Rule
  { name = "<time> nth of <time>"
  , pattern =
    [ dimension Time
    , dimension Ordinal
    , regex "من|في"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Ordinal od:_:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "بعد"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleTimeNthAfterTime :: Rule
ruleTimeNthAfterTime = Rule
  { name = "<time> nth after <time>"
  , pattern =
    [ dimension Time
    , dimension Ordinal
    , regex "بعد"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Ordinal od:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern = [Predicate $ isIntegerBetween 1000 2100]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt $ year n
      _ -> Nothing
  }

ruleYearPastLatent :: Rule
ruleYearPastLatent = Rule
 { name = "past year (latent)"
 , pattern =
   [ Predicate $
       or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 999]
   ]
 , prod = \tokens -> case tokens of
     (token:_) -> do
       n <- getIntValue token
       tt . mkLatent $ year n
     _ -> Nothing
 }

ruleYearFutureLatent :: Rule
ruleYearFutureLatent = Rule
 { name = "future year (latent)"
 , pattern = [Predicate $ isIntegerBetween 2101 10000]
 , prod = \tokens -> case tokens of
     (token:_) -> do
       n <- getIntValue token
       tt . mkLatent $ year n
     _ -> Nothing
 }

ruleDOMLatent :: Rule
ruleDOMLatent = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern = [Predicate isDOMOrdinal]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ dayOfMonth n
      _ -> Nothing
  }

ruleNamedDOMOrdinal :: Rule
ruleNamedDOMOrdinal = Rule
  { name = "<named-month>|<named-day> <day-of-month> (ordinal)"
  , pattern =
    [ Predicate $ or . sequence [isAMonth, isADayOfWeek]
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleMonthDOMNumeral :: Rule
ruleMonthDOMNumeral = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMOfMonth :: Rule
ruleDOMOfMonth = Rule
  { name = "<day-of-month> (ordinal or number) of <named-month>"
  , pattern =
    [ Predicate isDOMValue
    , regex "(من|في|بـ?)"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMMonth :: Rule
ruleDOMMonth = Rule
  { name = "<day-of-month> (ordinal or number) <named-month>"
  , pattern =
    [ Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMOrdinalMonthYear :: Rule
ruleDOMOrdinalMonthYear = Rule
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

ruleDOMOrdinalDayMonthYear :: Rule
ruleDOMOrdinalDayMonthYear = Rule
  { name = "<day-of-month>(ordinal) <named-month> year"
  , pattern =
    [ Predicate isDOMOrdinal
    , regex "يوم( من| في| بـ?)?"
    , Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:Token RegexMatch (GroupMatch (match:_)):_) -> do
        intVal <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year intVal)
      _ -> Nothing
  }

ruleTODIntegerLatent :: Rule
ruleTODIntegerLatent = Rule
  { name = "time-of-day integer (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour True n
      _ -> Nothing
  }

ruleTODOrdinalLatent :: Rule
ruleTODOrdinalLatent = Rule
  { name = "time-of-day ordinal (latent)"
  , pattern =
    [ Predicate $ isOrdinalBetween 1 24
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour True n
      _ -> Nothing
  }

ruleHourTOD :: Rule
ruleHourTOD = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "(ال)?ساع[ةه]"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleAtHourTOD :: Rule
ruleAtHourTOD = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "عند ((ال)?ساع[ةه])?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleHODAndInteger :: Rule
ruleHODAndInteger = Rule
  { name = "<hour-of-day> and integer"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , regex "و"
    , Predicate $ isIntegerBetween 0 60
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        tt $ hourMinute is12H hours (floor v)
      _ -> Nothing
  }

ruleHODAndIntegerMinutes :: Rule
ruleHODAndIntegerMinutes = Rule
  { name = "<hour-of-day> and integer minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "و"
    , Predicate $ isIntegerBetween 0 60
    , regex "دقيق[ةه]|دقا[ئي]ق"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        tt $ hourMinute is12H hours (floor v)
      _ -> Nothing
  }

ruleHHMM :: Rule
ruleHHMM = Rule
  { name = "hh:mm"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleHHMMLatent :: Rule
ruleHHMMLatent = Rule
  { name = "hhmm (latent)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . mkLatent $ hourMinute True h m
      _ -> Nothing
  }

ruleHHMMSS :: Rule
ruleHHMMSS = Rule
  { name = "hh:mm:ss"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)[:.]([0-5]\\d)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        s <- parseInt ss
        tt $ hourMinuteSecond True h m s
      _ -> Nothing
  }

ruleTODAMPM :: Rule
ruleTODAMPM = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "([ap])(\\s|\\.)?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (_:ap:_)):_) ->
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
  }

ruleHONumeral :: Rule
ruleHONumeral = Rule
  { name = "<hour-of-day> <integer>"
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

ruleHODHalf :: Rule
ruleHODHalf = Rule
  { name = "<hour-of-day> half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(و ?)?نصف?( ساع[ةه])?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHODThird :: Rule
ruleHODThird = Rule
  { name = "<hour-of-day> third"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(و ?)?ثلث( ساع[ةه])?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 20
      _ -> Nothing
  }

ruleHODQuarter :: Rule
ruleHODQuarter = Rule
  { name = "<hour-of-day> quarter"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(و ?)?ربع( ساع[ةه])?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleThirdToHOD :: Rule
ruleThirdToHOD = Rule
  { name = "<hour-of-day> till third"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "[إا]لا ثلثا?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 20 td
      _ -> Nothing
  }

ruleQuarterToHOD :: Rule
ruleQuarterToHOD = Rule
  { name = "<hour-of-day> till quarter"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "[إا]لا ربعا?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleHODAndNumeralAfter :: Rule
ruleHODAndNumeralAfter = Rule
  { name = "<hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "و"
    , Predicate $ isIntegerBetween 1 59
    , regex "(دقيق[ةه]|دقا[ئي]ق)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> do
        n <- getIntValue token
        Token Time <$> minutesAfter n td
      _ -> Nothing
  }

ruleNumeralAfterHOD :: Rule
ruleNumeralAfterHOD = Rule
  { name = "integer after <hour-of-day>"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "(دقيق[ةه]|دقا[ئي]ق) بعد"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesAfter n td
      _ -> Nothing
  }

ruleHalfAfterHOD :: Rule
ruleHalfAfterHOD = Rule
  { name = "half after <hour-of-day>"
  , pattern =
    [ regex "نصف? ساع[ةه] بعد"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 30 td
      _ -> Nothing
  }

ruleThirdAfterHOD :: Rule
ruleThirdAfterHOD = Rule
  { name = "third after <hour-of-day>"
  , pattern =
    [ regex "ثلث ساع[ةه] بعد"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 20 td
      _ -> Nothing
  }

ruleQuarterAfterHOD :: Rule
ruleQuarterAfterHOD = Rule
  { name = "quarter after <hour-of-day>"
  , pattern =
    [ regex "ربع ساع[ةه] بعد"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 15 td
      _ -> Nothing
  }

ruleDDMM :: Rule
ruleDDMM = Rule
  { name = "dd/mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s?[/-]\\s?(1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
        tt $ monthDay m d
      _ -> Nothing
  }

ruleDDMMYYYY :: Rule
ruleDDMMYYYY = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/-](1[0-2]|0?[1-9])[-/](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        y <- parseInt yy
        d <- parseInt dd
        m <- parseInt mm
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleMMYYYY :: Rule
ruleMMYYYY = Rule
  { name = "mm/yyyy"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])[/-](\\d{4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        tt $ yearMonthDay y m 1
      _ -> Nothing
  }

ruleYYYYMMDD :: Rule
ruleYYYYMMDD = Rule
  { name = "yyyy-mm-dd"
  , pattern = [regex "(\\d{2,4})-(0?[1-9]|1[0-2])-(3[01]|[12]\\d|0?[1-9])"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleNoonMidnightEOD :: Rule
ruleNoonMidnightEOD = Rule
  { name = "noon|midnight|EOD|end of day"
  , pattern = [regex "(ليلا|مساء|نهاية (ال)يوم)|منتصف الليل"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> tt . hour False $
        if match == "منتصف الليل" then 12 else 0
      _ -> Nothing
  }

parOfDaysMap :: HashMap Text (Int, Int)
parOfDaysMap = HashMap.fromList
  [ ( "فجر",    (3, 6)   )
  , ( "صبح",    (0, 12)  )
  , ( "صباح",   (0, 12)  )
  , ( "نهار",   (6, 17)  )
  , ( "ظهر",    (11, 15) )
  , ( "ظهر",    (11, 15) )
  , ( "ظهيرة",  (11, 15) )
  , ( "عصر",    (15, 18) )
  , ( "مغرب",   (17, 21) )
  , ( "غداء",   (13, 16) )
  , ( "عشاء",   (18, 3)  )
  , ( "ليلة",   (18, 3)  )
  , ( "ليله",   (18, 3)  )
  , ( "مساء",   (12, 24)  )
  ]

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(ال)?(صبا?ح|فجر|ظهر|ظهيرة|فطور|إفطار|عصر|مغرب|مساء|عشاء|ليل[ةه]|غداء)ا?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):_) -> do
        (start, end) <- HashMap.lookup (Text.toLower match) parOfDaysMap
        td <- interval TTime.Open (hour False start) (hour False end)
        tt . partOfDay $ mkLatent td
      _ -> Nothing
  }

ruleAfterPartOfDays :: Rule
ruleAfterPartOfDays = Rule
  { name = "after part of days"
  , pattern =
    [ regex "بعد ال(فجر|ظهر|ظهيرة|فطور|إفطار|عصر|مغرب|عشاء|ليل|غداء)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- HashMap.lookup (Text.toLower match) parOfDaysMap
        td <- interval TTime.Open (hour False start) (hour False (end + 1))
        tt . partOfDay $ notLatent td
      _ -> Nothing
  }

ruleBeforePartOfDays :: Rule
ruleBeforePartOfDays = Rule
  { name = "before part of days"
  , pattern =
    [ regex "قبل ال(فجر|ظهر|ظهيرة|فطور|إفطار|عصر|مغرب|عشاء|ليل|غداء)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- HashMap.lookup (Text.toLower match) parOfDaysMap
        td <- interval TTime.Open (hour False (start - 1)) (hour False end)
        tt . partOfDay $ notLatent td
      _ -> Nothing
  }

ruleEarlyMorning :: Rule
ruleEarlyMorning = Rule
  { name = "early morning"
  , pattern =
    [ regex "(ال)?صبا?ح (((ال)?باكر)|بدري)"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 4) (hour False 9)
  }

rulePODIn :: Rule
rulePODIn = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "(في|خلال|في خلال|عند)(وقت )?( ال)?|وقت ال"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePODThis :: Rule
rulePODThis = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "هذ[اه](ال)?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay . notLatent <$>
        intersect today td
      _ -> Nothing
  }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern = [regex "(هذه )?الليل[ةه]"]
  , prod = \_ -> do
      evening <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay . notLatent <$> intersect today evening
  }

ruleAfterPartofday :: Rule
ruleAfterPartofday = Rule
  { name = "after lunch/work/school"
  , pattern =
    [ regex "بعد[\\s-]?((ال)?غداء|(ال)?عمل|(ال)?شغل|(ال)?مدرس[ةه])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- case Text.toLower match of
          "(ال)?غداء"  -> Just (hour False 13, hour False 17)
          "(ال)?عمل|(ال)?شغل"   -> Just (hour False 17, hour False 21)
          "(ال)?مدرس[ةه]" -> Just (hour False 15, hour False 21)
          _        -> Nothing
        td <- interval TTime.Open start end
        Token Time . partOfDay . notLatent <$> intersect today td
      _ -> Nothing
  }

-- Since part of days are latent, general time intersection is blocked
ruleTimePOD :: Rule
ruleTimePOD = Rule
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
    [ regex "(عطل[ةه] )?نهاي[ةه] ال[اأ]سبوع"
    ]
  , prod = \_ -> tt weekend
  }

ruleSeasons :: Rule
ruleSeasons = Rule
  { name = "seasons"
  , pattern = [regex "((ال)?صيف|(ال)?خريف|(ال)?شتاء|(ال)?ربيع)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- case Text.toLower match of
          "الصيف"   -> Just $ (monthDay 6 21 , monthDay 9 23)
          "صيف"     -> Just $ (monthDay 6 21 , monthDay 9 23)
          "الخريف"  -> Just $ (monthDay 9 23 , monthDay 12 21)
          "خريف"    -> Just $ (monthDay 9 23 , monthDay 12 21)
          "الشتاء"  -> Just $ (monthDay 12 21, monthDay 3 20)
          "شتاء"    -> Just $ (monthDay 12 21, monthDay 3 20)
          "الربيع"  -> Just $ (monthDay 3 20 , monthDay 6 21)
          "ربيع"    -> Just $ (monthDay 3 20 , monthDay 6 21)
          _         -> Nothing
        Token Time <$> interval TTime.Open start end
      _ -> Nothing

  }

ruleTODPrecision :: Rule
ruleTODPrecision = Rule
  { name = "<time-of-day> sharp|exactly"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(تقريبا)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePrecisionTOD :: Rule
rulePrecisionTOD = Rule
  { name = "about|exactly <time-of-day>"
  , pattern =
    [ regex "(حول|حوالي?|تقريبا|قراب[ةه]|(ب|في )حدود)"
    , Predicate $ isGrainFinerThan TG.Year
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleIntervalMonthDDDD :: Rule
ruleIntervalMonthDDDD = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMValue
    , regex "\\-|[اإ]لى|لـ?|حتى"
    , Predicate isDOMValue
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       token1:
       _:
       token2:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalDDDDMonth :: Rule
ruleIntervalDDDDMonth = Rule
  { name = "dd-dd <month> (interval)"
  , pattern =
    [ Predicate isDOMValue
    , regex "\\-|[اإ]لى|لـ?|حتى"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token1:
       _:
       token2:
       Token Time td:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalFromMonthDDDD :: Rule
ruleIntervalFromMonthDDDD = Rule
  { name = "from <month> dd-dd (interval)"
  , pattern =
    [ regex "من"
    , Predicate isAMonth
    , Predicate isDOMValue
    , regex "\\-|[اإ]لى|لـ?|حتى"
    , Predicate isDOMValue
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time td:
       token1:
       _:
       token2:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalFromDDDDMonth :: Rule
ruleIntervalFromDDDDMonth = Rule
  { name = "from <day-of-month> (ordinal or number) to <day-of-month> (ordinal or number) <named-month> (interval)"
  , pattern =
    [ regex "من|بين"
    , Predicate isDOMValue
    , regex "(و ?)?(\\-|[اإ]لى|لـ?|حتى)"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:
       token1:
       _:
       token2:
       Token Time td:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalFromDDDDInMonth :: Rule
ruleIntervalFromDDDDInMonth = Rule
  { name = "from <day-of-month> (ordinal or number) to <day-of-month> (ordinal or number) in <named-month> (interval)"
  , pattern =
    [ regex "من|بين"
    , Predicate isDOMValue
    , regex "(و ?)?(\\-|[اإ]لى|لـ?|حتى)"
    , Predicate isDOMValue
    , regex "من|في"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:
       token1:
       _:
       token2:
       _:
       Token Time td:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

-- Blocked for :latent time. May need to accept certain latents only, like hours
ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "(و ?)?(\\-|[اإ]لى|حتى)"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalFrom :: Rule
ruleIntervalFrom = Rule
  { name = "from <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "من"
    , Predicate isNotLatent
    , regex "(و ?)?(\\-|[اإ]لى|لـ?|حتى)"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between <time> and <time>"
  , pattern =
    [ regex "بين ?((ال)?ساع[ةه])?"
    , dimension Time
    , regex "و ?(بين)? ?((ال)?ساع[ةه])?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

-- Specific for time-of-day, to help resolve ambiguities
ruleIntervalTODDash :: Rule
ruleIntervalTODDash = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "(و ?)?(\\-|:|[اإ]لى|لـ?|حتى)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalTODFrom :: Rule
ruleIntervalTODFrom = Rule
  { name = "from <time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ regex "(من|بين|)((ال)?ساع[ةه]?)"
    , Predicate isATimeOfDay
    , regex "(و ?)?(\\-|[اإ]لى|لـ?|حتى)((ال)?ساع[ةه]?)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalTODBetween :: Rule
ruleIntervalTODBetween = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "بين"
    , Predicate isATimeOfDay
    , regex "و"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalBy :: Rule
ruleIntervalBy = Rule
  { name = "by <time>"
  , pattern =
    [ regex "حتى"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleIntervalUntilTOD :: Rule
ruleIntervalUntilTOD = Rule
  { name = "until <time-of-day>"
  , pattern =
    [ regex "([اأ]ي وقت)?(قبل|حتى|[إا]لى)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleIntervalAfterTOD :: Rule
ruleIntervalAfterTOD = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "([اأ]ي وقت)?(بعد)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleIntervalSinceTOD :: Rule
ruleIntervalSinceTOD = Rule
  { name = "since <time-of-day>"
  , pattern =
    [ regex "منذ"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

daysOfWeek :: [(Text, String)]
daysOfWeek =
  [ ( "Monday"   , "(ال)?[اإ]ثنين"   )
  , ( "Tuesday"  , "(ال)?ثلاثاء?"     )
  , ( "Wednesday", "(ال)?[اأ]ربعاء?" )
  , ( "Thursday" , "(ال)?خميس"       )
  , ( "Friday"   , "(ال)?جمع[ةه]"    )
  , ( "Saturday" , "(ال)?سبت"        )
  , ( "Sunday"   , "(ال)?[اأ]حد"     )
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
  [ ( "January"  , "يناير|كانون (ال)ثاني?"       )
  , ( "February" , "فبراير|شباط"                 )
  , ( "March"    , "مارس|[اآ]ذار"                )
  , ( "April"    , "[اأ]بريل|نيسان"              )
  , ( "May"      , "مايو|[اأ]ي[اآ]ر"             )
  , ( "June"     , "يونيو|حزيران"                )
  , ( "July"     , "يوليو|تموز"                  )
  , ( "August"   , "[اأ]غسطس|[اآ]ب"              )
  , ( "September", "سي?بتمبر|[اأ]يلول"           )
  , ( "October"  , "[اأ]كتوبر|تشرين (ال)?[اأ]ول" )
  , ( "November" , "نوفمبر|تشرين (ال)?ثاني"      )
  , ( "December" , "ديسمبر|كانون (ال)?[اأ]ول"    )
  ]

ruleMonths :: [Rule]
ruleMonths = zipWith go months [1..12]
  where
    go (name, regexPattern) i = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ month i
      }

ruleMonthsNumeral :: Rule
ruleMonthsNumeral = Rule
  { name = "month (integer)"
  , pattern =
    [ regex "شهر"
    , Predicate $ isIntegerBetween 1 12
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt $ month n
      _ -> Nothing
  }

ruleMonthsOrdinal :: Rule
ruleMonthsOrdinal = Rule
  { name = "month (ordinal)"
  , pattern =
    [ regex "الشهر"
    , dimension Ordinal
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt $ month n
      _ -> Nothing
  }

ruleYearInteger :: Rule
ruleYearInteger = Rule
  { name = "year (integer)"
  , pattern =
    [ regex "سنة|عام"
    , Predicate $ isIntegerBetween 1 3000
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt $ year n
      _ -> Nothing
  }

rulePartOfMonth :: Rule
rulePartOfMonth = Rule
  { name = "part of <named-month>"
  , pattern =
    [ regex "([اأ]وائل|[اأ]واخر|نهاي[ةه])-?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token Time td:_) -> do
        (sd, (Just end)) <- case Text.toLower match of
          "اوائل" -> Just (1, intersect (dayOfMonth 10) td)
          "أوائل" -> Just (1, intersect (dayOfMonth 10) td)
          "اواخر" -> Just (21, Just $ cycleLastOf TG.Day td)
          "أواخر" -> Just (21, Just $ cycleLastOf TG.Day td)
          _       -> Nothing
        start <- intersect td $ dayOfMonth sd
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleDayOfMonth :: Rule
ruleDayOfMonth = Rule
  { name = "day of <named-month>"
  , pattern =
    [ regex "(بداي[ةه]|منتصف|نصف?|[اآ]خر|نهاي[ةه])-?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) -> case Text.toLower match of
        "بداية" -> Token Time <$> intersect (dayOfMonth 1) td
        "بدايه" -> Token Time <$> intersect (dayOfMonth 1) td
        "منتصف" -> Token Time <$> intersect (dayOfMonth 15) td
        "نصف"   -> Token Time <$> intersect (dayOfMonth 15) td
        "نص"    -> Token Time <$> intersect (dayOfMonth 15) td
        _       -> tt $ cycleLastOf TG.Day td
      _ -> Nothing
  }

usHolidays :: [(Text, String, Int, Int)]
usHolidays =
  [ ( "Christmas"       , "(يوم |عطل[ةه] )?((ال)?كري?سماس)"              , 12, 25 )
  , ( "Christmas Eve"   , "(ليل[ةه] )((ال)?كري?سماس)"                    , 12, 24 )
  , ( "New Year's Eve"  , "(ليل[ةه] )(ر[اأ]س السن[ةه])"                  , 12, 31 )
  , ( "New Year's Day"  , "(يوم |عطل[ةه] )?(ر[اأ]س السن[ةه])"            , 1 , 1  )
  , ( "Valentine's Day" , "(عيد|يوم|عطل[ةه])((ال)?حب|(ال)?فالنتا?ين)"    , 2 , 14 )
  , ( "Halloween"       , "(عيد |يوم |عطل[ةه] )?((ال)?هالوي?ين)"         , 10, 31 )
  ]

ruleUSHolidays :: [Rule]
ruleUSHolidays = map go usHolidays
  where
    go (name, regexPattern, m, d) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ monthDay m d
      }

ruleThisLastNextCycle :: Rule
ruleThisLastNextCycle = Rule
  { name = "this|last <cycle>"
  , pattern =
    [ regex "(هذا|هذه|اخر|آخر)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token TimeGrain grain:
       _) -> case Text.toLower match of
        "هذا" -> tt $ cycleNth grain 0
        "هذه" -> tt $ cycleNth grain 0
        "اخر" -> tt . cycleNth grain $ - 1
        "آخر" -> tt . cycleNth grain $ - 1
        _     -> Nothing
      _ -> Nothing
  }

ruleThisLastNextCycle2 :: Rule
ruleThisLastNextCycle2 = Rule
  { name = "this|last the <cycle>"
  , pattern =
    [ regex "(هذا|هذه|اخر|آخر)"
    , regex "(ال)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _:
       Token TimeGrain grain:
       _) -> case Text.toLower match of
        "هذا" -> tt $ cycleNth grain 0
        "هذه" -> tt $ cycleNth grain 0
        "اخر" -> tt . cycleNth grain $ - 1
        "آخر" -> tt . cycleNth grain $ - 1
        _     -> Nothing
      _ -> Nothing
  }

ruleCycleThisLastNext :: Rule
ruleCycleThisLastNext = Rule
  { name = "<cycle> this|last|next"
  , pattern =
    [ regex "ال"
    , dimension TimeGrain
    , regex "(هذ|القادم|التالي|الحالي|المقبل|الجاي|السابق|الماضي?|الفا[ئي]ت|المنصرم)[اةه]?(ين|ان|ات)?"
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token TimeGrain grain:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
        "هذ"      -> tt $ cycleNth grain 0
        "الحالي"  -> tt $ cycleNth grain 0
        "القادم"  -> tt $ cycleNth grain 1
        "التالي"  -> tt $ cycleNth grain 1
        "المقبل"  -> tt $ cycleNth grain 1
        "الجاي"   -> tt $ cycleNth grain 1
        "السابق"  -> tt . cycleNth grain $ - 1
        "الماضي"  -> tt . cycleNth grain $ - 1
        "الماض"   -> tt . cycleNth grain $ - 1
        "المنصرم" -> tt . cycleNth grain $ - 1
        "الفائت"  -> tt . cycleNth grain $ - 1
        "الفايت"  -> tt . cycleNth grain $ - 1
        _         -> Nothing
      _ -> Nothing
  }

ruleCycleTheAfterBeforeTime :: Rule
ruleCycleTheAfterBeforeTime = Rule
  { name = "the <cycle> after|before <time>"
  , pattern =
    [ regex "ال"
    , dimension TimeGrain
    , regex "(الذ?ي |ال)(قبل|بعد)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token TimeGrain grain:
       Token RegexMatch (GroupMatch (_:match:_)):
       Token Time td:
       _) -> case Text.toLower match of
        "بعد" -> tt $ cycleNthAfter False grain 1 td
        _     -> tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleCycleAfterBeforeTime :: Rule
ruleCycleAfterBeforeTime = Rule
  { name = "<cycle> after|before <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "(قبل|بعد)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) -> case Text.toLower match of
        "بعد" -> tt $ cycleNthAfter False grain 1 td
        _     -> tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleCycleLastN :: Rule
ruleCycleLastN = Rule
  { name = "last n <cycle>"
  , pattern =
    [ regex "([اآ]خر)"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleN True grain $ - n
      _ -> Nothing
  }

ruleCycleNextN :: Rule
ruleCycleNextN = Rule
  { name = "<cycle> n next"
  , pattern =
    [ regex "ال"
    , dimension TimeGrain
    , regex "ال"
    , Predicate $ isIntegerBetween 1 9999
    , regex "(ال)(تالي|قادم)[ةه]?"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:token:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain n
      _ -> Nothing
  }

ruleCycleOrdinalOfTime :: Rule
ruleCycleOrdinalOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "في|من|بـ?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleOrdinalDayOfTime :: Rule
ruleCycleOrdinalDayOfTime = Rule
  { name = "<cycle> <ordinal> day? of <time>"
  , pattern =
    [ dimension TimeGrain
    , dimension Ordinal
    , regex "((ال)?يوم )?(بـ?|في|من)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:token:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleTheCycleTheOrdinalOfTime :: Rule
ruleTheCycleTheOrdinalOfTime = Rule
  { name = "the <cycle> the <ordinal> of <time>"
  , pattern =
    [ regex "ال"
    , dimension TimeGrain
    , dimension Ordinal
    , regex "في|من|بـ?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:token:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }


ruleCycleOrdinalAfterTime :: Rule
ruleCycleOrdinalAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "بعد"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleDurationInWithinAfter :: Rule
ruleDurationInWithinAfter = Rule
  { name = "in|within|after <duration>"
  , pattern =
    [ regex "(في|خلال|بعد)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration dd:
       _) -> case Text.toLower match of
         "خلال" -> Token Time <$> interval TTime.Open now (inDuration dd)
         "بعد" -> tt . withDirection TTime.After $ inDuration dd
         "في" -> tt $ inDuration dd
         _ -> Nothing
      _ -> Nothing
  }

ruleDurationHenceAgo :: Rule
ruleDurationHenceAgo = Rule
  { name = "<duration> hence|ago"
  , pattern =
    [ dimension Duration
    , regex "(مض[تى]|من الان)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
        "من الان" -> tt $ inDuration dd
        _        -> tt $ durationAgo dd
      _ -> Nothing
  }

ruleInNumeral :: Rule
ruleInNumeral = Rule
  { name = "in <number> (implicit minutes)"
  , pattern =
    [ regex "في|خلال"
    , Predicate $ isIntegerBetween 0 60
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        tt . inDuration . duration TG.Minute $ floor v
      _ -> Nothing
  }

ruleDurationAfterBeforeTime :: Rule
ruleDurationAfterBeforeTime = Rule
  { name = "<duration> after|before|from <time>"
  , pattern =
    [ dimension Duration
    , regex "(من|قبل|بعد)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) -> case Text.toLower match of
         "قبل" -> tt $ durationBefore dd td
         _     -> tt $ durationAfter dd td
      _ -> Nothing
  }

ruleIntervalForDurationFrom :: Rule
ruleIntervalForDurationFrom = Rule
  { name = "for <duration> from <time>"
  , pattern =
    [ regex "لمدة|ل"
    , dimension Duration
    , regex "(من بداي[ةه]|ابتداء ب|بداية من|من)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_:Token Time td1:_) ->
        Token Time <$> interval TTime.Open td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleTimeForDuration :: Rule
ruleTimeForDuration = Rule
  { name = "<time> for <duration>"
  , pattern =
    [ dimension Time
    , regex "لمدة|ل"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Open td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "(بتوقيت |)?\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (_:tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  [ ( "عيد الميلاد" ,"عيد الميلاد", monthDay 12 25 )
  ]

ruleComputedHolidays :: [Rule]
ruleComputedHolidays = mkRuleHolidays
  [ ( "عيد الأضحى" ,"عيد الأضحى", eidalAdha )
  , ( "عيد الفطر" ,"عيد الفطر", eidalFitr )
  , ( "عيد الفصح" ,"عيد الفصح", easterSunday )
  , ( "رأس السنة الهجرية" ,"رأس السنة الهجرية", muharram )
  ]

ruleComputedHolidays' :: [Rule]
ruleComputedHolidays' = mkRuleHolidays'
  [ ( "رمضان", "رمضان"
    , let start = ramadan
          end = cycleNthAfter False TG.Day (-1) eidalFitr
        in interval TTime.Open start end )
  ]

rules :: [Rule]
rules =
  [ ruleIntersect
  , ruleIntersectOf
  , ruleAbsorbOnTime
  , ruleAbsorbOnADOW
  , ruleAbsorbInMonth
  , ruleAbsorbCommaTOD
  , ruleThisDOW
  , ruleDOWNext
  , ruleDOWLast
  , ruleThisTime
  , ruleNextTime
  , ruleLastTime
  , ruleLastTime2
  , ruleLastDOWOfTime
  , ruleDOWTheLastOfTime
  , ruleLastCycleOfTime
  , ruleCycleLastOfTime
  , ruleNthTimeOfTime
  , ruleTimeNthOfTime
  , ruleNthTimeAfterTime
  , ruleTimeNthAfterTime
  , ruleYear
  , ruleYearPastLatent
  , ruleYearFutureLatent
  , ruleYearInteger
  , ruleMonthsNumeral
  , ruleMonthsOrdinal
  , ruleDOMLatent
  , ruleNamedDOMOrdinal
  , ruleMonthDOMNumeral
  , ruleDOMMonth
  , ruleDOMOfMonth
  , ruleDOMOrdinalMonthYear
  , ruleDOMOrdinalDayMonthYear
  , ruleTODIntegerLatent
  , ruleTODOrdinalLatent
  , ruleAtHourTOD
  , ruleHODAndInteger
  , ruleHODAndIntegerMinutes
  , ruleHourTOD
  , ruleHHMM
  , ruleHHMMLatent
  , ruleHHMMSS
  , ruleTODAMPM
  , ruleHONumeral
  , ruleHODHalf
  , ruleHODThird
  , ruleHODQuarter
  , ruleThirdToHOD
  , ruleQuarterToHOD
  , ruleHODAndNumeralAfter
  , ruleNumeralAfterHOD
  , ruleHalfAfterHOD
  , ruleThirdAfterHOD
  , ruleQuarterAfterHOD
  , ruleDDMM
  , ruleDDMMYYYY
  , ruleYYYYMMDD
  , ruleMMYYYY
  , ruleNoonMidnightEOD
  , rulePartOfDays
  , ruleAfterPartOfDays
  , ruleBeforePartOfDays
  , ruleEarlyMorning
  , rulePODIn
  , rulePODThis
  , ruleTonight
  , ruleAfterPartofday
  , ruleTimePOD
  , ruleWeekend
  , ruleSeasons
  , ruleTODPrecision
  , rulePrecisionTOD
  , ruleIntervalFromMonthDDDD
  , ruleIntervalFromDDDDMonth
  , ruleIntervalFromDDDDInMonth
  , ruleIntervalMonthDDDD
  , ruleIntervalDDDDMonth
  , ruleIntervalDash
  , ruleIntervalFrom
  , ruleIntervalBetween
  , ruleIntervalTODDash
  , ruleIntervalTODFrom
  , ruleIntervalTODBetween
  , ruleIntervalBy
  , ruleIntervalUntilTOD
  , ruleIntervalAfterTOD
  , ruleIntervalSinceTOD
  , ruleThisLastNextCycle
  , ruleThisLastNextCycle2
  , ruleCycleThisLastNext
  , ruleCycleTheAfterBeforeTime
  , ruleCycleAfterBeforeTime
  , ruleCycleLastN
  , ruleCycleNextN
  , ruleTheCycleTheOrdinalOfTime
  , ruleCycleOrdinalOfTime
  , ruleCycleOrdinalDayOfTime
  , ruleCycleOrdinalAfterTime
  , ruleDurationInWithinAfter
  , ruleDurationHenceAgo
  , ruleDurationAfterBeforeTime
  , ruleIntervalForDurationFrom
  , ruleTimeForDuration
  , ruleInNumeral
  , ruleTimezone
  , rulePartOfMonth
  , ruleDayOfMonth
  , ruleNow
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleUSHolidays
  ++ rulePeriodicHolidays
  ++ ruleComputedHolidays
  ++ ruleComputedHolidays'
