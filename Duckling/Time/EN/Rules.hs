-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.Rules where

import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Duration.Types (DurationData (..))
import Duckling.Numeral.Helpers (isNatural, parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Time.Computed
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..), TimeIntervalType (..))
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate $ isGrainFinerThan TG.Year
    , Predicate $ or . sequence [isNotLatent, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_)
        | (not $ TTime.latent td1) || (not $ TTime.latent td2) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleIntersectOf :: Rule
ruleIntersectOf = Rule
  { name = "intersect by \",\", \"of\", \"from\", \"'s\""
  , pattern =
    [ Predicate isNotLatent
    , regex "of|from|for|'s|,"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleIntersectYear :: Rule
ruleIntersectYear = Rule
  { name = "intersect by \",\", \"of\", \"from\" for year"
  , pattern =
    [ Predicate isNotLatent
    , regex "of|from|,"
    , Predicate $ isGrainOfTime TG.Year
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleAbsorbOnDay :: Rule
ruleAbsorbOnDay = Rule
  { name = "on <day>"
  , pattern =
    [ regex "on"
    , Predicate $ isGrainOfTime TG.Day
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbOnADOW :: Rule
ruleAbsorbOnADOW = Rule
  { name = "on a <named-day>"
  , pattern =
    [ regex "on a"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbInMonthYear :: Rule
ruleAbsorbInMonthYear = Rule
  { name = "in|during <named-month>|year"
  , pattern =
    [ regex "in|during"
    , Predicate $ or . sequence [isAMonth, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleAbsorbCommaTOD :: Rule
ruleAbsorbCommaTOD = Rule
  { name = "absorption of , after named day"
  , pattern =
    [ Predicate isADayOfWeek
    , regex ","
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> Just token
      _ -> Nothing
  }

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("right now"    , TG.Second, 0  , "((just|right)\\s*)now|immediately")
  , ("today"        , TG.Day   , 0  , "todays?|(at this time)"           )
  , ("tomorrow"     , TG.Day   , 1  , "(tmrw?|tomm?or?rows?)"            )
  , ("yesterday"    , TG.Day   , - 1, "yesterdays?"                      )
  ]

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "now"
    ]
  , prod = \_ -> tt now
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "this|next"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "this|current|coming"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "next"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "(this past|last|previous)"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (- 1) False td
      _ -> Nothing
  }

ruleLastWeekendOfMonth :: Rule
ruleLastWeekendOfMonth = Rule
  { name = "last weekend of <named-month>"
  , pattern =
    [ regex "last\\s(week(\\s|-)?end|wkend)\\s(of|in)"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td2:_) -> tt $ predLastOf weekend td2
      _ -> Nothing
  }

ruleTimeBeforeLastAfterNext :: Rule
ruleTimeBeforeLastAfterNext = Rule
  { name = "<time> before last|after next"
  , pattern =
    [ dimension Time
    , regex "(before last|after next)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (match:_)):_) ->
        tt $ predNth 1 (Text.toLower match == "after next") td
      _ -> Nothing
  }

ruleLastDOWOfTime :: Rule
ruleLastDOWOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "last"
    , Predicate isADayOfWeek
    , regex "of"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "last"
    , dimension TimeGrain
    , regex "of|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleLastNight :: Rule
ruleLastNight = Rule
  { name = "last night"
  , pattern =
    [ regex "(late )?last night"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let hours = if Text.toLower match == "late " then 3 else 6
            start = durationBefore (DurationData hours TG.Hour) today
        in Token Time . partOfDay . notLatent <$> interval TTime.Open start today
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "of|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleTheNthTimeOfTime :: Rule
ruleTheNthTimeOfTime = Rule
  { name = "the nth <time> of <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension Time
    , regex "of|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
         predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleTheNthTimeAfterTime :: Rule
ruleTheNthTimeAfterTime = Rule
  { name = "the nth <time> after <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension Time
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleNDOWFromTime :: Rule
ruleNDOWFromTime = Rule
  { name = "<integer> <day-of-week> from <time>"
  , pattern =
    [ dimension Numeral
    , Predicate isADayOfWeek
    , regex "from"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td1:_:Token Time td2:_) -> do
        n <- getIntValue token
        tt $ predNthAfter (n - 1) td1 td2
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
      [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 10000]
      ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleYearADBC :: Rule
ruleYearADBC = Rule
  { name = "<year> (bc|ad)"
  , pattern =
    [ Predicate $ isIntegerBetween (-10000) 10000
    , regex "(a\\.?d\\.?|b\\.?c\\.?)"
    ]
  , prod = \case
    (token:Token RegexMatch (GroupMatch (ab:_)):_) -> do
      y <- getIntValue token
      tt . yearADBC $ if Text.head (Text.toLower ab) == 'b' then -y else y
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

ruleTheDOMNumeral :: Rule
ruleTheDOMNumeral = Rule
  { name = "the <day-of-month> (number)"
  , pattern =
    [ regex "the"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ dayOfMonth n
      _ -> Nothing
  }

ruleTheDOMOrdinal :: Rule
ruleTheDOMOrdinal = Rule
  { name = "the <day-of-month> (ordinal)"
  , pattern =
    [ regex "the"
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Ordinal OrdinalData{TOrdinal.value = v}:
       _) -> tt $ dayOfMonth v
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
    , regex "of|in"
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

ruleDOMMonthYear :: Rule
ruleDOMMonthYear = Rule
  { name = "<day-of-month>(ordinal or number)/<named-month>/year"
  , pattern =
    [ Predicate isDOMValue
    , regex "[-/\\s]"
    , Predicate isAMonth
    , regex "[-/\\s]"
    , regex "(\\d{4})"
    ]
  , prod = \tokens -> case tokens of
      (token:
       _:
       Token Time td:
       _:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         intVal <- parseInt match
         dom <- intersectDOM td token
         Token Time <$> intersect dom (year intVal)
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

ruleIdesOfMonth :: Rule
ruleIdesOfMonth = Rule
  { name = "the ides of <named-month>"
  , pattern =
    [ regex "the ides? of"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td@TimeData {TTime.form = Just (TTime.Month m)}:_) ->
        Token Time <$>
          intersect td (dayOfMonth $ if elem m [3, 5, 7, 10] then 15 else 13)
      _ -> Nothing
  }

ruleTODLatent :: Rule
ruleTODLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (n < 13) n
      _ -> Nothing
  }

ruleAtTOD :: Rule
ruleAtTOD = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "at|@"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleTODOClock :: Rule
ruleTODOClock = Rule
  { name = "<time-of-day> o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "o.?clock"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
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
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)(?!.\\d)"
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

ruleMilitaryAMPM :: Rule
ruleMilitaryAMPM = Rule
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

ruleMilitarySpelledOutAMPM :: Rule
ruleMilitarySpelledOutAMPM = Rule
  { name = "military spelled out numbers am|pm"
  , pattern =
    [ Predicate $ isIntegerBetween 10 12
    , Predicate $ isIntegerBetween 1 59
    , regex "(in the )?([ap])(\\s|\\.)?m?\\.?"
    ]
    , prod = \tokens -> case tokens of
        (h:m:Token RegexMatch (GroupMatch (_:ap:_)):_) -> do
          hh <- getIntValue h
          mm <- getIntValue m
          tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True hh mm
        _ -> Nothing
  }

ruleMilitarySpelledOutAMPM2 :: Rule
ruleMilitarySpelledOutAMPM2 = Rule
  { name = "six thirty six a.m."
  , pattern =
    [ Predicate $ isIntegerBetween 110 999
    , regex "(in the )?([ap])(\\s|\\.)?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (token:Token RegexMatch (GroupMatch (_:ap:_)):_) -> do
        n <- getIntValue token
        m <- case mod n 100 of
          v | v < 60 -> Just v
          _          -> Nothing
        let h = quot n 100
        tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True h m
      _ -> Nothing
  }

ruleTODAMPM :: Rule
ruleTODAMPM = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(in the )?([ap])(\\s|\\.)?(m?)\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td@TimeData{TTime.latent = True}:
       Token RegexMatch (GroupMatch (_:ap:_:"":_)):
       _) ->
        tt . mkLatent $ timeOfDayAMPM (Text.toLower ap == "a") td
      (Token Time td@TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       Token RegexMatch (GroupMatch (_:ap:_)):
       _) | hours < 13 ->
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
  }

ruleHONumeral :: Rule
ruleHONumeral = Rule
  { name = "<hour-of-day> <integer>"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)
                          ,TTime.latent = isLatent}:
       token:
       _) -> do
        n <- getIntValue token
        if isLatent
          then tt . mkLatent $ hourMinute is12H hours n
          else tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHODHalf :: Rule
ruleHODHalf = Rule
  { name = "<hour-of-day> half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "half"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHODQuarter :: Rule
ruleHODQuarter = Rule
  { name = "<hour-of-day> quarter"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(a|one)? ?quarter"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleNumeralToHOD :: Rule
ruleNumeralToHOD = Rule
  { name = "<integer> to|till|before <hour-of-day>"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "to|till|before|of"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }

ruleHalfToHOD :: Rule
ruleHalfToHOD = Rule
  { name = "half to|till|before <hour-of-day>"
  , pattern =
    [ regex "half (to|till|before|of)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleQuarterToHOD :: Rule
ruleQuarterToHOD = Rule
  { name = "quarter to|till|before <hour-of-day>"
  , pattern =
    [ regex "(a|one)? ?quarter (to|till|before|of)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleNumeralAfterHOD :: Rule
ruleNumeralAfterHOD = Rule
  { name = "integer after|past <hour-of-day>"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "after|past"
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
  { name = "half after|past <hour-of-day>"
  , pattern =
    [ regex "half (after|past)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 30 td
      _ -> Nothing
  }

ruleQuarterAfterHOD :: Rule
ruleQuarterAfterHOD = Rule
  { name = "quarter after|past <hour-of-day>"
  , pattern =
    [ regex "(a|one)? ?quarter (after|past)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 15 td
      _ -> Nothing
  }

ruleHalfHOD :: Rule
ruleHalfHOD = Rule
  { name = "half <integer> (UK style hour-of-day)"
  , pattern =
    [ regex "half"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 30 td
      _ -> Nothing
  }

ruleMMYYYY :: Rule
ruleMMYYYY = Rule
  { name = "mm/yyyy"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])[/-](\\d{4})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        tt $ yearMonth y m
      _ -> Nothing
  }

ruleYYYYMM :: Rule
ruleYYYYMM = Rule
  { name = "yyyy-mm"
  , pattern =
    [ regex "(\\d{4})\\s*[/-]\\s*(1[0-2]|0?[1-9])"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (yy:mm:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        tt $ yearMonth y m
      _ -> Nothing
  }

ruleYYYYMMDD :: Rule
ruleYYYYMMDD = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})-(0?[1-9]|1[0-2])-(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleYYYYQQ :: Rule
ruleYYYYQQ = Rule
  { name = "yyyyqq"
  , pattern =
    [ regex "(\\d{2,4})q([1-4])"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (yy:qq:_)):_) -> do
        y <- parseInt yy
        q <- parseInt qq
        tt . cycleNthAfter True TG.Quarter (q - 1) $ year y
      _ -> Nothing
  }

ruleNoonMidnightEOD :: Rule
ruleNoonMidnightEOD = Rule
  { name = "noon|midnight|EOD|end of day"
  , pattern =
    [ regex "(noon|midni(ght|te)|(the )?(EOD|end of (the )?day))"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> tt . hour False $
        if Text.toLower match == "noon" then 12 else 0
      _ -> Nothing
  }

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(morning|after ?noo?n(ish)?|evening|night|(at )?lunch)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case Text.toLower match of
              "morning"  -> (hour False 4, hour False 12)
              "evening"  -> (hour False 18, hour False 0)
              "night"    -> (hour False 18, hour False 0)
              "lunch"    -> (hour False 12, hour False 14)
              "at lunch" -> (hour False 12, hour False 14)
              _          -> (hour False 12, hour False 19)
        td <- interval TTime.Open start end
        tt . partOfDay $ mkLatent td
      _ -> Nothing
  }

ruleEarlyMorning :: Rule
ruleEarlyMorning = Rule
  { name = "early morning"
  , pattern =
    [ regex "early ((in|hours of) the )?morning"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 4) (hour False 9)
  }

rulePODIn :: Rule
rulePODIn = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "(in|during)( the)?"
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
    [ regex "this"
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
  , pattern = [regex "(late )?toni(ght|gth|te)s?"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let h = if Text.toLower match == "late " then 21 else 18
        evening <- interval TTime.Open (hour False h) (hour False 0)
        Token Time . partOfDay . notLatent <$> intersect today evening
      _ -> Nothing
  }

ruleAfterPartofday :: Rule
ruleAfterPartofday = Rule
  { name = "after lunch/work/school"
  , pattern =
    [ regex "after[\\s-]?(lunch|work|school)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- case Text.toLower match of
          "lunch"  -> Just (hour False 13, hour False 17)
          "work"   -> Just (hour False 17, hour False 21)
          "school" -> Just (hour False 15, hour False 21)
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

rulePODofTime :: Rule
rulePODofTime = Rule
  { name = "<part-of-day> of <time>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "of"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time pod:_:Token Time td:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "(week(\\s|-)?end|wkend)s?"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

ruleWeek :: Rule
ruleWeek = Rule
 { name = "week"
 , pattern = [regex "(all|rest of the|the) week"]
 , prod = \case
     (Token RegexMatch (GroupMatch (match:_)):_) ->
       let end = cycleNthAfter True TG.Day (-2) $ cycleNth TG.Week 1
           period = case Text.toLower match of
                      "all" -> interval Closed (cycleNth TG.Week 0) end
                      "rest of the" -> interval Open today end
                      "the" -> interval Open today end
                      _ -> Nothing
       in case Text.toLower match of
         "the" -> Token Time . mkLatent <$> period
         _ -> Token Time <$> period
     _ -> Nothing
 }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "last|this|next <season>"
  , pattern =
    [ regex "(this|current|next|last|past|previous) seasons?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        n <- case Text.toLower match of
               "this" -> Just 0
               "current" -> Just 0
               "last" -> Just (-1)
               "past" -> Just (-1)
               "previous" -> Just (-1)
               "next" -> Just 1
               _ -> Nothing
        tt $ predNth n False season
      _ -> Nothing
  }

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "summer", "summer"     , monthDay  6 21, monthDay  9 23 )
  , ( "fall"  , "fall|autumn", monthDay  9 23, monthDay 12 21 )
  , ( "winter", "winter"     , monthDay 12 21, monthDay  3 20 )
  , ( "spring", "spring"     , monthDay  3 20, monthDay  6 21 )
  ]

ruleTODPrecision :: Rule
ruleTODPrecision = Rule
  { name = "<time-of-day> sharp|exactly"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(sharp|exactly|-?ish|approximately)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePrecisionTOD :: Rule
rulePrecisionTOD = Rule
  { name = "about|exactly <time-of-day>"
  , pattern =
    [ regex "(about|around|approximately|exactly)"
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
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
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
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
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
    [ regex "from"
    , Predicate isAMonth
    , Predicate isDOMValue
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
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
  { name = "from the <day-of-month> (ordinal or number) to the <day-of-month> (ordinal or number) <named-month> (interval)"
  , pattern =
    [ regex "from( the)?"
    , Predicate isDOMValue
    , regex "\\-|to( the)?|th?ru|through|(un)?til(l)?"
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

ruleIntervalFromDDDDOfMonth :: Rule
ruleIntervalFromDDDDOfMonth = Rule
  { name = "from the <day-of-month> (ordinal or number) to the <day-of-month> (ordinal or number) of <named-month> (interval)"
  , pattern =
    [ regex "from( the)?"
    , Predicate isDOMValue
    , regex "\\-|to( the)?|th?ru|through|(un)?til(l)?"
    , Predicate isDOMValue
    , regex "of"
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
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalSlash :: Rule
ruleIntervalSlash = Rule
  { name = "<datetime>/<datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "/"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        if sameGrain td1 td2 then
          Token Time <$> interval TTime.Closed td1 td2
        else Nothing
      _ -> Nothing
  }

ruleIntervalFrom :: Rule
ruleIntervalFrom = Rule
  { name = "from <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "from"
    , dimension Time
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , dimension Time
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
    [ regex "between"
    , dimension Time
    , regex "and"
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
    , regex "\\-|:|to|th?ru|through|(un)?til(l)?"
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
    [ regex "(later than|from|(in[\\s-])?between)"
    , Predicate isATimeOfDay
    , regex "((but )?before)|\\-|to|th?ru|through|(un)?til(l)?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

-- We can't take generic TOD (e.g. "6:30am - 9pm").
-- Those are handled by other rules.
ruleIntervalTODAMPM :: Rule
ruleIntervalTODAMPM = Rule
 { name = "hh(:mm) - <time-of-day> am|pm"
 , pattern =
   [ regex "(?:from )?((?:[01]?\\d)|(?:2[0-3]))([:.]([0-5]\\d))?"
   , regex "\\-|:|to|th?ru|through|(un)?til(l)?"
   , Predicate isATimeOfDay
   , regex "(in the )?([ap])(\\s|\\.)?m?\\.?"
   ]
 , prod = \tokens -> case tokens of
     (Token RegexMatch (GroupMatch (hh:_:mm:_)):
      _:
      Token Time td2:
      Token RegexMatch (GroupMatch (_:ap:_)):
      _) -> do
       h <- parseInt hh
       let ampm = Text.toLower ap == "a"
           td1 = case parseInt mm of
             Just m -> hourMinute True h m
             Nothing -> hour True h
       Token Time <$>
         interval TTime.Closed (timeOfDayAMPM ampm td1) (timeOfDayAMPM ampm td2)
     _ -> Nothing
 }

ruleIntervalTODBetween :: Rule
ruleIntervalTODBetween = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "between"
    , Predicate isATimeOfDay
    , regex "and"
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
    [ regex "by"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleIntervalByTheEndOf :: Rule
ruleIntervalByTheEndOf = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "by (the )?end of"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Closed now td
      _ -> Nothing
  }

ruleIntervalUntilTime :: Rule
ruleIntervalUntilTime = Rule
  { name = "until <time>"
  , pattern =
    [ regex "(anytime |sometimes? )?(before|(un)?til(l)?|through|up to)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt . withDirection TTime.Before $ notLatent td
      _ -> Nothing
  }

ruleIntervalAfterFromSinceTime :: Rule
ruleIntervalAfterFromSinceTime = Rule
  { name = "from|since|after <time>"
  , pattern =
    [ regex "from|since|(anytime |sometimes? )?after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt . withDirection TTime.After $ notLatent td
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "mondays?|mon\\.?"         )
  , ( "Tuesday"  , "tuesdays?|tues?\\.?"      )
  , ( "Wednesday", "wed?nesdays?|wed\\.?"     )
  , ( "Thursday" , "thursdays?|thu(rs?)?\\.?" )
  , ( "Friday"   , "fridays?|fri\\.?"         )
  , ( "Saturday" , "saturdays?|sat\\.?"       )
  , ( "Sunday"   , "sundays?|sun\\.?"         )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonthsWithLatent
  [ ( "January"  , "january|jan\\.?"    , False )
  , ( "February" , "february|feb\\.?"   , False )
  , ( "March"    , "march|mar\\.?"      , False )
  , ( "April"    , "april|apr\\.?"      , False )
  , ( "May"      , "may"                , True  )
  , ( "June"     , "june|jun\\.?"       , False )
  , ( "July"     , "july|jul\\.?"       , False )
  , ( "August"   , "august|aug\\.?"     , False )
  , ( "September", "september|sept?\\.?", False )
  , ( "October"  , "october|oct\\.?"    , False )
  , ( "November" , "november|nov\\.?"   , False )
  , ( "December" , "december|dec\\.?"   , False )
  ]

rulePartOfMonth :: Rule
rulePartOfMonth = Rule
  { name = "part of <named-month>"
  , pattern =
    [ regex "(early|mid|late)-?( of)?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "early" -> Just (1, 10)
          "mid"   -> Just (11, 20)
          "late"  -> Just (21, -1)
          _       -> Nothing
        start <- intersect td $ dayOfMonth sd
        end <- if ed /= -1
          then intersect td $ dayOfMonth ed
          else Just $ cycleLastOf TG.Day td
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOrBeginningOfMonth :: Rule
ruleEndOrBeginningOfMonth = Rule
  { name = "at the beginning|end of <named-month>"
  , pattern =
    [ regex "(at the )?(beginning|end) of"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "beginning" -> Just (1, 10)
          "end"       -> Just (21, -1)
          _           -> Nothing
        start <- intersect td $ dayOfMonth sd
        end <- if ed /= -1
          then intersect td $ dayOfMonth ed
          else Just $ cycleLastOf TG.Day td
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOfMonth :: Rule
ruleEndOfMonth = Rule
  { name = "end of month"
  , pattern = [ regex "(by (the )?|(at )?the )?(EOM|end of (the )?month)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_)
        | (Just start, Just end) <- parsed ->
          Token Time <$> interval TTime.Open start end
        where
          cycleMonth = cycleNth TG.Month
          parsed = if "by" `Text.isPrefixOf` Text.toLower match
            then
              ( Just now
              , intersect (dayOfMonth 1) $ cycleMonth 1)
            else
              ( intersect (dayOfMonth 21) $ cycleMonth 0
              , Just $ cycleLastOf TG.Day $ cycleMonth 0)
      _ -> Nothing
  }

ruleBeginningOfMonth :: Rule
ruleBeginningOfMonth = Rule
  { name = "beginning of month"
  , pattern = [ regex "((at )?the )?(BOM|beginning of (the )?month)" ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      end <- intersect (dayOfMonth 10) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfYear :: Rule
ruleEndOrBeginningOfYear = Rule
  { name = "at the beginning|end of <year>"
  , pattern =
    [ regex "(at the )?(beginning|end) of"
    , Predicate $ isGrainOfTime TG.Year
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "beginning" -> Just (1, 4)
          "end"       -> Just (9, -1)
          _           -> Nothing
        start <- intersect td $ month sd
        end <- if ed /= -1
          then intersect td $ cycleLastOf TG.Month $ month ed
          else cycleNthAfter False TG.Year 1 <$> intersect td (month 1)
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOfYear :: Rule
ruleEndOfYear = Rule
  { name = "end of year"
  , pattern = [ regex "(by (the )?|(at )?the )?(EOY|end of (the )?year)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        start <- std
        end <- intersect (month 1) $ cycleYear 1
        Token Time <$> interval TTime.Open start end
          where
            std = if "by" `Text.isPrefixOf` Text.toLower match
              then Just now
              else intersect (month 9) $ cycleYear 0
            cycleYear = cycleNth TG.Year
      _ -> Nothing
  }

ruleBeginningOfYear :: Rule
ruleBeginningOfYear = Rule
  { name = "beginning of year"
  , pattern = [ regex "((at )?the )?(BOY|beginning of (the )?year)" ]
  , prod = \_ -> do
      start <- intersect (month 1) $ cycleNth TG.Year 0
      end <- intersect (month 4) $ cycleNth TG.Year 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfWeek :: Rule
ruleEndOrBeginningOfWeek = Rule
  { name = "at the beginning|end of <week>"
  , pattern =
    [ regex "(at the )?(beginning|end) of"
    , Predicate $ isGrainOfTime TG.Week
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match1:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match1 of
          "beginning" -> Just (1, 3)
          "end"       -> Just (5, 7)
          _           -> Nothing
        start <- intersect td $ dayOfWeek sd
        end <- intersect td $ dayOfWeek ed
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "Africa Day", "africa(n (freedom|liberation))? day", monthDay 5 25 )
  , ( "Africa Industrialization Day", "africa industrialization day", monthDay 11 20 )
  , ( "All Saints' Day", "all saints' day", monthDay 11 1 )
  , ( "All Souls' Day", "all souls' day", monthDay 11 2 )
  , ( "April Fools", "(april|all) fool'?s('? day)?", monthDay 4 1 )
  , ( "Arabic Language Day", "arabic language day", monthDay 12 18 )
  , ( "Assumption of Mary", "assumption of mary", monthDay 8 15 )
  , ( "Boxing Day", "boxing day", monthDay 12 26 )
  , ( "Chinese Language Day", "chinese language day", monthDay 4 20 )
  , ( "Christmas", "(xmas|christmas)( day)?", monthDay 12 25 )
  , ( "Christmas Eve", "(xmas|christmas)( day)?('s)? eve", monthDay 12 24 )
  , ( "Day of Remembrance for all Victims of Chemical Warfare", "day of remembrance for all victims of chemical warfare", monthDay 4 29 )
  , ( "Day of Remembrance of the Victims of the Rwanda Genocide", "day of remembrance of the victims of the rwanda genocide", monthDay 4 7 )
  , ( "Day of the Seafarer", "day of the seafarer", monthDay 6 25 )
  , ( "Earth Day", "earth day", monthDay 4 22 )
  , ( "English Language Day", "english language day", monthDay 4 23 )
  , ( "Epiphany", "Epiphany", monthDay 1 6 )
  , ( "Feast of St Francis of Assisi", "feast of st\\.? francis of assisi", monthDay 10 4 )
  , ( "Feast of the Immaculate Conception", "feast of the immaculate conception", monthDay 12 8 )
  , ( "Global Day of Parents", "global day of parents", monthDay 6 1 )
  , ( "Halloween", "hall?owe?en( day)?", monthDay 10 31 )
  , ( "Human Rights Day", "human rights? day", monthDay 12 10 )
  , ( "International Albinism Awareness Day", "international albinism awareness day", monthDay 6 13 )
  , ( "International Anti-Corruption Day", "international anti(\\-|\\s)corruption day", monthDay 12 9 )
  , ( "International Asteroid Day", "international asteroid day", monthDay 6 30 )
  , ( "International Celebrate Bisexuality Day", "international celebrate bisexuality day", monthDay 9 23 )
  , ( "International Chernobyl Disaster Remembrance Day", "international chernobyl disaster remembrance day", monthDay 4 26 )
  , ( "International Civil Aviation Day", "international civil aviation day", monthDay 12 7 )
  , ( "International Customs Day", "international customs day", monthDay 1 26 )
  , ( "International Day Against Drug Abuse and Illicit Trafficking", "international day against drug abuse and illicit trafficking", monthDay 6 26 )
  , ( "International Day against Nuclear Tests", "international day against nuclear tests", monthDay 8 29 )
  , ( "International Day for Biological Diversity", "international day for biological diversity|world biodiversity day", monthDay 5 22 )
  , ( "International Day for Monuments and Sites", "international day for monuments and sites", monthDay 4 18 )
  , ( "International Day for Preventing the Exploitation of the Environment in War and Armed Conflict", "international day for preventing the exploitation of the environment in war and armed conflict", monthDay 11 6 )
  , ( "International Day for South-South Cooperation", "international day for south(\\-|\\s)south cooperation", monthDay 9 12 )
  , ( "International Day for Tolerance", "international day for tolerance", monthDay 11 16 )
  , ( "International Day for the Abolition of Slavery", "international day for the abolition of slavery", monthDay 12 2 )
  , ( "International Day for the Elimination of Racial Discrimination", "international day for the elimination of racial discrimination", monthDay 3 21 )
  , ( "International Day for the Elimination of Sexual Violence in Conflict", "international day for the elimination of sexual violence in conflict", monthDay 6 19 )
  , ( "International Day for the Elimination of Violence against Women", "international day for the elimination of violence against women", monthDay 11 25 )
  , ( "International Day for the Eradication of Poverty", "international day for the eradication of poverty", monthDay 10 17 )
  , ( "International Day for the Preservation of the Ozone Layer", "international day for the preservation of the ozone Layer", monthDay 9 16 )
  , ( "International Day for the Remembrance of the Slave Trade and its Abolition", "international day for the remembrance of the slave trade and its abolition", monthDay 8 23 )
  , ( "International Day for the Right to the Truth concerning Gross Human Rights Violations and for the Dignity of Victims", "international day for the right to the truth concerning gross human rights violations and for the dignity of victims", monthDay 3 24 )
  , ( "International Day for the Total Elimination of Nuclear Weapons", "international day for the total elimination of nuclear weapons", monthDay 9 26 )
  , ( "International Day in Support of Victims of Torture", "international day in support of victims of torture", monthDay 6 26 )
  , ( "International Day of Charity", "international day of charity", monthDay 9 5 )
  , ( "International Day of Commemoration in Memory of the Victims of the Holocaust", "international day of commemoration in memory of the victims of the holocaust", monthDay 1 27 )
  , ( "International Day of Democracy", "international day of democracy", monthDay 9 15 )
  , ( "International Day of Disabled Persons", "international day of disabled persons", monthDay 12 3 )
  , ( "International Day of Families", "international day of families", monthDay 5 15 )
  , ( "International Day of Family Remittances", "international day of family remittances", monthDay 6 16 )
  , ( "International Day of Forests", "international day of forests", monthDay 3 21 )
  , ( "International Day of Friendship", "international day of friendship", monthDay 7 30 )
  , ( "International Day of Happiness", "international day of happiness", monthDay 3 20 )
  , ( "International Day of Human Space Flight", "international day of human space flight", monthDay 4 12 )
  , ( "International Day of Innocent Children Victims of Aggression", "international day of innocent children victims of aggression", monthDay 6 4 )
  , ( "International Day of Non-Violence", "international day of non(\\-|\\s)violence", monthDay 10 2 )
  , ( "International Day of Nowruz", "international day of nowruz", monthDay 3 21 )
  , ( "International Day of Older Persons", "international day of older persons", monthDay 10 1 )
  , ( "International Day of Peace", "international day of peace", monthDay 9 21 )
  , ( "International Day of Persons with Disabilities", "international day of persons with disabilities", monthDay 12 3 )
  , ( "International Day of Remembrance of Slavery Victims and the Transatlantic Slave Trade", "international day of remembrance of slavery victims and the transatlantic slave trade", monthDay 3 25 )
  , ( "International Day of Rural Women", "international day of rural women", monthDay 10 15 )
  , ( "International Day of Solidarity with Detained and Missing Staff Members", "international day of solidarity with detained and missing staff members", monthDay 3 25 )
  , ( "International Day of Solidarity with the Palestinian People", "international day of solidarity with the palestinian people", monthDay 11 29 )
  , ( "International Day of Sport for Development and Peace", "international day of sport for development and peace", monthDay 4 6 )
  , ( "International Day of United Nations Peacekeepers", "international day of united nations peacekeepers", monthDay 5 29 )
  , ( "International Day of Women and Girls in Science", "international day of women and girls in science", monthDay 2 11 )
  , ( "International Day of Yoga", "international day of yoga", monthDay 6 21 )
  , ( "International Day of Zero Tolerance for Female Genital Mutilation", "international day of zero tolerance for female genital mutilation", monthDay 2 6 )
  , ( "International Day of the Girl Child", "international day of the girl child", monthDay 10 11 )
  , ( "International Day of the Victims of Enforced Disappearances", "international day of the victims of enforced disappearances", monthDay 8 30 )
  , ( "International Day of the World's Indigenous People", "international day of the world'?s indigenous people", monthDay 8 9 )
  , ( "International Day to End Impunity for Crimes against Journalists", "international day to end impunity for crimes against journalists", monthDay 11 2 )
  , ( "International Day to End Obstetric Fistula", "international day to end obstetric fistula", monthDay 5 23 )
  , ( "International Day for Disaster Reduction", "iddr|international day for (natural )?disaster reduction", monthDay 10 13 )
  , ( "International Human Solidarity Day", "international human solidarity day", monthDay 12 20 )
  , ( "International Jazz Day", "international jazz day", monthDay 4 30 )
  , ( "International Literacy Day", "international literacy day", monthDay 9 8 )
  , ( "International Men's Day", "international men'?s day", monthDay 11 19 )
  , ( "International Migrants Day", "international migrants day", monthDay 12 18 )
  , ( "International Mother Language Day", "international mother language day", monthDay 2 21 )
  , ( "International Mountain Day", "international mountain day", monthDay 12 11 )
  , ( "International Nurses Day", "international nurses day", monthDay 5 12 )
  , ( "International Overdose Awareness Day", "international overdose awareness day", monthDay 8 31 )
  , ( "International Volunteer Day for Economic and Social Development", "international volunteer day for economic and social development", monthDay 12 5 )
  , ( "International Widows' Day", "international widows'? day", monthDay 6 23 )
  , ( "International Women's Day", "international women'?s day", monthDay 3 8 )
  , ( "International Youth Day", "international youth day", monthDay 8 12 )
  , ( "May Day", "may day", monthDay 5 1 )
  , ( "Nelson Mandela Day", "nelson mandela day", monthDay 7 18 )
  , ( "New Year's Day", "new year'?s?( day)?", monthDay  1  1 )
  , ( "New Year's Eve", "new year'?s? eve", monthDay 12 31 )
  , ( "Orthodox Christmas Day", "orthodox christmas day", monthDay 1 7 )
  , ( "Orthodox New Year", "orthodox new year", monthDay 1 14 )
  , ( "Public Service Day", "public service day", monthDay 6 23 )
  , ( "St. George's Day", "(saint|st\\.?) george'?s day|feast of saint george", monthDay 4 23 )
  , ( "St Patrick's Day", "st\\.? (patrick|paddy)'?s day", monthDay 3 17 )
  , ( "St. Stephen's Day", "st\\.? stephen'?s day", monthDay 12 26 )
  , ( "Time of Remembrance and Reconciliation for Those Who Lost Their Lives during the Second World War", "time of remembrance and reconciliation for those who lost their lives during the second world war", monthDay 5 8 )
  , ( "United Nations Day", "united nations day", monthDay 10 24 )
  , ( "United Nations' Mine Awareness Day", "united nations'? mine awareness day", monthDay 4 4 )
  , ( "United Nations' World Health Day", "united nations'? world health day", monthDay 4 7 )
  , ( "Universal Children's Day", "universal children'?s day", monthDay 11 20 )
  , ( "Valentine's Day", "valentine'?s?( day)?", monthDay 2 14 )
  , ( "World AIDS Day", "world aids day", monthDay 12 1 )
  , ( "World Autism Awareness Day", "world autism awareness day", monthDay 4 2 )
  , ( "World Autoimmune Arthritis Day", "world autoimmune arthritis day", monthDay 5 20 )
  , ( "World Blood Donor Day", "world blood donor day", monthDay 6 14 )
  , ( "World Book and Copyright Day", "world book and copyright day", monthDay 4 23 )
  , ( "World Braille Day", "world braille day", monthDay 1 4 )
  , ( "World Cancer Day", "world cancer day", monthDay 2 4 )
  , ( "World Cities Day", "world cities day", monthDay 10 31 )
  , ( "World CP Day", "world (cerebral palsy| cp) day", monthDay 10 6 )
  , ( "World Day Against Child Labour", "world day against child labour", monthDay 6 12 )
  , ( "World Day against Trafficking in Persons", "world day against trafficking in persons", monthDay 7 30 )
  , ( "World Day for Audiovisual Heritage", "world day for audiovisual heritage", monthDay 10 27 )
  , ( "World Day for Cultural Diversity for Dialogue and Development", "world day for cultural diversity for dialogue and development", monthDay 5 21 )
  , ( "World Day for Safety and Health at Work", "world day for safety and health at work", monthDay 4 28 )
  , ( "World Day for the Abolition of Slavery", "world day for the abolition of slavery", monthDay 12 2 )
  , ( "World Day of Social Justice", "world day of social justice", monthDay 2 20 )
  , ( "World Day of the Sick", "world day of the sick", monthDay 2 11 )
  , ( "World Day to Combat Desertification and Drought", "world day to combat desertification and drought", monthDay 6 17 )
  , ( "World Development Information Day", "world development information day", monthDay 10 24 )
  , ( "World Diabetes Day", "world diabetes day", monthDay 11 14 )
  , ( "World Down Syndrome Day", "world down syndrome day", monthDay 3 21 )
  , ( "World Elder Abuse Awareness Day", "world elder abuse awareness day", monthDay 6 15 )
  , ( "World Environment Day", "world environment day", monthDay 6 5 )
  , ( "World Food Day", "world food day", monthDay 10 16 )
  , ( "World Genocide Commemoration Day", "world genocide commemoration day", monthDay 12 9 )
  , ( "World Heart Day", "world heart day", monthDay 9 29 )
  , ( "World Hepatitis Day", "world hepatitis day", monthDay 7 28 )
  , ( "World Humanitarian Day", "world humanitarian day", monthDay 8 19 )
  , ( "World Information Society Day", "world information society day", monthDay 5 17 )
  , ( "World Intellectual Property Day", "world intellectual property day", monthDay 4 26 )
  , ( "World Malaria Day", "world malaria day", monthDay 4 25 )
  , ( "World Mental Health Day", "world mental health day", monthDay 10 10 )
  , ( "World Meteorological Day", "world meteorological day", monthDay 3 23 )
  , ( "World No Tobacco Day", "world no tobacco day", monthDay 5 31 )
  , ( "World Oceans Day", "world oceans day", monthDay 6 8 )
  , ( "World Ovarian Cancer Day", "world ovarian cancer day", monthDay 5 8 )
  , ( "World Pneumonia Day", "world pneumonia day", monthDay 11 12 )
  , ( "World Poetry Day", "world poetry day", monthDay 3 21 )
  , ( "World Population Day", "world population day", monthDay 7 11 )
  , ( "World Post Day", "world post day", monthDay 10 9 )
  , ( "World Prematurity Day", "world prematurity day", monthDay 11 17 )
  , ( "World Press Freedom Day", "world press freedom day", monthDay 5 3 )
  , ( "World Rabies Day", "world rabies day", monthDay 9 28 )
  , ( "World Radio Day", "world radio day", monthDay 2 13 )
  , ( "World Refugee Day", "world refugee day", monthDay 6 20 )
  , ( "World Science Day for Peace and Development", "world science day for peace and development", monthDay 11 10 )
  , ( "World Sexual Health Day", "world sexual health day", monthDay 9 4 )
  , ( "World Soil Day", "world soil day", monthDay 12 5 )
  , ( "World Stroke Day", "world stroke day", monthDay 10 29 )
  , ( "World Suicide Prevention Day", "world suicide prevention day", monthDay 9 10 )
  , ( "World Teachers' Day", "world teachers'? day", monthDay 10 5 )
  , ( "World Television Day", "world television day", monthDay 11 21 )
  , ( "World Toilet Day", "world toilet day", monthDay 11 19 )
  , ( "World Tourism Day", "world tourism day", monthDay 9 27 )
  , ( "World Tuberculosis Day", "world tuberculosis day", monthDay 3 24 )
  , ( "World Tuna Day", "world tuna day", monthDay 5 2 )
  , ( "World Vegan Day", "world vegan day", monthDay 11 1 )
  , ( "World Vegetarian Day", "world vegetarian day", monthDay 10 1 )
  , ( "World Water Day", "world water day", monthDay 3 22 )
  , ( "World Wetlands Day", "world wetlands day", monthDay 2 2 )
  , ( "World Wildlife Day", "world wildlife day", monthDay 3 3 )
  , ( "World Youth Skills Day", "world youth skills day", monthDay 7 15 )
  , ( "Zero Discrimination Day", "zero discrimination day", monthDay 3 1 )

  -- Fixed day/week/month, year over year
  , ( "Commonwealth Day", "commonwealth day", nthDOWOfMonth 2 1 3 )
  , ( "Day of Remembrance for Road Traffic Victims"
    , "(world )?day of remembrance for road traffic victims"
    , nthDOWOfMonth 3 7 11 )
  , ( "International Day of Cooperatives"
    , "international day of co\\-?operatives", nthDOWOfMonth 1 6 7 )
  , ( "Martin Luther King's Day"
    , "(MLK|Martin Luther King('?s)?,?)( Jr\\.?| Junior)? day|(civil|idaho human) rights day"
    , nthDOWOfMonth 3 1 1
    )

  -- The day after Thanksgiving (not always the fourth Friday of November)
  , ( "Black Friday", "black frid?day"
    , cycleNthAfter False TG.Day 1 $ nthDOWOfMonth 4 4 11
    )
  , ( "World Habitat Day", "world habitat day", nthDOWOfMonth 1 1 10 )
  , ( "World Kidney Day", "world kidney day", nthDOWOfMonth 2 4 3 )
  , ( "World Leprosy Day", "world leprosy day"
    , predLastOf (dayOfWeek 7) (month 1) )
  , ( "World Maritime Day", "world maritime day"
    , predLastOf (dayOfWeek 4) (month 9) )
  , ( "World Migratory Bird Day", "world migratory bird day"
    , nthDOWOfMonth 2 6 5 )
  , ( "World Philosophy Day", "world philosophy day", nthDOWOfMonth 3 4 11 )
  , ( "World Religion Day", "world religion day", nthDOWOfMonth 3 7 1 )
  , ( "World Sight Day", "world sight day", nthDOWOfMonth 2 4 10 )

  -- Other
  , ( "Boss's Day", "boss'?s?( day)?"
    , predNthClosest 0 weekday (monthDay 10 16) )
  ]

ruleComputedHolidays :: [Rule]
ruleComputedHolidays = mkRuleHolidays
  [ ( "Ascension Day", "ascension\\s+(thurs)?day"
    , cycleNthAfter False TG.Day 39 easterSunday )
  , ( "Ash Wednesday", "ash\\s+wednesday|carnival"
    , cycleNthAfter False TG.Day (-46) easterSunday )
  , ( "Ashura", "(day of )?ashura"
    , cycleNthAfter False TG.Day 9 muharram )
  , ( "Bhai Dooj", "bhai(ya)?\\s+d(u|oo)j|bhau\\-beej|bhai\\s+(tika|phonta)"
    , cycleNthAfter False TG.Day 4 dhanteras )
  -- 6th day after Diwali
  , ( "Chhath", "chhathi?|chhath (parv|puja)|dala (chhath|puja)|surya shashthi"
    , cycleNthAfter False TG.Day 8 dhanteras )
  , ( "Boghi", "boghi|bogi\\s+pandigai"
    , cycleNthAfter False TG.Day (-1) thaiPongal )
  , ( "Chinese New Year", "chinese\\s+(lunar\\s+)?new\\s+year('?s(\\s+day)?)?"
    , chineseNewYear )
  , ( "Clean Monday"
    , "(orthodox\\s+)?(ash|clean|green|pure|shrove)\\s+monday|monday of lent"
    , cycleNthAfter False TG.Day (-48) orthodoxEaster )
  , ( "Corpus Christi", "(the feast of )?corpus\\s+christi"
    , cycleNthAfter False TG.Day 60 easterSunday )
  , ( "Dhanteras", "dhanatrayodashi|dhanteras|dhanvantari\\s+trayodashi"
    , dhanteras )
  , ( "Diwali", "deepavali|diwali|lakshmi\\s+puja"
    , cycleNthAfter False TG.Day 2 dhanteras )
  , ( "Durga Ashtami", "(durga|maha)(\\s+a)?shtami"
    , cycleNthAfter False TG.Day 7 navaratri )
  , ( "Easter Monday", "easter\\s+mon(day)?"
    , cycleNthAfter False TG.Day 1 easterSunday )
  , ( "Easter Sunday", "easter(\\s+sun(day)?)?", easterSunday )
  , ( "Eid al-Adha", "bakr[\\-\\s]e?id|e?id [au]l\\-adha|sacrifice feast"
    , eidalAdha )
  , ( "Eid al-Fitr", "eid al\\-fitr", eidalFitr )
  , ( "Govardhan Puja", "govardhan\\s+puja|annak(u|oo)t"
    , cycleNthAfter False TG.Day 3 dhanteras )
  , ( "Good Friday", "(good|great|holy)\\s+fri(day)?"
    , cycleNthAfter False TG.Day (-2) easterSunday )
  , ( "Guru Gobind Singh Jayanti"
    , "guru\\s+(gobind|govind)\\s+singh\\s+(birthday|jayanti)"
    , guruGobindSinghJayanti )
  , ( "Holi", "(rangwali )?holi|dhuleti|dhulandi|phagwah"
    , cycleNthAfter False TG.Day 39 vasantPanchami )
  , ( "Holika Dahan", "holika dahan|kamudu pyre|chhoti holi"
    , cycleNthAfter False TG.Day 38 vasantPanchami )
  , ( "Holy Saturday"
    , "(black|holy (and great )?|joyous)sat(urday)?|the great sabbath|easter eve"
    , cycleNthAfter False TG.Day (-1) easterSunday )
  , ( "Islamic New Year", "(arabic|hijri|islamic) new year|amun jadid|muharram"
    , muharram )
  , ( "Isra and Mi'raj"
    , "isra and mi'raj|(the )?prophet'?s'? ascension|(the )?ascension to heaven|the night journey"
    , cycleNthAfter False TG.Day 26 rajab
    )
  , ( "Jumu'atul-Wida", "jumu'atul\\-widaa?'?|jamat[\\-\\s]ul[\\-\\s]vida"
    , predNthAfter (-1) (dayOfWeek 5) eidalFitr )
  , ( "Kaanum Pongal", "(kaanum|kanni)\\s+pongal"
    , cycleNthAfter False TG.Day 2 thaiPongal )
  , ( "Lag BaOmer", "lag b[a']omer", lagBaOmer )
  , ( "Vaisakhi", "mesadi|[bv]aisakhi|vaisakhadi|vasakhi|vaishakhi", vaisakhi)
  , ( "Laylat al-Qadr"
    , "laylat al[\\-\\s][qk]adr|night of (destiny|measures|power|value)"
    , cycleNthAfter False TG.Day 26 ramadan )
  , ( "Lazarus Saturday", "lazarus\\s+saturday"
    , cycleNthAfter False TG.Day (-8) orthodoxEaster )
  , ( "Maha Navami", "maha\\s+navami", cycleNthAfter False TG.Day 8 navaratri )
  , ( "Maha Saptami", "maha\\s+saptami", cycleNthAfter False TG.Day 6 navaratri )
  , ( "Mattu Pongal", "maa?ttu\\s+pongal"
    , cycleNthAfter False TG.Day 1 thaiPongal )
  , ( "Maundy Thursday"
    , "(covenant|(great and )?holy|maundy|sheer)\\s+thu(rsday)?|thu(rsday)? of mysteries"
    , cycleNthAfter False TG.Day (-3) easterSunday )
  , ( "Mawlid"
    , "mawlid(\\s+al\\-nab(awi|i\\s+al\\-sharif))?|mevli[dt]|mulud|birth(day)? of (the )?prophet( muhammad)?|(the )?prophet's birthday"
    , mawlid )
  , ( "Naraka Chaturdashi"
    , "naraka? (nivaran )?chaturdashi|(kali|roop) chaudas|choti diwali"
    , cycleNthAfter False TG.Day 1 dhanteras )
  , ( "Orthodox Easter Monday", "orthodox\\s+easter\\s+mon(day)?"
    , cycleNthAfter False TG.Day 1 orthodoxEaster )
  , ( "Orthodox Easter Sunday", "orthodox\\s+easter(\\s+sun(day)?)?|pascha?"
    , orthodoxEaster )
  , ( "Orthodox Holy Saturday", "orthodox\\s+holy\\s+sat(urday)?|the great sabbath"
    , cycleNthAfter False TG.Day (-1) orthodoxEaster )
  , ( "Orthodox Great Friday", "orthodox\\s+great(\\s+and\\s+holy)?\\s+friday"
    , cycleNthAfter False TG.Day (-2) orthodoxEaster )
  , ( "Orthodox Palm Sunday", "orthodox\\s+(branch|palm|yew)\\s+sunday"
    , cycleNthAfter False TG.Day (-7) orthodoxEaster )
  , ( "Palm Sunday", "(branch|palm|yew)\\s+sunday"
    , cycleNthAfter False TG.Day (-7) easterSunday )
  , ( "Pentecost", "pentecost|white sunday|whitsunday"
    , cycleNthAfter False TG.Day 49 easterSunday )
  , ( "Purim", "purim", purim )
  , ( "Raksha Bandhan", "raksha(\\s+)?bandhan|rakhi", rakshaBandhan )
  , ( "Dayananda Saraswati Jayanti","((maharishi|swami) )?(dayananda )?saraswati jayanti", saraswatiJayanti )
  , ( "Krishna Janmashtami", "(krishna )?janmashtami|gokulashtami", krishnaJanmashtami )
  , ( "Shemini Atzeret", "shemini\\s+atzeret"
    , cycleNthAfter False TG.Day 21 roshHashana )
  , ( "Shrove Tuesday", "pancake (tues)?day|shrove tuesday|mardi gras"
    , cycleNthAfter False TG.Day (-47) easterSunday )
  , ( "Shushan Purim", "shushan\\s+purim", cycleNthAfter False TG.Day 1 purim )
  , ( "Simchat Torah", "simc?hat\\s+torah"
    , cycleNthAfter False TG.Day 22 roshHashana )
  , ( "Thai Pongal"
    , "(thai )?pongal|pongal pandigai|(makara? |magha )?sankranth?i|maghi"
    , thaiPongal )
  , ( "Thiru Onam", "(thiru(v|\\s+))?onam", thiruOnam )
  , ( "Tisha B'Av", "tisha b'av", tishaBAv )
  , ( "Trinity Sunday", "trinity\\s+sunday"
    , cycleNthAfter False TG.Day 56 easterSunday )
  , ( "Vasant Panchami", "[bv]asant\\s+panchami", vasantPanchami )
  , ( "Vijayadashami", "dasara|duss(eh|he)ra|vijayadashami"
    , cycleNthAfter False TG.Day 9 navaratri )
  -- 15th day of Shevat
  , ( "Tu BiShvat", "tu b[i']shvat", tuBishvat )
  -- day of the full moon in May in the Gregorian calendar
  , ( "Vesak", "v(e|ai)sak(ha)?|buddha (day|purnima)", vesak )
  , ( "Yom Ha'atzmaut", "yom ha'?atzmaut", yomHaatzmaut )
  , ( "Yom HaShoah"
    , "yom hashoah|yom hazikaron lashoah ve-lag'vurah|holocaust (remembrance )?day"
    , cycleNthAfter False TG.Day 12 passover )
  , ( "Yom Kippur", "yom\\s+kippur", cycleNthAfter False TG.Day 9 roshHashana )
  , ( "Whit Monday", "(pentecost|whit)\\s+monday|monday of the holy spirit"
    , cycleNthAfter False TG.Day 50 easterSunday )
  -- Rabindra Jayanti 25th day of the Bengali month of Boishakh
  , ( "Rabindra Jayanti", "rabindra(nath)?\\s+jayanti", rabindraJayanti )
  , ("Guru Ravidass Jayanti", "guru\\s+ravidass?\\s+(birthday|jayanti)"
    , ravidassJayanti )
  ]

ruleComputedHolidays' :: [Rule]
ruleComputedHolidays' = mkRuleHolidays'
  [ ( "Global Youth Service Day", "global youth service day|gysd"
    , let start = globalYouthServiceDay
          end = cycleNthAfter False TG.Day 2 globalYouthServiceDay
        in interval TTime.Open start end )
  , ( "Great Lent", "great\\s+(fast|lent)"
    , let start = cycleNthAfter False TG.Day (-48) orthodoxEaster
          end = cycleNthAfter False TG.Day (-9) orthodoxEaster
        in interval TTime.Open start end )
  , ( "Hanukkah", "c?hann?ukk?ah"
    , let start = chanukah
          end = cycleNthAfter False TG.Day 7 chanukah
        in interval TTime.Open start end )
  , ( "Lent", "lent"
    , let start = cycleNthAfter False TG.Day (-46) easterSunday
          end = cycleNthAfter False TG.Day (-1) easterSunday
        in interval TTime.Open start end )
  , ( "Navaratri", "durga\\s+puja|durgotsava|nava?rath?ri"
    , let start = navaratri
          end = cycleNthAfter False TG.Day 9 navaratri
        in interval TTime.Open start end )
  , ( "Passover", "passover|pesa[ck]?h"
    , let start = passover
          end = cycleNthAfter False TG.Day 8 passover
        in interval TTime.Open start end )
  , ( "Ramadan", "rama[dt]h?an|ramzaa?n"
    , let start = ramadan
          end = cycleNthAfter False TG.Day (-1) eidalFitr
        in interval TTime.Open start end )
  , ( "Rosh Hashanah", "rosh hashann?ah?|yom teruah"
    , let start = roshHashana
          end = cycleNthAfter False TG.Day 2 roshHashana
        in interval TTime.Open start end )
  , ( "Shavuot", "feast of weeks|shavu'?oth?|shovuos"
    , let start = cycleNthAfter False TG.Day 50 passover
          end = cycleNthAfter False TG.Day 52 passover
        in interval TTime.Open start end )
  , ( "Sukkot", "feast of (booths|tabernacles|the ingathering)|su[ck]{2}o[st]"
    , let start = cycleNthAfter False TG.Day 14 roshHashana
          end = cycleNthAfter False TG.Day 22 roshHashana
        in interval TTime.Open start end )

  -- Other
  -- Last Saturday of March unless it falls on Holy Saturday
  -- In which case it's the Saturday before
  , ( "Earth Hour", "earth hour"
    , let holySaturday = cycleNthAfter False TG.Day (-1) easterSunday
          tentative = predLastOf (dayOfWeek 6) (month 3)
          alternative = cycleNthAfter False TG.Day (-7) tentative
        in do
          day <- intersectWithReplacement holySaturday tentative alternative
          start <- intersect day $ hourMinute True 20 30
          interval TTime.Closed start $ cycleNthAfter False TG.Minute 60 start )
  -- Does not account for leap years, so every 365 days.
  , ( "Parsi New Year", "parsi new year|jamshedi navroz"
    , predEveryNDaysFrom 365 (2020, 8, 16)
    )
  -- king's day is on April 27th unless its on Sunday in which case its a day
  -- earlier used a bit of a trick since intersectWithReplacement expects all
  -- times to be aligned in granularity (e.g once a week, twice a year, etc.)
  -- we intersect with last Sunday of April since if "4/27 is on Sunday" it
  -- will be the last Sunday on April
  , ( "King's Day", "king's day|koningsdag"
    , let tentative = monthDay 4 27
          alternative = monthDay 4 26
          lastSundayOfApril = predLastOf (dayOfWeek 7) (month 4)
        in intersectWithReplacement lastSundayOfApril tentative alternative )
  ]

ruleCycleThisLastNext :: Rule
ruleCycleThisLastNext = Rule
  { name = "this|last|next <cycle>"
  , pattern =
    [ regex "(this|current|coming|next|the following|last|past|previous)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token TimeGrain grain:_) ->
        case Text.toLower match of
          "this"          -> tt $ cycleNth grain 0
          "coming"        -> tt $ cycleNth grain 0
          "current"       -> tt $ cycleNth grain 0
          "last"          -> tt . cycleNth grain $ - 1
          "past"          -> tt . cycleNth grain $ - 1
          "previous"      -> tt . cycleNth grain $ - 1
          "next"          -> tt $ cycleNth grain 1
          "the following" -> tt $ cycleNth grain 1
          _ -> Nothing
      _ -> Nothing
  }

ruleDOMOfTimeMonth :: Rule
ruleDOMOfTimeMonth = Rule
  { name = "<day-of-month> (ordinal or number) of <month>"
  , pattern =
    [ Predicate isDOMValue
    , regex "of( the)?"
    , Predicate $ isGrainOfTime TG.Month
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleCycleTheAfterBeforeTime :: Rule
ruleCycleTheAfterBeforeTime = Rule
  { name = "the <cycle> after|before <time>"
  , pattern =
    [ regex "the"
    , dimension TimeGrain
    , regex "(after|before)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (  _
       : Token TimeGrain grain
       : Token RegexMatch (GroupMatch (match:_))
       : Token Time td
       : _) ->
        let n = if Text.toLower match == "after" then 1 else - 1 in
          tt $ cycleNthAfter False grain n td
      _ -> Nothing
  }

ruleCycleAfterBeforeTime :: Rule
ruleCycleAfterBeforeTime = Rule
  { name = "<cycle> after|before <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "(after|before)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) ->
        let n = if Text.toLower match == "after" then 1 else - 1 in
          tt $ cycleNthAfter False grain n td
      _ -> Nothing
  }

ruleCycleOrdinalOfTime :: Rule
ruleCycleOrdinalOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "of|in|from"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleLastOrdinalOfTime :: Rule
ruleCycleLastOrdinalOfTime = Rule
  { name = "<ordinal> last <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , regex "last"
    , dimension TimeGrain
    , regex "of|in|from"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True grain (-n) . cycleNthAfter True (timeGrain td) 1 $ td
      _ -> Nothing
  }

ruleCycleTheOrdinalOfTime :: Rule
ruleCycleTheOrdinalOfTime = Rule
  { name = "the <ordinal> <cycle> of <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "of|in|from"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleTheLastOrdinalOfTime :: Rule
ruleCycleTheLastOrdinalOfTime = Rule
  { name = "the <ordinal> last <cycle> of <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , regex "last"
    , dimension TimeGrain
    , regex "of|in|from"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:_:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True grain (-n) . cycleNthAfter True (timeGrain td) 1 $ td
      _ -> Nothing
  }

ruleCycleTheOfTime :: Rule
ruleCycleTheOfTime = Rule
  { name = "the <cycle> of <time>"
  , pattern =
    [ regex "the"
    , dimension TimeGrain
    , regex "of"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain 0 td
      _ -> Nothing
  }

ruleCycleOrdinalAfterTime :: Rule
ruleCycleOrdinalAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleTheOrdinalAfterTime :: Rule
ruleCycleTheOrdinalAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleOrdinalQuarter :: Rule
ruleCycleOrdinalQuarter = Rule
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

ruleCycleTheOrdinalQuarter :: Rule
ruleCycleTheOrdinalQuarter = Rule
  { name = "the <ordinal> quarter"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True TG.Quarter (n - 1) $
          cycleNth TG.Year 0
      _ -> Nothing
  }

ruleCycleOrdinalQuarterYear :: Rule
ruleCycleOrdinalQuarterYear = Rule
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

ruleDurationInWithinAfter :: Rule
ruleDurationInWithinAfter = Rule
  { name = "in|within|after <duration>"
  , pattern =
    [ regex "(in|within|after)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration dd:
       _) -> case Text.toLower match of
         "within" -> Token Time <$> interval TTime.Open now (inDuration dd)
         "after"  -> tt . withDirection TTime.After $ inDuration dd
         "in"     -> tt $ inDuration dd
         _        -> Nothing
      _ -> Nothing
  }

ruleDurationLastNext :: Rule
ruleDurationLastNext = Rule
  { name = "last|past|next <duration>"
  , pattern =
    [ regex "([lp]ast|next)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration DurationData{TDuration.grain, TDuration.value}:
       _) -> case Text.toLower match of
         "next" -> tt $ cycleN True grain value
         "last" -> tt $ cycleN True grain (- value)
         "past" -> tt $ cycleN True grain (- value)
         _      -> Nothing
      _ -> Nothing
  }

ruleNDOWago :: Rule
ruleNDOWago = Rule
  { name = "<integer> <named-day> ago|back"
  , pattern =
    [ Predicate isNatural
    , Predicate isADayOfWeek
    , regex "ago|back"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:Token Time td:_) ->
        tt $ predNth (- (floor v)) False td
      _ -> Nothing
  }

ruleDurationHenceAgo :: Rule
ruleDurationHenceAgo = Rule
  { name = "<duration> hence|ago"
  , pattern =
    [ dimension Duration
    , regex "(hence|ago)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
        "ago" -> tt $ durationAgo dd
        _     -> tt $ inDuration dd
      _ -> Nothing
  }

ruleDayDurationHenceAgo :: Rule
ruleDayDurationHenceAgo = Rule
  { name = "<day> <duration> hence|ago"
  , pattern =
    [ Predicate $ or . sequence [isGrainOfTime TG.Day, isGrainOfTime TG.Month]
    , dimension Duration
    , regex "(from now|hence|ago)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "ago" -> Token Time <$> intersect td (durationIntervalAgo dd)
         _     -> Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleDayInDuration :: Rule
ruleDayInDuration = Rule
  { name = "<day> in <duration>"
  , pattern =
    [ Predicate $ or . sequence [isGrainOfTime TG.Day, isGrainOfTime TG.Month]
    , regex "in"
    , Predicate $ isDurationGreaterThan TG.Hour
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:Token Duration dd:_) ->
        Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleInDurationAtTime :: Rule
ruleInDurationAtTime = Rule
  { name = "in <duration> at <time-of-day>"
  , pattern =
    [ regex "in"
    , Predicate $ isDurationGreaterThan TG.Hour
    , regex "at"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_:Token Time td:_) ->
        Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleInNumeral :: Rule
ruleInNumeral = Rule
  { name = "in <number> (implicit minutes)"
  , pattern =
    [ regex "in"
    , Predicate $ isIntegerBetween 0 60
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        tt . inDuration . duration TG.Minute $ floor v
      _ -> Nothing
  }

ruleDurationAfterBeforeTime :: Rule
ruleDurationAfterBeforeTime = Rule
  { name = "<duration> after|before|from|past <time>"
  , pattern =
    [ dimension Duration
    , regex "(after|before|from|past)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) -> case Text.toLower match of
         "before" -> tt $ durationBefore dd td
         _        -> tt $ durationAfter dd td
      _ -> Nothing
  }

ruleIntervalForDurationFrom :: Rule
ruleIntervalForDurationFrom = Rule
  { name = "for <duration> from <time>"
  , pattern =
    [ regex "for"
    , dimension Duration
    , regex "(from|starting|beginning|after|starting from)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_:Token Time td1:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleIntervalTimeForDuration :: Rule
ruleIntervalTimeForDuration = Rule
  { name = "<time> for <duration>"
  , pattern =
    [ Predicate isNotLatent
    , regex "for"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleIntervalFromTimeForDuration :: Rule
ruleIntervalFromTimeForDuration = Rule
  { name = "from <time> for <duration>"
  , pattern =
    [ regex "(from|starting|beginning|after|starting from)"
    , Predicate isNotLatent
    , regex "for"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
}

timezoneName :: String
timezoneName = "YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT"

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay, hasNoTimezone]
    , regex $ "\\b(" ++ timezoneName ++ ")\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleTimezoneBracket :: Rule
ruleTimezoneBracket = Rule
  { name = "<time> (timezone)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay, hasNoTimezone]
    , regex $ "\\((" ++ timezoneName ++ ")\\)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleIntervalDashTimezone :: Rule
ruleIntervalDashTimezone = Rule
  { name = "<datetime> - <datetime> (interval) timezone"
  , pattern =
    [ Predicate $ and . sequence [isATimeOfDay, hasNoTimezone]
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , Predicate $ and . sequence [isATimeOfDay, hasNoTimezone]
    , regex $ "\\b(" ++ timezoneName ++ ")\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:
       _:
       Token Time td2:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> do
        tdz1 <- inTimezone (Text.toUpper tz) td1
        tdz2 <- inTimezone (Text.toUpper tz) td2
        Token Time <$> interval TTime.Closed tdz1 tdz2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntersect
  , ruleIntersectOf
  , ruleIntersectYear
  , ruleAbsorbOnDay
  , ruleAbsorbOnADOW
  , ruleAbsorbInMonthYear
  , ruleAbsorbCommaTOD
  , ruleNextDOW
  , ruleNextTime
  , ruleThisTime
  , ruleLastTime
  , ruleTimeBeforeLastAfterNext
  , ruleLastDOWOfTime
  , ruleLastCycleOfTime
  , ruleLastNight
  , ruleLastWeekendOfMonth
  , ruleNthTimeOfTime
  , ruleTheNthTimeOfTime
  , ruleNthTimeAfterTime
  , ruleTheNthTimeAfterTime
  , ruleNDOWFromTime
  , ruleYearLatent
  , ruleYearADBC
  , ruleTheDOMNumeral
  , ruleTheDOMOrdinal
  , ruleDOMLatent
  , ruleNamedDOMOrdinal
  , ruleMonthDOMNumeral
  , ruleDOMMonth
  , ruleDOMOfMonth
  , ruleDOMOrdinalMonthYear
  , ruleDOMMonthYear
  , ruleIdesOfMonth
  , ruleTODLatent
  , ruleAtTOD
  , ruleTODOClock
  , ruleHHMM
  , ruleHHMMLatent
  , ruleHHMMSS
  , ruleMilitaryAMPM
  , ruleMilitarySpelledOutAMPM
  , ruleMilitarySpelledOutAMPM2
  , ruleTODAMPM
  , ruleHONumeral
  , ruleHODHalf
  , ruleHODQuarter
  , ruleNumeralToHOD
  , ruleHalfToHOD
  , ruleQuarterToHOD
  , ruleNumeralAfterHOD
  , ruleHalfAfterHOD
  , ruleQuarterAfterHOD
  , ruleHalfHOD
  , ruleYYYYQQ
  , ruleYYYYMM
  , ruleYYYYMMDD
  , ruleMMYYYY
  , ruleNoonMidnightEOD
  , rulePartOfDays
  , ruleEarlyMorning
  , rulePODIn
  , rulePODThis
  , ruleTonight
  , ruleAfterPartofday
  , ruleTimePOD
  , rulePODofTime
  , ruleWeekend
  , ruleWeek
  , ruleTODPrecision
  , rulePrecisionTOD
  , ruleIntervalFromMonthDDDD
  , ruleIntervalFromDDDDMonth
  , ruleIntervalFromDDDDOfMonth
  , ruleIntervalMonthDDDD
  , ruleIntervalDDDDMonth
  , ruleIntervalDash
  , ruleIntervalSlash
  , ruleIntervalFrom
  , ruleIntervalBetween
  , ruleIntervalTODDash
  , ruleIntervalTODFrom
  , ruleIntervalTODAMPM
  , ruleIntervalTODBetween
  , ruleIntervalBy
  , ruleIntervalByTheEndOf
  , ruleIntervalUntilTime
  , ruleIntervalAfterFromSinceTime
  , ruleCycleTheAfterBeforeTime
  , ruleCycleThisLastNext
  , ruleDOMOfTimeMonth
  , ruleCycleAfterBeforeTime
  , ruleCycleOrdinalOfTime
  , ruleCycleLastOrdinalOfTime
  , ruleCycleTheOrdinalOfTime
  , ruleCycleTheLastOrdinalOfTime
  , ruleCycleTheOfTime
  , ruleCycleOrdinalAfterTime
  , ruleCycleTheOrdinalAfterTime
  , ruleCycleOrdinalQuarter
  , ruleCycleTheOrdinalQuarter
  , ruleCycleOrdinalQuarterYear
  , ruleDurationInWithinAfter
  , ruleDurationLastNext
  , ruleNDOWago
  , ruleDurationHenceAgo
  , ruleDayDurationHenceAgo
  , ruleDayInDuration
  , ruleInDurationAtTime
  , ruleDurationAfterBeforeTime
  , ruleIntervalForDurationFrom
  , ruleIntervalFromTimeForDuration
  , ruleIntervalTimeForDuration
  , ruleInNumeral
  , ruleTimezone
  , ruleTimezoneBracket
  , ruleIntervalDashTimezone
  , rulePartOfMonth
  , ruleEndOrBeginningOfMonth
  , ruleEndOrBeginningOfYear
  , ruleEndOrBeginningOfWeek
  , ruleNow
  , ruleSeason
  , ruleEndOfMonth
  , ruleBeginningOfMonth
  , ruleEndOfYear
  , ruleBeginningOfYear
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ ruleComputedHolidays
  ++ ruleComputedHolidays'
  ++ rulePeriodicHolidays
