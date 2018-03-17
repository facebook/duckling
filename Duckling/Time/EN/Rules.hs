-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.Rules
  ( rules
  ) where

import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Duration.Types (DurationData (..))
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Time.Computed (easterSunday)
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
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
    , regex "of|from|for|'s|,"
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
    [ regex "on"
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
    [ regex "on a"
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
    [ regex "in"
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
  , ("end of month" , TG.Month , 1  , "(the )?(EOM|end of (the )?month)" )
  , ("end of year"  , TG.Year  , 1  , "(the )?(EOY|end of (the )?year)"  )
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
        tt . mkLatent $ hour True n
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
    , regex "(in the )?([ap])(\\s|\\.)?m?\\.?"
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
    , regex "half"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
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
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
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
        t <- minutesBefore n td
        Just $ Token Time t
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
        t <- minutesAfter n td
        Just $ Token Time t
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

ruleNoonMidnightEOD :: Rule
ruleNoonMidnightEOD = Rule
  { name = "noon|midnight|EOD|end of day"
  , pattern =
    [ regex "(noon|midni(ght|te)|(the )?(EOD|end of (the )?day))"
    ]
  , prod = \tokens -> case tokens of
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
        intersect (cycleNth TG.Day 0) td
      _ -> Nothing
  }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern = [regex "toni(ght|gth|te)s?"]
  , prod = \_ -> do
      let today = cycleNth TG.Day 0
      evening <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay . notLatent <$> intersect today evening
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
        Token Time . partOfDay . notLatent <$>
          intersect (cycleNth TG.Day 0) td
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
  { name = "from <day-of-month> (ordinal or number) to <day-of-month> (ordinal or number) <named-month> (interval)"
  , pattern =
    [ regex "from"
    , Predicate isDOMValue
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
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
      (_:Token Time td:_) ->
        Token Time <$> interval TTime.Open (cycleNth TG.Second 0) td
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
      (_:Token Time td:_) ->
        Token Time <$> interval TTime.Closed (cycleNth TG.Second 0) td
      _ -> Nothing
  }

ruleIntervalUntilTOD :: Rule
ruleIntervalUntilTOD = Rule
  { name = "until <time-of-day>"
  , pattern =
    [ regex "(anytime |sometimes? )?(before|(un)?til(l)?|through|up to)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleIntervalAfterFromSinceTOD :: Rule
ruleIntervalAfterFromSinceTOD = Rule
  { name = "from|since|after <time-of-day>"
  , pattern =
    [ regex "from|since|(anytime |sometimes? )?after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "monday|mon\\.?"         )
  , ( "Tuesday"  , "tuesday|tues?\\.?"      )
  , ( "Wednesday", "wed?nesday|wed\\.?"     )
  , ( "Thursday" , "thursday|thu(rs?)?\\.?" )
  , ( "Friday"   , "friday|fri\\.?"         )
  , ( "Saturday" , "saturday|sat\\.?"       )
  , ( "Sunday"   , "sunday|sun\\.?"         )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"  , "january|jan\\.?"     )
  , ( "February" , "february|feb\\.?"    )
  , ( "March"    , "march|mar\\.?"       )
  , ( "April"    , "april|apr\\.?"       )
  , ( "May"      , "may"                 )
  , ( "June"     , "june|jun\\.?"        )
  , ( "July"     , "july|jul\\.?"        )
  , ( "August"   , "august|aug\\.?"      )
  , ( "September", "september|sept?\\.?" )
  , ( "October"  , "october|oct\\.?"     )
  , ( "November" , "november|nov\\.?"    )
  , ( "December" , "december|dec\\.?"    )
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

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "Africa Industrialization Day", "africa industrialization day", monthDay 11 20 )
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
  , ( "Human Rights Day", "human rights day", monthDay 12 10 )
  , ( "International Albinism Awareness Day", "international albinism awareness day", monthDay 6 13 )
  , ( "International Anti-Corruption Day", "international anti(\\-|\\s)corruption day", monthDay 12 9 )
  , ( "International Asteroid Day", "international asteroid day", monthDay 6 30 )
  , ( "International Celebrate Bisexuality Day", "international celebrate bisexuality day", monthDay 9 23 )
  , ( "International Chernobyl Disaster Remembrance Day", "international chernobyl disaster remembrance day", monthDay 4 26 )
  , ( "International Civil Aviation Day", "international civil aviation day", monthDay 12 7 )
  , ( "International Customs Day", "international customs day", monthDay 1 26 )
  , ( "International Day Against Drug Abuse and Illicit Trafficking", "international day against drug abuse and illicit trafficking", monthDay 6 26 )
  , ( "International Day against Nuclear Tests", "international day against nuclear tests", monthDay 8 29 )
  , ( "International Day for Biological Diversity", "international day for biological diversity", monthDay 5 22 )
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
  , ( "St Patrick's Day", "st\\.? patrick'?s day", monthDay 3 17 )
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
  , ( "Martin Luther King's Day"
    , "(MLK|Martin Luther King,?)( Jr\\.?| Junior)? day"
    , nthDOWOfMonth 3 1 1
    )
  , ( "Father's Day"            , "father'?s?'? day", nthDOWOfMonth 3 7 6 )
  , ( "Mother's Day"            , "mother'?s?'? day", nthDOWOfMonth 2 7 5 )
  , ( "Labor Day"               , "labor day"       , nthDOWOfMonth 1 1 9 )

  -- The day after Thanksgiving (not always the fourth Friday of November)
  , ( "Black Friday", "black frid?day"
    , cycleNthAfter False TG.Day 1 $ nthDOWOfMonth 4 4 11
    )
  -- Long weekend before the first Monday of September
  , ( "Labor Day weekend", "labor day week(\\s|-)?ends?"
    , longWEBefore $ nthDOWOfMonth 1 1 9
    )
  ]

ruleComputedHolidays :: [Rule]
ruleComputedHolidays = mkRuleHolidays
  [ ( "Easter Sunday", "easter(\\s+sun(day)?)?", easterSunday)
  , ( "Easter Monday", "easter\\s+mon(day)?"
    , cycleNthAfter False TG.Day 1 easterSunday
    )
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
         "within" -> Token Time <$>
           interval TTime.Open (cycleNth TG.Second 0) (inDuration dd)
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
  { name = "<duration> after|before|from <time>"
  , pattern =
    [ dimension Duration
    , regex "(after|before|from)"
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
        Token Time <$> interval TTime.Open td1 (durationAfter dd td1)
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
  [ ruleIntersect
  , ruleIntersectOf
  , ruleAbsorbOnTime
  , ruleAbsorbOnADOW
  , ruleAbsorbInMonth
  , ruleAbsorbCommaTOD
  , ruleNextDOW
  , ruleNextTime
  , ruleThisTime
  , ruleLastTime
  , ruleTimeBeforeLastAfterNext
  , ruleLastDOWOfTime
  , ruleLastCycleOfTime
  , ruleLastWeekendOfMonth
  , ruleNthTimeOfTime
  , ruleTheNthTimeOfTime
  , ruleNthTimeAfterTime
  , ruleTheNthTimeAfterTime
  , ruleYear
  , ruleYearPastLatent
  , ruleYearFutureLatent
  , ruleTheDOMNumeral
  , ruleTheDOMOrdinal
  , ruleDOMLatent
  , ruleNamedDOMOrdinal
  , ruleMonthDOMNumeral
  , ruleDOMMonth
  , ruleDOMOfMonth
  , ruleDOMOrdinalMonthYear
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
  , ruleTODPrecision
  , rulePrecisionTOD
  , ruleIntervalFromMonthDDDD
  , ruleIntervalFromDDDDMonth
  , ruleIntervalMonthDDDD
  , ruleIntervalDDDDMonth
  , ruleIntervalDash
  , ruleIntervalFrom
  , ruleIntervalBetween
  , ruleIntervalTODDash
  , ruleIntervalTODFrom
  , ruleIntervalTODAMPM
  , ruleIntervalTODBetween
  , ruleIntervalBy
  , ruleIntervalByTheEndOf
  , ruleIntervalUntilTOD
  , ruleIntervalAfterFromSinceTOD
  , ruleCycleTheAfterBeforeTime
  , ruleCycleThisLastNext
  , ruleCycleAfterBeforeTime
  , ruleCycleOrdinalOfTime
  , ruleCycleTheOrdinalOfTime
  , ruleCycleTheOfTime
  , ruleCycleOrdinalAfterTime
  , ruleCycleTheOrdinalAfterTime
  , ruleCycleOrdinalQuarter
  , ruleCycleTheOrdinalQuarter
  , ruleCycleOrdinalQuarterYear
  , ruleDurationInWithinAfter
  , ruleDurationLastNext
  , ruleDurationHenceAgo
  , ruleDurationAfterBeforeTime
  , ruleIntervalForDurationFrom
  , ruleInNumeral
  , ruleTimezone
  , rulePartOfMonth
  , ruleNow
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ ruleComputedHolidays
  ++ rulePeriodicHolidays
