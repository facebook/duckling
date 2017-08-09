-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.Rules
  ( rules ) where

import Control.Monad (liftM2)
import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
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

instants :: [(Text, String, TG.Grain, Int)]
instants =
  [ ("right now", "((just|right)\\s*)now|immediately", TG.Second, 0)
  , ("today", "todays?|(at this time)", TG.Day, 0)
  , ("tomorrow", "(tmrw?|tomm?or?rows?)", TG.Day, 1)
  , ("yesterday", "yesterdays?", TG.Day, - 1)
  , ("end of month", "(the )?(EOM|end of (the )?month)", TG.Month, 1)
  , ("end of year", "(the )?(EOY|end of (the )?year)", TG.Year, 1)
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
    , Predicate isNotLatent
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
    , Predicate isNotLatent
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
    , Predicate isNotLatent
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
        tt $ predNth 1 (match == "after next") td
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
       liftM2 (||) (isIntegerBetween (- 10000) 0) (isIntegerBetween 25 999)
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
       Token Ordinal (OrdinalData {TOrdinal.value = v}):
       _) -> tt $ dayOfMonth v
      _ -> Nothing
  }

ruleNamedDOMOrdinal :: Rule
ruleNamedDOMOrdinal = Rule
  { name = "<named-month>|<named-day> <day-of-month> (ordinal)"
  , pattern =
    [ Predicate $ liftM2 (||) isAMonth isADayOfWeek
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
    [ Predicate $ liftM2 (&&) isNumeralSafeToUse (isIntegerBetween 0 23)
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

ruleMilitaryAMPM :: Rule
ruleMilitaryAMPM = Rule
  { name = "hhmm (military) am|pm"
  , pattern =
    [ regex "((?:1[012]|0?\\d))([0-5]\\d)"
    , regex "([ap])\\.?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):Token RegexMatch (GroupMatch (ap:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . timeOfDayAMPM (hourMinute True h m) $
          Text.toLower ap == "a"
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
        tt . timeOfDayAMPM td $ Text.toLower ap == "a"
      _ -> Nothing
  }

ruleHONumeral :: Rule
ruleHONumeral = Rule
  { name = "<hour-of-day> <integer>"
  , pattern =
    [ Predicate $ liftM2 (&&) isNotLatent isAnHourOfDay
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

ruleMMDDYYYY :: Rule
ruleMMDDYYYY = Rule
  { name = "mm/dd/yyyy"
  , pattern =
    [regex "(0?[1-9]|1[0-2])[/-](3[01]|[12]\\d|0?[1-9])[-/](\\d{2,4})"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
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

ruleMMDD :: Rule
ruleMMDD = Rule
  { name = "mm/dd"
  , pattern = [regex "(0?[1-9]|1[0-2])\\s?[/-]\\s?(3[01]|[12]\\d|0?[1-9])"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleNoonMidnightEOD :: Rule
ruleNoonMidnightEOD = Rule
  { name = "noon|midnight|EOD|end of day"
  , pattern = [regex "(noon|midni(ght|te)|(the )?(EOD|end of (the )?day))"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> tt . hour False $
        if match == "noon" then 12 else 0
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
  , pattern = [regex "toni(ght|gth|te)"]
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
    [ regex "(week(\\s|-)?end|wkend)"
    ]
  , prod = \_ -> tt weekend
  }

ruleSeasons :: Rule
ruleSeasons = Rule
  { name = "seasons"
  , pattern = [regex "(summer|fall|autumn|winter|spring)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        start <- case Text.toLower match of
          "summer" -> Just $ monthDay 6 21
          "fall"   -> Just $ monthDay 9 23
          "autumn" -> Just $ monthDay 9 23
          "winter" -> Just $ monthDay 12 21
          "spring" -> Just $ monthDay 3 20
          _ -> Nothing
        end <- case Text.toLower match of
          "summer" -> Just $ monthDay 9 23
          "fall"   -> Just $ monthDay 12 21
          "autumn" -> Just $ monthDay 12 21
          "winter" -> Just $ monthDay 3 20
          "spring" -> Just $ monthDay 6 21
          _ -> Nothing
        Token Time <$> interval TTime.Open start end
      _ -> Nothing

  }

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
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (d1:_)):
       _:
       Token RegexMatch (GroupMatch (d2:_)):
       _) -> do
        dd1 <- parseInt d1
        dd2 <- parseInt d2
        dom1 <- intersect (dayOfMonth dd1) td
        dom2 <- intersect (dayOfMonth dd2) td
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalFromMonthDDDD :: Rule
ruleIntervalFromMonthDDDD = Rule
  { name = "from <month> dd-dd (interval)"
  , pattern =
    [ regex "from"
    , Predicate isAMonth
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time td:
       Token RegexMatch (GroupMatch (d1:_)):
       _:
       Token RegexMatch (GroupMatch (d2:_)):
       _) -> do
        dd1 <- parseInt d1
        dd2 <- parseInt d2
        dom1 <- intersect (dayOfMonth dd1) td
        dom2 <- intersect (dayOfMonth dd2) td
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
    [ Predicate $ liftM2 (&&) isNotLatent isATimeOfDay
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
         interval TTime.Closed (timeOfDayAMPM td1 ampm) (timeOfDayAMPM td2 ampm)
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

ruleIntervalAfterTOD :: Rule
ruleIntervalAfterTOD = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "(anytime |sometimes? )?after"
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
    [ regex "since"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

daysOfWeek :: [(Text, String)]
daysOfWeek =
  [ ( "Monday"   , "monday|mon\\.?"         )
  , ( "Tuesday"  , "tuesday|tues?\\.?"      )
  , ( "Wednesday", "wed?nesday|wed\\.?"     )
  , ( "Thursday" , "thursday|thu(rs?)?\\.?" )
  , ( "Friday"   , "friday|fri\\.?"         )
  , ( "Saturday" , "saturday|sat\\.?"       )
  , ( "Sunday"   , "sunday|sun\\.?"         )
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

ruleMonths :: [Rule]
ruleMonths = zipWith go months [1..12]
  where
    go (name, regexPattern) i = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ month i
      }

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

usHolidays :: [(Text, String, Int, Int)]
usHolidays =
  [ ( "Christmas"       , "(xmas|christmas)( day)?"         , 12, 25 )
  , ( "Christmas Eve"   , "(xmas|christmas)( day)?('s)? eve", 12, 24 )
  , ( "New Year's Eve"  , "new year'?s? eve"                , 12, 31 )
  , ( "New Year's Day"  , "new year'?s?( day)?"             , 1 , 1  )
  , ( "Valentine's Day" , "valentine'?s?( day)?"            , 2 , 14 )
  , ( "Independence Day", "independence day"                , 7 , 4  )
  , ( "Halloween"       , "hall?owe?en( day)?"              , 10, 31 )
  ]

ruleUSHolidays :: [Rule]
ruleUSHolidays = map go usHolidays
  where
    go (name, regexPattern, m, d) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ monthDay m d
      }

moreUSHolidays :: [(Text, String, Int, Int, Int)]
moreUSHolidays =
  [ ( "Martin Luther King's Day" -- Third Monday of January
    , "(MLK|Martin Luther King,?)( Jr.?| Junior)? day"
    , 3, 1, 1
    )
  , ( "Father's Day" -- Third Sunday of June
    , "father'?s?'? day"
    , 3, 7, 6
    )
  , ( "Mother's Day" -- Second Sunday of May
    , "mother'?s?'? day"
    , 2, 7, 5
    )
  , ( "Thanksgiving Day" -- Fourth Thursday of November
    , "thanks?giving( day)?"
    , 4, 4, 11
    )
  ,  ( "Labor Day" -- First Monday of September
     , "labor day"
     , 1, 1, 9
     )
  ]

ruleMoreUSHolidays :: [Rule]
ruleMoreUSHolidays = map go moreUSHolidays
  where
    go (name, regexPattern, n, dow, m) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> tt $ nthDOWOfMonth n dow m
      }

-- The day after Thanksgiving (not always the fourth Friday of November)
ruleBlackFriday :: Rule
ruleBlackFriday = Rule
  { name = "black friday"
  , pattern =
    [ regex "black frid?day"
    ]
  , prod = \_ -> tt . cycleNthAfter False TG.Day 1 $ nthDOWOfMonth 4 4 11
  }

-- Last Monday of May
ruleMemorialDay :: Rule
ruleMemorialDay = Rule
  { name = "Memorial Day"
  , pattern = [regex "memorial day"]
  , prod = \_ -> tt $ predLastOf (dayOfWeek 1) (month 5)
  }

-- Long weekend before the last Monday of May
ruleMemorialDayWeekend :: Rule
ruleMemorialDayWeekend = Rule
  { name = "Memorial Day Weekend"
  , pattern = [regex "memorial day week(\\s|-)?end"]
  , prod = \_ ->
      tt . longWEBefore $ predLastOf (dayOfWeek 1) (month 5)
  }

-- Long weekend before the first Monday of September
ruleLaborDayWeekend :: Rule
ruleLaborDayWeekend = Rule
  { name = "Labor Day weekend"
  , pattern = [regex "labor day week(\\s|-)?end"]
  , prod = \_ -> tt . longWEBefore $ nthDOWOfMonth 1 1 9
  }

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
        let n = if match == "after" then 1 else - 1 in
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
      (Token TimeGrain grain:Token RegexMatch (GroupMatch (match:_)):Token Time td:_) ->
        let n = if match == "after" then 1 else - 1 in
          tt $ cycleNthAfter False grain n td
      _ -> Nothing
  }

ruleCycleLastNextN :: Rule
ruleCycleLastNextN = Rule
  { name = "last|next n <cycle>"
  , pattern =
    [ regex "((last|past)|(next))"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleN True grain $ if match == "next" then n else - n
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
         "after" -> tt . withDirection TTime.After $ inDuration dd
         "in" -> tt $ inDuration dd
         _ -> Nothing
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
        _ -> tt $ inDuration dd
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
      (_:Token Numeral (NumeralData {TNumeral.value = v}):_) ->
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
  [ ruleIntersect
  , ruleIntersectOf
  , ruleAbsorbOnTime
  , ruleAbsorbOnADOW
  , ruleAbsorbInMonth
  , ruleAbsorbCommaTOD
  , ruleNextDOW
  , ruleThisTime
  , ruleNextTime
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
  , ruleMMDDYYYY
  , ruleYYYYMMDD
  , ruleMMDD
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
  , ruleSeasons
  , ruleTODPrecision
  , rulePrecisionTOD
  , ruleIntervalFromMonthDDDD
  , ruleIntervalMonthDDDD
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
  , ruleIntervalAfterTOD
  , ruleIntervalSinceTOD
  , ruleMemorialDay
  , ruleMemorialDayWeekend
  , ruleLaborDayWeekend
  , ruleCycleThisLastNext
  , ruleCycleTheAfterBeforeTime
  , ruleCycleAfterBeforeTime
  , ruleCycleLastNextN
  , ruleCycleOrdinalOfTime
  , ruleCycleTheOrdinalOfTime
  , ruleCycleTheOfTime
  , ruleCycleOrdinalAfterTime
  , ruleCycleTheOrdinalAfterTime
  , ruleCycleOrdinalQuarter
  , ruleCycleTheOrdinalQuarter
  , ruleCycleOrdinalQuarterYear
  , ruleDurationInWithinAfter
  , ruleDurationHenceAgo
  , ruleDurationAfterBeforeTime
  , ruleIntervalForDurationFrom
  , ruleInNumeral
  , ruleTimezone
  , rulePartOfMonth
  , ruleNow
  , ruleBlackFriday
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleUSHolidays
  ++ ruleMoreUSHolidays
