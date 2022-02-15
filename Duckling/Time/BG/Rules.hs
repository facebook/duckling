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

module Duckling.Time.BG.Rules
  ( rules
  ) where

import Control.Applicative ((<|>))
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
    [ Predicate $ and . sequence [isNotLatent, isGrainFinerThan TG.Year]
    , Predicate $ or . sequence [isNotLatent, isGrainOfTime TG.Year]
    ]
  , prod = \case
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleIntersectOf :: Rule
ruleIntersectOf = Rule
  { name = "intersect by \",\", \"of\", \"from\", \"'s\""
  , pattern =
    [ Predicate isNotLatent
    , regex "от|за|на|,"
    , Predicate isNotLatent
    ]
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleIntersectYear :: Rule
ruleIntersectYear = Rule
  { name = "intersect by \",\", \"of\", \"from\" for year"
  , pattern =
    [ Predicate isNotLatent
    , regex "на|,"
    , Predicate $ isGrainOfTime TG.Year
    ]
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleAbsorbOnDay :: Rule
ruleAbsorbOnDay = Rule
  { name = "on <day>"
  , pattern =
    [ regex "на"
    , Predicate $ isGrainOfTime TG.Day
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbOnADOW :: Rule
ruleAbsorbOnADOW = Rule
  { name = "on a <named-day>"
  , pattern =
    [ regex "в"
    , Predicate isADayOfWeek
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbInMonthYear :: Rule
ruleAbsorbInMonthYear = Rule
  { name = "in|during <named-month>|year"
  , pattern =
    [ regex "в|през"
    , Predicate $ or . sequence [isAMonth, isGrainOfTime TG.Year]
    ]
  , prod = \case
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
  , prod = \case
      (token:_) -> Just token
      _ -> Nothing
  }

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("right now"        , TG.Second, 0  , "((точно\\s+)?сега)|веднага")
  , ("today"            , TG.Day   , 0  , "днес|(по това време)"      )
  , ("tomorrow"         , TG.Day   , 1  , "утре"                      )
  , ("yesterday"        , TG.Day   , - 1, "вчера"                     )
  , ( "after tomorrow"  , TG.Day   , 2  , "(в\\s*)?другиден"          )
  , ( "before yesterday", TG.Day   , - 2, "(оня ден)|завчера"         )
  ]

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "сега"
    ]
  , prod = \_ -> tt now
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "(т(о|а)зи)|следващ((ия(т)?)|ата)"
    , Predicate isADayOfWeek
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "(т(а|о)зи)|това"
    , Predicate isOkWithThisNext
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "следващ((ия(т)?)|ата|ото)"
    , Predicate isOkWithThisNext
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "((пред(н|ишн))|минал)((ия(т)?)|ата|ото)"
    , Predicate isOkWithThisNext
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ predNth (- 1) False td
      _ -> Nothing
  }

ruleLastWeekendOfMonth :: Rule
ruleLastWeekendOfMonth = Rule
  { name = "last weekend of <named-month>"
  , pattern =
    [ regex "последния(т)? уикенд\\s+(през|на)"
    , Predicate isAMonth
    ]
  , prod = \case
      (_:Token Time td2:_) -> tt $ predLastOf weekend td2
      _ -> Nothing
  }

ruleTimeBeforeLast :: Rule
ruleTimeBeforeLast = Rule
  { name = "<time> before last"
  , pattern =
    [ regex "по(\\-|\\s+)((пред(н|ишн))|минал)((ия(т)?)|ата|ото)"
    , dimension Time
    ]
  , prod = \case
    (_:Token Time td:_) -> tt $ predNth (- 2) False td
    _ -> Nothing
  }

ruleTimeAfterNext :: Rule
ruleTimeAfterNext = Rule
  { name = "<time> after next"
  , pattern =
    [ regex "по(\\-|\\s+)следващ((ия(т)?)|ата|ото)"
    , dimension Time
    ]
  , prod = \case
    (_:Token Time td:_) -> tt $ predNth 1 True td
    _ -> Nothing
  }

ruleLastDOWOfTime :: Rule
ruleLastDOWOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "последн((ия(т)?)|ата)"
    , Predicate isADayOfWeek
    , regex "на"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "последн((ия(т)?)|ото|ата)"
    , dimension TimeGrain
    , regex "на|в|през"
    , dimension Time
    ]
  , prod = \case
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleLastNight :: Rule
ruleLastNight = Rule
  { name = "last night"
  , pattern =
    [ regex "(късно )?снощи"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let hours = if Text.toLower match == "късно " then 3 else 6
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
    , regex "на|в|през"
    , dimension Time
    ]
  , prod = \case
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "след|от"
    , dimension Time
    ]
  , prod = \case
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleNDOWFromTime :: Rule
ruleNDOWFromTime = Rule
  { name = "<integer> <day-of-week> from <time>"
  , pattern =
    [ dimension Numeral
    , Predicate isADayOfWeek
    , regex "от"
    , dimension Time
    ]
  , prod = \case
      (token:Token Time td1:_:Token Time td2:_) -> do
        n <- getIntValue token
        tt $ predNthAfter (n - 1) td1 td2
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
      [ Predicate $ isIntegerBetween 25 10000
      ]
  , prod = \case
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleYearADBC :: Rule
ruleYearADBC = Rule
  { name = "<year> (bc|ad)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 10000
    , regex "(пр|сл)\\.?\\s+(Хр\\.?|н\\.?\\e.?)"
    ]
  , prod = \case
    (token:Token RegexMatch (GroupMatch (ab:_)):_) -> do
      y <- getIntValue token
      tt . yearADBC $ if Text.head (Text.toLower ab) == 'п' then -y else y
    _ -> Nothing
  }

ruleDOMLatent :: Rule
ruleDOMLatent = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern = [Predicate isDOMOrdinal]
  , prod = \case
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ dayOfMonth n
      _ -> Nothing
  }

ruleTheDOMNumeral :: Rule
ruleTheDOMNumeral = Rule
  { name = "the <day-of-month> (number)"
  , pattern =
    [ Predicate isDOMInteger
    ]
  , prod = \case
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ dayOfMonth n
      _ -> Nothing
  }

ruleTheDOMOrdinal :: Rule
ruleTheDOMOrdinal = Rule
  { name = "the <day-of-month> (ordinal)"
  , pattern =
    [ Predicate isDOMOrdinal
    ]
  , prod = \case
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
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
  , prod = \case
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
  , prod = \case
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMMonth :: Rule
ruleDOMMonth = Rule
  { name = "<day-of-month> (ordinal or number) <named-month>"
  , pattern =
    [ Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \case
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
  , prod = \case
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
  , prod = \case
      (token:Token Time td:Token RegexMatch (GroupMatch (match:_)):_) -> do
        intVal <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year intVal)
      _ -> Nothing
  }

ruleMonthYear :: Rule
ruleMonthYear = Rule
  { name = "<named-month> year"
  , pattern =
    [ Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \case
      (Token Time td:Token RegexMatch (GroupMatch (match:_)):_) -> do
        intVal <- parseInt match
        Token Time <$> intersect td (year intVal)
      _ -> Nothing
  }

ruleIdesOfMonth :: Rule
ruleIdesOfMonth = Rule
  { name = "the ides of <named-month>"
  , pattern =
    [ regex "средата на"
    , Predicate isAMonth
    ]
  , prod = \case
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
  , prod = \case
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (n < 13) n
      _ -> Nothing
  }

ruleAtTOD :: Rule
ruleAtTOD = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "в"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleTODOClock :: Rule
ruleTODOClock = Rule
  { name = "<time-of-day> o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "часа"
    ]
  , prod = \case
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleHHMM :: Rule
ruleHHMM = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute False h m
      _ -> Nothing
  }

ruleHHhMM :: Rule
ruleHHhMM = Rule
  { name = "hhhmm"
  , pattern =
    [ regex "(?<!/)((?:[01]?\\d)|(?:2[0-3]))ч(([0-5]\\d)|(?!\\d))"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm <|> Just 0
        tt $ hourMinute False h m
      _ -> Nothing
  }

ruleHHMMLatent :: Rule
ruleHHMMLatent = Rule
  { name = "hhmm (latent)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)(?!.\\d)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . mkLatent $ hourMinute False h m
      _ -> Nothing
  }

ruleHHMMSS :: Rule
ruleHHMMSS = Rule
  { name = "hh:mm:ss"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3])):([0-5]\\d):([0-5]\\d)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        s <- parseInt ss
        tt $ hourMinuteSecond False h m s
      _ -> Nothing
  }

ruleTODAM :: Rule
ruleTODAM = Rule
  { name = "<time-of-day> am"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "сутрин(та)?"
    ]
  , prod = \case
      (Token Time td:_) -> tt $ timeOfDayAMPM True td
      _ -> Nothing
  }

ruleTODPM :: Rule
ruleTODPM = Rule
  { name = "<time-of-day> pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "вечер(та)?|след об(е|я)д"
    ]
  , prod = \case
      (Token Time td:_) -> tt $ timeOfDayAMPM False td
      _ -> Nothing
  }

ruleHONumeral :: Rule
ruleHONumeral = Rule
  { name = "<hour-of-day> <integer>"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \case
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
    , regex "и половина"
    ]
  , prod = \case
      (Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHODQuarter :: Rule
ruleHODQuarter = Rule
  { name = "<hour-of-day> quarter"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "и (четвърт|(петнайсе(т)?))"
    ]
  , prod = \case
      (Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleNumeralToHOD :: Rule
ruleNumeralToHOD = Rule
  { name = "<integer> to|till|before <hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "без"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \case
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }

ruleQuarterToHOD :: Rule
ruleQuarterToHOD = Rule
  { name = "quarter to|till|before <hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "без (четвърт|(петнайсе(т)?))"
    ]
  , prod = \case
      (Token Time td:_:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleNumeralAfterHOD :: Rule
ruleNumeralAfterHOD = Rule
  { name = "integer after|past <hour-of-day>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "и"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \case
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        Token Time <$> minutesAfter n td
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

ruleDDMMYYYYDot :: Rule
ruleDDMMYYYYDot = Rule
  { name = "dd.mm.yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\.(1[0-2]|0?[1-9])\\.(\\d{2,4})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
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
    [ regex "(обед|обяд|полунощ|((в )?(края на деня)))"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> tt . hour False $
        if (Text.toLower match == "обед" || Text.toLower match == "обяд")  then 12 else 0
      _ -> Nothing
  }

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(сутрин(та)?|(по|след) ?об(е|я)д|вечер(та)?|(през )?нощ(та)?)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case Text.toLower match of
              "сутрин"  -> (hour False 4, hour False 12)
              "сутринта"  -> (hour False 4, hour False 12)
              "вечер"  -> (hour False 18, hour False 0)
              "вечерта"  -> (hour False 18, hour False 0)
              "нощ"    -> (hour False 0, hour False 4)
              "нощта"    -> (hour False 0, hour False 4)
              "през нощта"    -> (hour False 0, hour False 4)
              "обед"    -> (hour False 12, hour False 14)
              "по обед" -> (hour False 12, hour False 14)
              "обяд"    -> (hour False 12, hour False 14)
              "по обяд" -> (hour False 12, hour False 14)
              _          -> (hour False 12, hour False 19)
        td <- interval TTime.Open start end
        tt . partOfDay $ mkLatent td
      _ -> Nothing
  }

ruleEarlyMorning :: Rule
ruleEarlyMorning = Rule
  { name = "early morning"
  , pattern =
    [ regex "рано сутрин(та)?"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 4) (hour False 9)
  }

rulePODThis :: Rule
rulePODThis = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "(т(о|а)зи)"
    , Predicate isAPartOfDay
    ]
  , prod = \case
      (_:Token Time td:_) -> Token Time . partOfDay . notLatent <$>
        intersect today td
      _ -> Nothing
  }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern =
    [ regex "(късно )?вечер(та)?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let h = if Text.toLower match == "късно " then 21 else 18
        evening <- interval TTime.Open (hour False h) (hour False 0)
        Token Time . partOfDay . notLatent <$> intersect today evening
      _ -> Nothing
  }

ruleAfterPartofday :: Rule
ruleAfterPartofday = Rule
  { name = "after lunch/work/school"
  , pattern =
    [ regex "след[\\s*]?((об(е|я)д)|работа|училище)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- case Text.toLower match of
          "обед"  -> Just (hour False 13, hour False 17)
          "обяд"  -> Just (hour False 13, hour False 17)
          "работа"   -> Just (hour False 17, hour False 21)
          "училище" -> Just (hour False 15, hour False 21)
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
  , prod = \case
      (Token Time td:Token Time pod:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

rulePODofTime :: Rule
rulePODofTime = Rule
  { name = "<part-of-day> of <time>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "на"
    , dimension Time
    ]
  , prod = \case
      (Token Time pod:_:Token Time td:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "(края на седмицата)|(уикенд(а)?)"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

ruleWeek :: Rule
ruleWeek = Rule
 { name = "week"
 , pattern = [regex "(цяла|(остатъка на( тази)?)) седмица(та)?"]
 , prod = \case
     (Token RegexMatch (GroupMatch (match:_)):_) ->
       let end = cycleNthAfter True TG.Day (-2) $ cycleNth TG.Week 1
           period = case Text.toLower match of
                      "цяла" -> interval Closed (cycleNth TG.Week 0) end
                      "остатъка на" -> interval Open today end
                      "остатъка на тази" -> interval Open today end
                      _ -> Nothing
       in case Text.toLower match of
         "тази" -> Token Time . mkLatent <$> period
         _ -> Token Time <$> period
     _ -> Nothing
 }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "last|this|next <season>"
  , pattern =
    [ regex "(това|тази|(по\\-)?(по\\-)?((настоящ|следващ|друг|последн|минал|предн)(ото|ата))) seasons?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        n <- case Text.toLower match of
               "това" -> Just 0
               "тази" -> Just 0
               "настоящото" -> Just 0
               "настоящата" -> Just 0
               "миналото" -> Just (-1)
               "миналата" -> Just (-1)
               "последното" -> Just (-1)
               "последната" -> Just (-1)
               "предното" -> Just (-1)
               "предната" -> Just (-1)
               "следващото" -> Just 1
               "следващата" -> Just 1
               "другото" -> Just 1
               "другата" -> Just 1
               "по-миналото" -> Just (-2)
               "по-миналата" -> Just (-2)
               "по-последното" -> Just (-2)
               "по-последната" -> Just (-2)
               "по-предното" -> Just (-2)
               "по-предната" -> Just (-2)
               "по-следващото" -> Just 2
               "по-следващата" -> Just 2
               "по-другото" -> Just 2
               "по-другата" -> Just 2
               "по-по-миналото" -> Just (-3)
               "по-по-миналата" -> Just (-3)
               "по-по-последното" -> Just (-3)
               "по-по-последната" -> Just (-3)
               "по-по-предното" -> Just (-3)
               "по-по-предната" -> Just (-3)
               "по-по-следващото" -> Just 3
               "по-по-следващата" -> Just 3
               "по-по-другото" -> Just 3
               "по-по-другата" -> Just 3
               _ -> Nothing
        tt $ predNth n False season
      _ -> Nothing
  }

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "summer", "лято"     , monthDay  6 21, monthDay  9 23 )
  , ( "fall"  , "есен"     , monthDay  9 23, monthDay 12 21 )
  , ( "winter", "зима"     , monthDay 12 21, monthDay  3 20 )
  , ( "spring", "пролет"   , monthDay  3 20, monthDay  6 21 )
  ]

ruleTODPrecision :: Rule
ruleTODPrecision = Rule
  { name = "<time-of-day> sharp|exactly"
  , pattern =
    [ regex "(в )?точно( в )?"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePrecisionTOD :: Rule
rulePrecisionTOD = Rule
  { name = "about|exactly <time-of-day>"
  , pattern =
    [ regex "около"
    , Predicate $ isGrainFinerThan TG.Year
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleIntervalMonthDDDD :: Rule
ruleIntervalMonthDDDD = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMValue
    , regex "\\-|до"
    , Predicate isDOMValue
    ]
  , prod = \case
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
    , regex "\\-|до"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \case
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

ruleIntervalFromDDDDMonth :: Rule
ruleIntervalFromDDDDMonth = Rule
  { name = "from the <day-of-month> (ordinal or number) to the <day-of-month> (ordinal or number) <named-month> (interval)"
  , pattern =
    [ regex "от"
    , Predicate isDOMValue
    , regex "\\-|до"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \case
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
    , regex "\\-|до"
    , Predicate isNotLatent
    ]
  , prod = \case
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
  , prod = \case
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
    [ regex "от"
    , dimension Time
    , regex "до"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between <time> and <time>"
  , pattern =
    [ regex "между"
    , dimension Time
    , regex "и"
    , dimension Time
    ]
  , prod = \case
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
    , regex "\\-|до"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalTODBetween :: Rule
ruleIntervalTODBetween = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "между"
    , Predicate isATimeOfDay
    , regex "и"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalBy :: Rule
ruleIntervalBy = Rule
  { name = "by <time>"
  , pattern =
    [ regex "до"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleIntervalByTheEndOf :: Rule
ruleIntervalByTheEndOf = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "до края на"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td:_) -> Token Time <$> interval TTime.Closed now td
      _ -> Nothing
  }

ruleIntervalUntilTime :: Rule
ruleIntervalUntilTime = Rule
  { name = "until <time>"
  , pattern =
    [ regex "преди|до"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td:_) -> tt . withDirection TTime.Before $ notLatent td
      _ -> Nothing
  }

ruleIntervalAfterFromSinceTime :: Rule
ruleIntervalAfterFromSinceTime = Rule
  { name = "from|since|after <time>"
  , pattern =
    [ regex "след|от"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td:_) -> tt . withDirection TTime.After $ notLatent td
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "понеделник|пон\\.?|пн\\.?" )
  , ( "Tuesday"  , "вторник|вт\\.?"            )
  , ( "Wednesday", "сряда|ср\\.?"              )
  , ( "Thursday" , "четвъртък|четв\\.?|чт\\.?" )
  , ( "Friday"   , "петък|пет\\.?|пт\\.?"      )
  , ( "Saturday" , "събота|съб\\.?|сб\\.?"     )
  , ( "Sunday"   , "неделя|нед\\.?|нд\\.?"     )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"  , "януари|ян\\.?"     )
  , ( "February" , "февруари|февр\\.?" )
  , ( "March"    , "март"              )
  , ( "April"    , "април|апр\\.?"     )
  , ( "May"      , "май"               )
  , ( "June"     , "юни"               )
  , ( "July"     , "юли"               )
  , ( "August"   , "август|авг\\.?"    )
  , ( "September", "септември|септ\\.?")
  , ( "October"  , "октомври|окт\\.?"  )
  , ( "November" , "ноември|ноем\\.?"  )
  , ( "December" , "декември|дек\\.?"  )
  ]

rulePartOfMonth :: Rule
rulePartOfMonth = Rule
  { name = "part of <named-month>"
  , pattern =
    [ regex "(началото|средата|края) на"
    , Predicate isAMonth
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "началото" -> Just (1, 10)
          "средата"   -> Just (11, 20)
          "края"  -> Just (21, -1)
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
    [ regex "(в )?(началото|края) на"
    , Predicate isAMonth
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (_:match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "началото" -> Just (1, 10)
          "края"       -> Just (21, -1)
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
  , pattern =
    [ regex "((до|в)\\s+)?края на месеца"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_)
        | (Just start, Just end) <- parsed ->
          Token Time <$> interval TTime.Open start end
        where
          cycleMonth = cycleNth TG.Month
          parsed = if "до" `Text.isPrefixOf` Text.toLower match
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
  , pattern = [ regex "(в\\s+ )?началото на месеца" ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      end <- intersect (dayOfMonth 10) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfYear :: Rule
ruleEndOrBeginningOfYear = Rule
  { name = "at the beginning|end of <year>"
  , pattern =
    [ regex "(в\\s+)?(началото|края) на"
    , Predicate $ isGrainOfTime TG.Year
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (_:match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "началото" -> Just (1, 4)
          "края"       -> Just (9, -1)
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
  , pattern =
    [ regex "((до|в)\\s+)?края на годината"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        start <- std
        end <- intersect (month 1) $ cycleYear 1
        Token Time <$> interval TTime.Open start end
          where
            std = if "до" `Text.isPrefixOf` Text.toLower match
              then Just now
              else intersect (month 9) $ cycleYear 0
            cycleYear = cycleNth TG.Year
      _ -> Nothing
  }

ruleBeginningOfYear :: Rule
ruleBeginningOfYear = Rule
  { name = "beginning of year"
  , pattern = [ regex "(в\\s+)?началото на годината" ]
  , prod = \_ -> do
      start <- intersect (month 1) $ cycleNth TG.Year 0
      end <- intersect (month 4) $ cycleNth TG.Year 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfWeek :: Rule
ruleEndOrBeginningOfWeek = Rule
  { name = "at the beginning|end of <week>"
  , pattern =
    [ regex "(в\\s+)?(началото|края) на"
    , Predicate $ isGrainOfTime TG.Week
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (_:match1:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match1 of
          "началото" -> Just (1, 3)
          "края"       -> Just (5, 7)
          _           -> Nothing
        start <- intersect td $ dayOfWeek sd
        end <- intersect td $ dayOfWeek ed
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "April Fools", "първи април", monthDay 4 1 )
  , ( "Bulgarian Liberation Day", "(трети март)|(ден(я|ят)? на освобождението на българия( от турско робство)?)", monthDay 3 3 )
  , ( "Bulgarian Indenpendence Day", "ден(я|ят)? на (обявянето на)? независимостта на българия", monthDay 9 22 )
  , ( "Bulgarian Unification Day", "ден(я|ят)? на съединението на (княжество )?българия(и източна румелия)?", monthDay 9 6 )
  , ( "Slavonic Litaracy Day", "(ден(я|ят)? на славянската писменост)|(св(\\.|ети)?(св(\\.|ети)?)? кирил и методи(й)?)", monthDay 5 24 )
  , ( "Saint Geroge's Day", "(св(\\.|ети)? георги)|гергьовден", monthDay 5 6 )
  , ( "Assumption of Mary", "голяма богородица", monthDay 8 15 )
  , ( "Christmas", "коледа|(рождество( христово)?)", monthDay 12 25 )
  , ( "Christmas Eve", "бъдни вечер", monthDay 12 24 )
  , ( "Earth Day", "ден(я|ят) на земята", monthDay 4 22 )
  , ( "May Day", "(първи май)|(ден(я|ят)? на труда)", monthDay 5 1 )
  , ( "New Year's Day", "нова година", monthDay  1  1 )
  , ( "St. George's Day", "гергьовден|(св(\\.|ети)? георги)", monthDay 5 6 )
  , ( "Valentine's Day", "св(\\.|ети)? валентин", monthDay 2 14 )
  , ( "International Women's Day", "ден(я|ят)? на (майката|жената)", monthDay 3 8 )
  ]

ruleComputedHolidays :: [Rule]
ruleComputedHolidays = mkRuleHolidays
  [ ( "Orthodox Easter Monday", "велики понеделник"
    , cycleNthAfter False TG.Day 1 orthodoxEaster )
  , ( "Orthodox Easter Sunday", "великден"
    , orthodoxEaster )
  , ( "Orthodox Holy Saturday", "велика събота"
    , cycleNthAfter False TG.Day (-1) orthodoxEaster )
  , ( "Orthodox Great Friday", "(велики|разпети) петък"
    , cycleNthAfter False TG.Day (-2) orthodoxEaster )
  , ( "Orthodox Palm Sunday", "цветница"
    , cycleNthAfter False TG.Day (-7) orthodoxEaster )
  ]

ruleComputedHolidays' :: [Rule]
ruleComputedHolidays' = mkRuleHolidays'
  [ ( "Great Lent", "велики(те)? пости"
    , let start = cycleNthAfter False TG.Day (-48) orthodoxEaster
          end = cycleNthAfter False TG.Day (-9) orthodoxEaster
        in interval TTime.Open start end )

  -- Other
  -- Last Saturday of March unless it falls on Holy Saturday
  -- In which case it's the Saturday before
  , ( "Earth Hour", "час(а|ът) на земята"
    , let holySaturday = cycleNthAfter False TG.Day (-1) easterSunday
          tentative = predLastOf (dayOfWeek 6) (month 3)
          alternative = cycleNthAfter False TG.Day (-7) tentative
        in do
          day <- intersectWithReplacement holySaturday tentative alternative
          start <- intersect day $ hourMinute True 20 30
          interval TTime.Closed start $ cycleNthAfter False TG.Minute 60 start )
  ]

ruleCycleThisLastNext :: Rule
ruleCycleThisLastNext = Rule
  { name = "this|last|next <cycle>"
  , pattern =
    [ regex "(т(о|а)зи|това|((по\\-)*(настоящ|идн|следващ|минал|последн|предишн)(ия(т)?|ата|ото)))"
    , dimension TimeGrain
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):Token TimeGrain grain:_) ->
        case Text.toLower match of
          "този"          -> tt $ cycleNth grain 0
          "тази"          -> tt $ cycleNth grain 0
          "това"          -> tt $ cycleNth grain 0
          "настоящия"     -> tt $ cycleNth grain 0
          "настоящият"    -> tt $ cycleNth grain 0
          "настоящата"    -> tt $ cycleNth grain 0
          "настоящото"    -> tt $ cycleNth grain 0
          "последния"     -> tt . cycleNth grain $ - 1
          "последният"    -> tt . cycleNth grain $ - 1
          "последната"    -> tt . cycleNth grain $ - 1
          "последното"    -> tt . cycleNth grain $ - 1
          "миналия"       -> tt . cycleNth grain $ - 1
          "миналият"      -> tt . cycleNth grain $ - 1
          "миналата"      -> tt . cycleNth grain $ - 1
          "миналото"      -> tt . cycleNth grain $ - 1
          "предишния"     -> tt . cycleNth grain $ - 1
          "предишният"    -> tt . cycleNth grain $ - 1
          "предишната"    -> tt . cycleNth grain $ - 1
          "предишното"    -> tt . cycleNth grain $ - 1
          "следващия"     -> tt $ cycleNth grain 1
          "следващият"    -> tt $ cycleNth grain 1
          "следващата"    -> tt $ cycleNth grain 1
          "следващото"    -> tt $ cycleNth grain 1
          "идния"         -> tt $ cycleNth grain 1
          "идният"        -> tt $ cycleNth grain 1
          "идната"        -> tt $ cycleNth grain 1
          "идното"        -> tt $ cycleNth grain 1
          "по-последния"     -> tt . cycleNth grain $ - 2
          "по-последният"    -> tt . cycleNth grain $ - 2
          "по-последната"    -> tt . cycleNth grain $ - 2
          "по-последното"    -> tt . cycleNth grain $ - 2
          "по-миналия"       -> tt . cycleNth grain $ - 2
          "по-миналият"      -> tt . cycleNth grain $ - 2
          "по-миналата"      -> tt . cycleNth grain $ - 2
          "по-миналото"      -> tt . cycleNth grain $ - 2
          "по-предишния"     -> tt . cycleNth grain $ - 2
          "по-предишният"    -> tt . cycleNth grain $ - 2
          "по-предишната"    -> tt . cycleNth grain $ - 2
          "по-предишното"    -> tt . cycleNth grain $ - 2
          "по-следващия"     -> tt $ cycleNth grain 2
          "по-следващият"    -> tt $ cycleNth grain 2
          "по-следващата"    -> tt $ cycleNth grain 2
          "по-следващото"    -> tt $ cycleNth grain 2
          "по-по-последния"     -> tt . cycleNth grain $ - 3
          "по-по-последният"    -> tt . cycleNth grain $ - 3
          "по-по-последната"    -> tt . cycleNth grain $ - 3
          "по-по-последното"    -> tt . cycleNth grain $ - 3
          "по-по-миналия"       -> tt . cycleNth grain $ - 3
          "по-по-миналият"      -> tt . cycleNth grain $ - 3
          "по-по-миналата"      -> tt . cycleNth grain $ - 3
          "по-по-миналото"      -> tt . cycleNth grain $ - 3
          "по-по-предишния"     -> tt . cycleNth grain $ - 3
          "по-по-предишният"    -> tt . cycleNth grain $ - 3
          "по-по-предишната"    -> tt . cycleNth grain $ - 3
          "по-по-предишното"    -> tt . cycleNth grain $ - 3
          "по-по-следващия"     -> tt $ cycleNth grain 3
          "по-по-следващият"    -> tt $ cycleNth grain 3
          "по-по-следващата"    -> tt $ cycleNth grain 3
          "по-по-следващото"    -> tt $ cycleNth grain 3
          _ -> Nothing
      _ -> Nothing
  }

ruleCycleAfterBeforeTime :: Rule
ruleCycleAfterBeforeTime = Rule
  { name = "<cycle> after|before <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "след|преди"
    , dimension Time
    ]
  , prod = \case
      (Token TimeGrain grain:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) ->
        let n = if Text.toLower match == "след" then 1 else - 1 in
          tt $ cycleNthAfter False grain n td
      _ -> Nothing
  }

ruleDayInDuration :: Rule
ruleDayInDuration = Rule
  { name = "<day> in <duration>"
  , pattern =
    [ Predicate $ or . sequence [isGrainOfTime TG.Day, isGrainOfTime TG.Month]
    , regex "в|през"
    , Predicate $ isDurationGreaterThan TG.Hour
    ]
  , prod = \case
      (Token Time td:_:Token Duration dd:_) ->
        Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleDurationInWithinAfter :: Rule
ruleDurationInWithinAfter = Rule
  { name = "in|within|after <duration>"
  , pattern =
    [ regex "(за|в рамките на|след)"
    , dimension Duration
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
        Token Duration dd:
        _) -> case Text.toLower match of
          "в рамките на" -> Token Time <$> interval TTime.Open now (inDuration dd)
          "след"  -> tt . withDirection TTime.After $ inDuration dd
          "за"     -> tt $ inDuration dd
          _        -> Nothing
      _ -> Nothing
  }

ruleNDOWago :: Rule
ruleNDOWago = Rule
  { name = "<integer> <named-day> ago|back"
  , pattern =
    [ regex "преди"
    , Predicate isNatural
    , Predicate isADayOfWeek
    ]
  , prod = \case
      (_:Token Numeral NumeralData{TNumeral.value = v}:Token Time td:_) ->
        tt $ predNth (- (floor v)) False td
      _ -> Nothing
  }

ruleDurationHenceAgo :: Rule
ruleDurationHenceAgo = Rule
  { name = "<duration> hence|ago"
  , pattern =
    [ regex "(от|преди)"
    , dimension Duration
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):Token Duration dd:_) ->
        case Text.toLower match of
          "от" -> tt $ durationAgo dd
          _     -> tt $ inDuration dd
      _ -> Nothing
  }

ruleInNumeral :: Rule
ruleInNumeral = Rule
  { name = "in <number> (implicit minutes)"
  , pattern =
    [ regex "след"
    , Predicate $ isIntegerBetween 0 60
    ]
  , prod = \case
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        tt . inDuration . duration TG.Minute $ floor v
      _ -> Nothing
  }

ruleIntervalForDurationFrom :: Rule
ruleIntervalForDurationFrom = Rule
  { name = "for <duration> from <time>"
  , pattern =
    [ regex "за"
    , dimension Duration
    , regex "от|след"
    , dimension Time
    ]
  , prod = \case
      (_:Token Duration dd:_:Token Time td1:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleIntervalTimeForDuration :: Rule
ruleIntervalTimeForDuration = Rule
  { name = "<time> for <duration>"
  , pattern =
    [ Predicate isNotLatent
    , regex "за"
    , dimension Duration
    ]
  , prod = \case
      (Token Time td1:_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleIntervalFromTimeForDuration :: Rule
ruleIntervalFromTimeForDuration = Rule
  { name = "from <time> for <duration>"
  , pattern =
    [ regex "от|след"
    , Predicate isNotLatent
    , regex "за"
    , dimension Duration
    ]
  , prod = \case
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
  , prod = \case
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
  , prod = \case
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
    , regex "\\-|до"
    , Predicate $ and . sequence [isATimeOfDay, hasNoTimezone]
    , regex $ "\\b(" ++ timezoneName ++ ")\\b"
    ]
  , prod = \case
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
  , ruleAbsorbInMonthYear
  , ruleAbsorbCommaTOD
  , ruleAbsorbOnADOW
  , ruleNextDOW
  , ruleNextTime
  , ruleThisTime
  , ruleLastTime
  , ruleTimeBeforeLast
  , ruleTimeAfterNext
  , ruleLastDOWOfTime
  , ruleLastCycleOfTime
  , ruleLastNight
  , ruleLastWeekendOfMonth
  , ruleNthTimeOfTime
  , ruleNthTimeAfterTime
  , ruleNDOWFromTime
  , ruleYearLatent
  , ruleYearADBC
  , ruleTheDOMNumeral
  , ruleTheDOMOrdinal
  , ruleDOMLatent
  , ruleNamedDOMOrdinal
  , ruleMonthDOMNumeral
  , ruleDOMMonth
  , ruleDOMOrdinalMonthYear
  , ruleDOMMonthYear
  , ruleIdesOfMonth
  , ruleTODLatent
  , ruleAtTOD
  , ruleTODOClock
  , ruleTODAM
  , ruleTODPM
  , ruleHHMM
  , ruleHHhMM
  , ruleHHMMLatent
  , ruleHHMMSS
  , ruleHONumeral
  , ruleHODHalf
  , ruleHODQuarter
  , ruleNumeralToHOD
  , ruleQuarterToHOD
  , ruleNumeralAfterHOD
  , ruleYYYYQQ
  , ruleYYYYMM
  , ruleYYYYMMDD
  , ruleDDMMYYYYDot
  , ruleMMYYYY
  , ruleNoonMidnightEOD
  , rulePartOfDays
  , ruleEarlyMorning
  , rulePODThis
  , ruleTonight
  , ruleAfterPartofday
  , ruleTimePOD
  , rulePODofTime
  , ruleWeekend
  , ruleWeek
  , ruleTODPrecision
  , rulePrecisionTOD
  , ruleIntervalFromDDDDMonth
  , ruleIntervalMonthDDDD
  , ruleIntervalDDDDMonth
  , ruleIntervalDash
  , ruleIntervalSlash
  , ruleIntervalFrom
  , ruleIntervalBetween
  , ruleIntervalTODDash
  , ruleIntervalTODBetween
  , ruleIntervalBy
  , ruleIntervalByTheEndOf
  , ruleIntervalUntilTime
  , ruleIntervalAfterFromSinceTime
  , ruleCycleThisLastNext
  , ruleCycleAfterBeforeTime
  , ruleDayInDuration
  , ruleDurationInWithinAfter
  , ruleNDOWago
  , ruleDurationHenceAgo
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
  , ruleMonthYear
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ ruleComputedHolidays
  ++ ruleComputedHolidays'
  ++ rulePeriodicHolidays
