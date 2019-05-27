-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.IT.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
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

ruleFestaDellaRepubblica :: Rule
ruleFestaDellaRepubblica = Rule
  { name = "festa della repubblica"
  , pattern =
    [ regex "((festa del)?la )?repubblica"
    ]
  , prod = \_ -> tt $ monthDay 6 2
  }

ruleEpifania :: Rule
ruleEpifania = Rule
  { name = "epifania"
  , pattern =
    [ regex "(epifania|befana)"
    ]
  , prod = \_ -> tt $ monthDay 1 6
  }

ruleDayofmonthNamedmonth :: Rule
ruleDayofmonthNamedmonth = Rule
  { name = "<day-of-month> <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleTheDayAfterTomorrow :: Rule
ruleTheDayAfterTomorrow = Rule
  { name = "the day after tomorrow"
  , pattern =
    [ regex "(il giorno )?dopo\\s?domani"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleInafterDuration :: Rule
ruleInafterDuration = Rule
  { name = "in/after <duration>"
  , pattern =
    [ regex "[tf]ra|in|dopo"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleTheLastCycle :: Rule
ruleTheLastCycle = Rule
  { name = "the last <cycle>"
  , pattern =
    [ regex "(([nd]el)?l' ?ultim|(il|la) passat|([nd]el)?l[ao] scors)[oa]"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleStanotte :: Rule
ruleStanotte = Rule
  { name = "stanotte"
  , pattern =
    [ regex "(sta|nella )notte|(in|nella) nottata"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day 1
      td2 <- interval TTime.Open (hour False 0) (hour False 4)
      Token Time . partOfDay <$> intersect td1 td2
  }

ruleDomattina :: Rule
ruleDomattina = Rule
  { name = "domattina"
  , pattern =
    [ regex "domattina"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day 1
      td2 <- interval TTime.Open (hour False 4) (hour False 12)
      Token Time . partOfDay <$> intersect td1 td2
  }

ruleTheCycleNext :: Rule
ruleTheCycleNext = Rule
  { name = "the <cycle> next"
  , pattern =
    [ regex "l'|il|la|[nd]el(la)?"
    , dimension TimeGrain
    , regex "prossim[oa]"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleCycleNext :: Rule
ruleCycleNext = Rule
  { name = "<cycle> next"
  , pattern =
    [ dimension TimeGrain
    , regex "prossim[oa]"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleFestaDellaLiberazione :: Rule
ruleFestaDellaLiberazione = Rule
  { name = "festa della liberazione"
  , pattern =
    [ regex "((festa|anniversario) della|(al)?la) liberazione"
    ]
  , prod = \_ -> tt $ monthDay 4 25
  }

ruleStamattina :: Rule
ruleStamattina = Rule
  { name = "stamattina"
  , pattern =
    [ regex "stamattina"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 4) (hour False 12)
      Token Time . partOfDay <$> intersect today td2
  }

ruleYearNotLatent :: Rule
ruleYearNotLatent = Rule
  { name = "year (1000-2100 not latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt $ year v
      _ -> Nothing
  }

ruleValentinesDay :: Rule
ruleValentinesDay = Rule
  { name = "valentine's day"
  , pattern =
    [ regex "san valentino|festa degli innamorati"
    ]
  , prod = \_ -> tt $ monthDay 2 14
  }

ruleTheOrdinalCycleOfTime :: Rule
ruleTheOrdinalCycleOfTime = Rule
  { name = "the <ordinal> <cycle> of <time>"
  , pattern =
    [ regex "il|l[a']|[nd]el(l[a'])?"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "di|del(l[a'])?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
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

ruleTheOrdinalQuarter :: Rule
ruleTheOrdinalQuarter = Rule
  { name = "the <ordinal> quarter"
  , pattern =
    [ regex "il|[nd]el(l')?|l'"
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

ruleCycleTheOrdinalTime :: Rule
ruleCycleTheOrdinalTime = Rule
  { name = "the <ordinal> <cycle> <time>"
  , pattern =
    [ regex "il|[nd]el(l')?|l'"
    , dimension Ordinal
    , dimension TimeGrain
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleDdddMonthInterval :: Rule
ruleDdddMonthInterval = Rule
  { name = "dd-dd <month> (interval)"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       _:
       Token RegexMatch (GroupMatch (m2:_)):
       Token Time td:
       _) -> do
         v1 <- parseInt m1
         v2 <- parseInt m2
         from <- intersect (dayOfMonth v1) td
         to <- intersect (dayOfMonth v2) td
         Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleTimeLast :: Rule
ruleTimeLast = Rule
  { name = "<time> last"
  , pattern =
    [ dimension Time
    , regex "(ultim|scors|passat)[oaie]"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleThisDayofweek :: Rule
ruleThisDayofweek = Rule
  { name = "this <day-of-week>"
  , pattern =
    [ regex "quest[oaie]"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleUna :: Rule
ruleUna = Rule
  { name = "una"
  , pattern =
    [ regex "una"
    ]
  , prod = \_ -> tt . mkLatent $ hour True 1
  }

ruleNthTimeOfTime2 :: Rule
ruleNthTimeOfTime2 = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "di|del(l[oa'])|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData {TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNewYearsDay :: Rule
ruleNewYearsDay = Rule
  { name = "new year's day"
  , pattern =
    [ regex "(capodanno|primo dell' ?anno)"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time> "
  , pattern =
    [ regex "(ultim|scors|passat)[oaie]"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleIlDayofmonthDeNamedmonth :: Rule
ruleIlDayofmonthDeNamedmonth = Rule
  { name = "il <day-of-month> <named-month>"
  , pattern =
    [ regex "il|l'"
    , Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|[fs]ino a(l(l[e'])?)?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleEvening :: Rule
ruleEvening = Rule
  { name = "evening"
  , pattern =
    [ regex "ser(ata|[ae])"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleDayOfMonthSt :: Rule
ruleDayOfMonthSt = Rule
  { name = "day of month (1st)"
  , pattern =
    [ regex "(primo|1o|1º|1°)"
    ]
  , prod = \_ -> tt $ dayOfMonth 1
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in|entro <duration>"
  , pattern =
    [ regex "(in|entro)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration dd:
       _) -> case Text.toLower match of
        "entro" -> Token Time <$> interval TTime.Open now (inDuration dd)
        "in"    -> tt $ inDuration dd
        _       -> Nothing
      _ -> Nothing
  }

ruleInNamedmonth :: Rule
ruleInNamedmonth = Rule
  { name = "in <named-month>"
  , pattern =
    [ regex "in|del mese( di)?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleLeIdiDiNamedmonth :: Rule
ruleLeIdiDiNamedmonth = Rule
  { name = "le idi di <named-month>"
  , pattern =
    [ regex "(le )?idi di"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td@TimeData {TTime.form = Just (TTime.Month m)}:_) ->
        Token Time <$>
          intersect (dayOfMonth $ if elem m [3, 5, 7, 10] then 15 else 13) td
      _ -> Nothing
  }

ruleRightNow :: Rule
ruleRightNow = Rule
  { name = "right now"
  , pattern =
    [ regex "(subito|(immediata|attual)mente|(proprio )?adesso|(in questo|al) momento|ora)"
    ]
  , prod = \_ -> tt now
  }

ruleToday :: Rule
ruleToday = Rule
  { name = "today"
  , pattern =
    [ regex "(di )?oggi|in giornata"
    ]
  , prod = \_ -> tt today
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "(l')ultim[oa]"
    , dimension TimeGrain
    , regex "di|del(l[oa'])"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleHhRelativeminutesDelPomeriggiotimeofday2 :: Rule
ruleHhRelativeminutesDelPomeriggiotimeofday2 = Rule
  { name = "hh <relative-minutes> del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e"
    , Predicate $ isIntegerBetween 1 59
    , regex "d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        let h = if hours > 12 then hours else hours + 12
        tt $ hourMinute False h n
      _ -> Nothing
  }
ruleHourofdayIntegerAsRelativeMinutes :: Rule
ruleHourofdayIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
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
ruleHourofdayAndRelativeMinutes :: Rule
ruleHourofdayAndRelativeMinutes = Rule
  { name = "<hour-of-day> and <relative minutes>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }
ruleRelativeMinutesToIntegerAsHourofday :: Rule
ruleRelativeMinutesToIntegerAsHourofday = Rule
  { name = "relative minutes to <integer> (as hour-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "a"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }
ruleHourofdayMinusIntegerAsRelativeMinutes :: Rule
ruleHourofdayMinusIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> minus <integer> (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "meno"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }
ruleHhRelativeminutesDelPomeriggiotimeofday :: Rule
ruleHhRelativeminutesDelPomeriggiotimeofday = Rule
  { name = "hh <relative-minutes> del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    , regex "d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       token:
       _) -> do
        n <- getIntValue token
        let h = if hours > 12 then hours else hours + 12
        tt $ hourMinute False h n
      _ -> Nothing
  }

ruleHhIntegerminutesDelPomeriggiotimeofday2 :: Rule
ruleHhIntegerminutesDelPomeriggiotimeofday2 = Rule
  { name = "hh <relative-minutes> minutes del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e"
    , Predicate $ isIntegerBetween 1 59
    , regex "min(ut[oi]|\\.)? d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        let h = if hours > 12 then hours else hours + 12
        tt $ hourMinute False h n
      _ -> Nothing
  }

ruleHourofdayIntegerMinutes :: Rule
ruleHourofdayIntegerMinutes = Rule
  { name = "<hour-of-day> <integer> minutes"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    , regex "min(ut[oi]|\\.)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }
ruleHourofdayAndIntegerMinutes :: Rule
ruleHourofdayAndIntegerMinutes = Rule
  { name = "<hour-of-day> and <integer> minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e"
    , Predicate $ isIntegerBetween 1 59
    , regex "min(ut[oi]|\\.)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }
ruleMinutesToIntegerAsHourofday :: Rule
ruleMinutesToIntegerAsHourofday = Rule
  { name = "minutes to <integer> (as hour-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "min(ut[oi]|\\.)? a"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }
ruleHourofdayMinusIntegerMinutes :: Rule
ruleHourofdayMinusIntegerMinutes = Rule
  { name = "<hour-of-day> minus <integer> minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "meno"
    , Predicate $ isIntegerBetween 1 59
    , regex "min(ut[oi]|\\.)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       _:
       token:
       _) -> do
         v <- getIntValue token
         Token Time <$> minutesBefore v td
      _ -> Nothing
  }
ruleHhIntegerminutesDelPomeriggiotimeofday :: Rule
ruleHhIntegerminutesDelPomeriggiotimeofday = Rule
  { name = "hh <integer> minutes del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    , regex "min(ut[oi]|\\.)? d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       token:
       _) -> do
         v <- getIntValue token
         let h = if hours > 12 then hours else hours + 12
         tt $ hourMinute False h v
      _ -> Nothing
  }

ruleHhQuartDelPomeriggiotimeofday2 :: Rule
ruleHhQuartDelPomeriggiotimeofday2 = Rule
  { name = "hh quart del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e un quarto d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) ->
         let h = if hours > 12 then hours else hours + 12
         in tt $ hourMinute False h 15
      _ -> Nothing
  }
ruleHourofdayQuart :: Rule
ruleHourofdayQuart = Rule
  { name = "<hour-of-day> quart"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "un quarto"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }
ruleHourofdayAndAQuart :: Rule
ruleHourofdayAndAQuart = Rule
  { name = "<hour-of-day> and a quart"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e un quarto"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }
ruleQuartAsHourofday :: Rule
ruleQuartAsHourofday = Rule
  { name = "a quart to <integer> (as hour-of-day)"
  , pattern =
    [ regex "un quarto a"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }
ruleHourofdayMinusQuart :: Rule
ruleHourofdayMinusQuart = Rule
  { name = "<hour-of-day> minus quart"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "meno un quarto"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }
ruleHhQuartDelPomeriggiotimeofday :: Rule
ruleHhQuartDelPomeriggiotimeofday = Rule
  { name = "hh quart del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "un quarto d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) ->
         let h = if hours > 12 then hours else hours + 12
         in tt $ hourMinute False h 15
      _ -> Nothing
  }

ruleHhHalfDelPomeriggiotimeofday2 :: Rule
ruleHhHalfDelPomeriggiotimeofday2 = Rule
  { name = "hh half del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e mezzo d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) ->
         let h = if hours > 12 then hours else hours + 12
         in tt $ hourMinute False h 30
      _ -> Nothing
  }
ruleHourofdayHalf :: Rule
ruleHourofdayHalf = Rule
  { name = "<hour-of-day> half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "mezzo"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }
ruleHourofdayAndHalf :: Rule
ruleHourofdayAndHalf = Rule
  { name = "<hour-of-day> and half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e mezzo"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }
ruleHalfToIntegerAsHourofday :: Rule
ruleHalfToIntegerAsHourofday = Rule
  { name = "half to <integer> (as hour-of-day)"
  , pattern =
    [ regex "mezzo a"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }
ruleHourofdayMinusHalf :: Rule
ruleHourofdayMinusHalf = Rule
  { name = "<hour-of-day> minus half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "meno mezzo"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }
ruleHhHalfDelPomeriggiotimeofday :: Rule
ruleHhHalfDelPomeriggiotimeofday = Rule
  { name = "hh half del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "mezzo d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) ->
         let h = if hours > 12 then hours else hours + 12
         in tt $ hourMinute False h 30
      _ -> Nothing
  }

ruleHhThreeQuarterDelPomeriggiotimeofday2 :: Rule
ruleHhThreeQuarterDelPomeriggiotimeofday2 = Rule
  { name = "hh three quarters del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e (3|tre) quarti? d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) ->
         let h = if hours > 12 then hours else hours + 12
         in tt $ hourMinute False h 45
      _ -> Nothing
  }
ruleHourofdayThreeQuarters :: Rule
ruleHourofdayThreeQuarters = Rule
  { name = "<hour-of-day> three quarters"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(3|tre) quarti?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 45
      _ -> Nothing
  }
ruleHourofdayAndThreeQuarter :: Rule
ruleHourofdayAndThreeQuarter = Rule
  { name = "<hour-of-day> and three quarters"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e (3|tre) quarti?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 45
      _ -> Nothing
  }
ruleThreeQuarterToIntegerAsHourofday :: Rule
ruleThreeQuarterToIntegerAsHourofday = Rule
  { name = "three quarter to <integer> (as hour-of-day)"
  , pattern =
    [ regex "(3|tre) quarti? a"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 45 td
      _ -> Nothing
  }
ruleHourofdayMinusThreeQuarter :: Rule
ruleHourofdayMinusThreeQuarter = Rule
  { name = "<hour-of-day> minus three quarter"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "meno (3|tre) quarti?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 45 td
      _ -> Nothing
  }
ruleHhThreeQuarterDelPomeriggiotimeofday :: Rule
ruleHhThreeQuarterDelPomeriggiotimeofday = Rule
  { name = "hh three quarter del pomeriggio(time-of-day)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(3|tre) quarti? d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       _) ->
         let h = if hours > 12 then hours else hours + 12
         in tt $ hourMinute False h 45
      _ -> Nothing
  }

ruleHhhmmTimeofday :: Rule
ruleHhhmmTimeofday = Rule
  { name = "hh(:|h)mm (time-of-day)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:h]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute False h m
      _ -> Nothing
  }

ruleDalIntAlInt :: Rule
ruleDalIntAlInt = Rule
  { name = "dal <integer> al <integer> (interval)"
  , pattern =
    [ regex "dal(?:l')?"
    , Predicate isDOMInteger
    , regex "([fs]ino )?al(l')?"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token1:_:token2:_) -> do
         v1 <- getIntValue token1
         v2 <- getIntValue token2
         Token Time <$>
           interval TTime.Closed (dayOfMonth v1) (dayOfMonth v2)
      _ -> Nothing
  }

ruleTraIlIntEIlInt :: Rule
ruleTraIlIntEIlInt = Rule
  { name = "tra il <integer> e il <integer> (interval)"
  , pattern =
    [ regex "tra( (il|l'))?"
    , Predicate isDOMInteger
    , regex "e( (il|l'))?"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token1:_:token2:_) -> do
         v1 <- getIntValue token1
         v2 <- getIntValue token2
         Token Time <$>
           interval TTime.Closed (dayOfMonth v1) (dayOfMonth v2)
      _ -> Nothing
  }

ruleTimeofdayOra :: Rule
ruleTimeofdayOra = Rule
  { name = "<time-of-day> ora"
  , pattern =
    [ regex "or[ea]"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleNextTime2 :: Rule
ruleNextTime2 = Rule
  { name = "next <time>"
  , pattern =
    [ dimension Time
    , regex "dopo|prossim[ao]"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 False td
      _ -> Nothing
  }

ruleSantoStefano :: Rule
ruleSantoStefano = Rule
  { name = "santo stefano"
  , pattern =
    [ regex "s(anto|\\.) stefano"
    ]
  , prod = \_ -> tt $ monthDay 12 26
  }

ruleIlTime :: Rule
ruleIlTime = Rule
  { name = "il <time>"
  , pattern =
    [ regex "il|l'"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleFestaDelPap :: Rule
ruleFestaDelPap = Rule
  { name = "festa del papà"
  , pattern =
    [ regex "festa del pap(a|à)|(festa di )?s(an|\\.) giuseppe"
    ]
  , prod = \_ -> tt $ monthDay 3 19
  }

ruleEntroIlDuration :: Rule
ruleEntroIlDuration = Rule
  { name = "entro il <duration>"
  , pattern =
    [ regex "(entro|durante|per( tutt[ao])?) (il|l[a'])|in|nel(l[a'])?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        Token Time <$> interval TTime.Open now (cycleNth grain 1)
      _ -> Nothing
  }

ruleDimTimeAlPartofday :: Rule
ruleDimTimeAlPartofday = Rule
  { name = "<dim time> al <part-of-day>"
  , pattern =
    [ dimension Time
    , regex "al|nel(la)?|in|d(i|el(la)?)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleSeason4 :: Rule
ruleSeason4 = Rule
  { name = "season"
  , pattern =
    [ regex "(in )?primavera"
    ]
  , prod = \_ ->
      let from = monthDay 3 20
          to = monthDay 6 21
      in Token Time <$> interval TTime.Open from to
  }

ruleYearLatent2 :: Rule
ruleYearLatent2 = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 2101 10000
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ year v
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "mezz?ogiorno"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleTheDayBeforeYesterday :: Rule
ruleTheDayBeforeYesterday = Rule
  { name = "the day before yesterday"
  , pattern =
    [ regex "(l')?altro\\s?ieri"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
  }

ruleNextCycle :: Rule
ruleNextCycle = Rule
  { name = "next <cycle> "
  , pattern =
    [ regex "((il|la|[nd]el(la)?) )?prossim[oa]"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Lunedi"   , "luned(i|ì)|lun?\\.?"   )
  , ( "Martedi"  , "marted(i|ì)|mar\\.?"   )
  , ( "Mercoledi", "mercoled(i|ì)|mer\\.?" )
  , ( "Giovedi"  , "gioved(i|ì)|gio\\.?"   )
  , ( "Venerdi"  , "venerd(i|ì)|ven\\.?"   )
  , ( "Sabato"   , "sabato|sab\\.?"        )
  , ( "Domenica" , "domenica|dom\\.?"      )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Gennaio"  , "gennaio|genn?\\.?"   )
  , ( "Febbraio" , "febbraio|febb?\\.?"  )
  , ( "Marzo"    , "marzo|mar\\.?"       )
  , ( "Aprile"   , "aprile|apr\\.?"      )
  , ( "Maggio"   , "maggio|magg?\\.?"    )
  , ( "Giugno"   , "giugno|giu\\.?"      )
  , ( "Luglio"   , "luglio|lug\\.?"      )
  , ( "Agosto"   , "agosto|ago\\.?"      )
  , ( "Settembre", "settembre|sett?\\.?" )
  , ( "Ottobre"  , "ottobre|ott\\.?"     )
  , ( "Novembre" , "novembre|nov\\.?"    )
  , ( "Dicembre" , "dicembre|dic\\.?"    )
  ]

ruleTheCycleOfTime :: Rule
ruleTheCycleOfTime = Rule
  { name = "the <cycle> of <time>"
  , pattern =
    [ regex "il|la"
    , dimension TimeGrain
    , regex "del"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain 0 td
      _ -> Nothing
  }

ruleLunch :: Rule
ruleLunch = Rule
  { name = "lunch"
  , pattern =
    [ regex "(a )?pranzo"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 14
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd[/-]mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/-](0?[1-9]|1[0-2])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        d <- parseInt m1
        m <- parseInt m2
        tt $ monthDay m d
      _ -> Nothing
  }

ruleGliUltimiNCycle :: Rule
ruleGliUltimiNCycle = Rule
  { name = "gli ultimi <n> <cycle>"
  , pattern =
    [ regex "((([nd]el)?le|([nd]e)?gli) )?(scors|ultim)[ei]"
    , Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "pomeriggio?"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

rulePartofdayOfDimTime :: Rule
rulePartofdayOfDimTime = Rule
  { name = "<part-of-day> of <dim time>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "d(i|el)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleDimTimeDelMattino :: Rule
ruleDimTimeDelMattino = Rule
  { name = "<dim time> del mattino"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "del mattino"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        let from = hour False 0
            to = hour False 12
        td2 <- mkLatent . partOfDay <$> interval TTime.Open from to
        Token Time <$> intersect td td2
      _ -> Nothing
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "midnight"
  , pattern =
    [ regex "mez?zanott?e"
    ]
  , prod = \_ -> tt $ hour False 0
  }

ruleChristmasEve :: Rule
ruleChristmasEve = Rule
  { name = "christmas eve"
  , pattern =
    [ regex "((al)?la )?vigig?lia( di natale)?"
    ]
  , prod = \_ -> tt $ monthDay 12 24
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "dopo"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> tt $ predNthAfter (v - 1) td1 td2
      _ -> Nothing
  }

ruleHhhmmTimeofday2 :: Rule
ruleHhhmmTimeofday2 = Rule
  { name = "hh(:|h)mm (time-of-day)"
  , pattern =
    [ regex "((?:0?\\d)|(?:1[0-2]))[:h]([0-5]\\d) d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- parseInt m1
        v2 <- parseInt m2
        tt $ hourMinute False (v1 + 12) v2
      _ -> Nothing
  }

ruleTimeNotte :: Rule
ruleTimeNotte = Rule
  { name = "<time> notte"
  , pattern =
    [ dimension Time
    , regex "((in|nella|alla) )?nott(e|ata)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        let td1 = cycleNthAfter False TG.Day 1 td
        td2 <- interval TTime.Open (hour False 0) (hour False 4)
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleStasera :: Rule
ruleStasera = Rule
  { name = "stasera"
  , pattern =
    [ regex "stasera"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect today td2
  }

ruleSeason3 :: Rule
ruleSeason3 = Rule
  { name = "season"
  , pattern =
    [ regex "(in )?inverno"
    ]
  , prod = \_ ->
      let from = monthDay 12 21
          to = monthDay 3 20
      in Token Time <$> interval TTime.Open from to
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "season"
  , pattern =
    [ regex "(in )?estate"
    ]
  , prod = \_ ->
      let from = monthDay 6 21
          to = monthDay 9 23
       in Token Time <$> interval TTime.Open from to
  }

ruleIntegerLatentTimeofday :: Rule
ruleIntegerLatentTimeofday = Rule
  { name = "<integer> (latent time-of-day)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 24
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ hour False v
      _ -> Nothing
  }

ruleInThePartofdayOfDimTime :: Rule
ruleInThePartofdayOfDimTime = Rule
  { name = "in the <part-of-day> of <dim time>"
  , pattern =
    [ regex "nel(la)?"
    , Predicate isAPartOfDay
    , regex "d(i|el)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleNewYearsEve :: Rule
ruleNewYearsEve = Rule
  { name = "new year's eve"
  , pattern =
    [ regex "((la )?vigig?lia di capodanno|san silvestro)"
    ]
  , prod = \_ -> tt $ monthDay 12 31
  }

ruleFerragosto :: Rule
ruleFerragosto = Rule
  { name = "ferragosto"
  , pattern =
    [ regex "ferragosto|assunzione"
    ]
  , prod = \_ -> tt $ monthDay 8 15
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ dimension Duration
    , regex "fa"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleTimeNotte2 :: Rule
ruleTimeNotte2 = Rule
  { name = "<time> notte"
  , pattern =
    [ regex "((nella|alla) )?nott(e|ata)( d(i|el))"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> do
        let td1 = cycleNthAfter False TG.Day 1 td
        td2 <- interval TTime.Open (hour False 0) (hour False 4)
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleTimeofdayPrecise :: Rule
ruleTimeofdayPrecise = Rule
  { name = "<time-of-day> precise"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "precise"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleHhmmMilitaryTimeofday :: Rule
ruleHhmmMilitaryTimeofday = Rule
  { name = "hhmm (military time-of-day)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt . mkLatent $ hourMinute False h m
      _ -> Nothing
  }

ruleTheCycleLast :: Rule
ruleTheCycleLast = Rule
  { name = "the <cycle> last"
  , pattern =
    [ regex "l'|il|la|[nd]el(la)?"
    , dimension TimeGrain
    , regex "(scors|passat)[oa]"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleFinoAlDatetimeInterval :: Rule
ruleFinoAlDatetimeInterval = Rule
  { name = "fino al <datetime> (interval)"
  , pattern =
    [ regex "[fs]ino a(l(l[ae'])?)?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "(al)?l[e']|a"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleTheNthTimeOfTime :: Rule
ruleTheNthTimeOfTime = Rule
  { name = "the nth <time> of <time>"
  , pattern =
    [ regex "il|l[a']|[nd]el(l[a'])?"
    , dimension Ordinal
    , dimension Time
    , regex "di|del(l[a'])?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
         predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "di|del(l[a'])?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
         predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleDalDatetimeAlDatetimeInterval :: Rule
ruleDalDatetimeAlDatetimeInterval = Rule
  { name = "dal <datetime> al <datetime> (interval)"
  , pattern =
    [ regex "da(l(l')?)?"
    , Predicate isNotLatent
    , regex "([fs]ino )?a(l(l')?)?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleTimeofdayTimeofdayDayofmonthInterval :: Rule
ruleTimeofdayTimeofdayDayofmonthInterval = Rule
  { name = "<time-of-day> - <time-of-day> <day-of-month> (interval)"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "\\-"
    , Predicate isATimeOfDay
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:Token Time td3:_) -> do
        from <- intersect td1 td3
        to <- intersect td2 td3
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "week[ -]?end|fine ?settimana|we"
    ]
  , prod = \_ -> tt weekend
  }

ruleIlWeekendDelTime :: Rule
ruleIlWeekendDelTime = Rule
  { name = "il week-end del <time>"
  , pattern =
    [ regex "il (week[ -]?end|fine ?settimana|we) del"
    , Predicate $ isGrainOfTime TG.Day
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> do
        from1 <- intersect (cycleNthAfter False TG.Week 0 td) (dayOfWeek 5)
        from <- intersect from1 (hour False 18)
        to1 <- intersect (cycleNthAfter False TG.Week 1 td) (dayOfWeek 1)
        to <- intersect to1 (hour False 0)
        Token Time <$> interval TTime.Open from to
      _ -> Nothing
  }

ruleEomendOfMonth :: Rule
ruleEomendOfMonth = Rule
  { name = "EOM|End of month"
  , pattern =
    [ regex "fine del mese"
    ]
  , prod = \_ -> tt $ cycleNth TG.Month 1
  }

ruleCommemorazioneDeiDefunti :: Rule
ruleCommemorazioneDeiDefunti = Rule
  { name = "commemorazione dei defunti"
  , pattern =
    [ regex "(((giorno|commemorazione) dei|ai) )?(morti|defunti)"
    ]
  , prod = \_ -> tt $ monthDay 11 2
  }

ruleImmacolataConcezione :: Rule
ruleImmacolataConcezione = Rule
  { name = "immacolata concezione"
  , pattern =
    [ regex "(all')?immacolata( concezione)?"
    ]
  , prod = \_ -> tt $ monthDay 12 8
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

ruleTraIlDatetimeEIlDatetimeInterval :: Rule
ruleTraIlDatetimeEIlDatetimeInterval = Rule
  { name = "tra il <datetime> e il <datetime> (interval)"
  , pattern =
    [ regex "tra( il| l')?"
    , Predicate isATimeOfDay
    , regex "e( il| l')?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNthTimeAfterTime2 :: Rule
ruleNthTimeAfterTime2 = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ regex "il|l'"
    , dimension Ordinal
    , dimension Time
    , regex "dopo"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleDopoLeTimeofday :: Rule
ruleDopoLeTimeofday = Rule
  { name = "dopo le <time-of-day>"
  , pattern =
    [ regex "dopo( l['ea'])?|dal(l['ea'])?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt . withDirection TTime.After $ notLatent td
      _ -> Nothing
  }

ruleDopoTime :: Rule
ruleDopoTime = Rule
  { name = "dopo <time>"
  , pattern =
    [ regex "dopo|dal?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleDalInt :: Rule
ruleDalInt = Rule
  { name = "dal <integer"
  , pattern =
    [ regex "dal"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt . withDirection TTime.After $ dayOfMonth v
      _ -> Nothing
  }

ruleTimeEntroLeTime :: Rule
ruleTimeEntroLeTime = Rule
  { name = "<time> entro le <time>"
  , pattern =
    [ dimension Time
    , regex "entro( l[e'])?|prima d(i|ell['ea])"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time . withDirection TTime.Before <$> intersect td1 td2
      _ -> Nothing
  }

ruleEntroTime :: Rule
ruleEntroTime = Rule
  { name = "entro <time>"
  , pattern =
    [ regex "entro( la)?|prima d(i|ella)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleEntroIlInt :: Rule
ruleEntroIlInt = Rule
  { name = "entro il <integer>"
  , pattern =
    [ regex "entro il|prima del"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt . withDirection TTime.Before $ dayOfMonth v
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "prossim[ao]"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 1 False td
      _ -> Nothing
  }

ruleNthTimeOfTime3 :: Rule
ruleNthTimeOfTime3 = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ regex "il|l'"
    , dimension Ordinal
    , dimension Time
    , regex "di|del(l[oa'])|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
         predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleIlCycleDopoTime :: Rule
ruleIlCycleDopoTime = Rule
  { name = "il <cycle> dopo <time>"
  , pattern =
    [ regex "l[a']|il|[nd]el"
    , dimension TimeGrain
    , regex "dopo"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
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
    [ regex "((([nd]e)?i|([nd]el)?le) )?prossim[ei]"
    , Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "mattin(ata|[aoe])"
    ]
  , prod = \_ ->
      let from = hour False 4
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleTheDayofmonth :: Rule
ruleTheDayofmonth = Rule
  { name = "il <day-of-month>"
  , pattern =
    [ regex "il|l'"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "(que)?st[oa]|i[nl]|(al|nel)(la)?|la"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay <$> intersect today td
      _ -> Nothing
  }

ruleThisCycle :: Rule
ruleThisCycle = Rule
  { name = "this <cycle>"
  , pattern =
    [ regex "(in )?quest['oa]|per (il|l['a])"
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
    [ regex "quest[oaie']"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleTwoTimeTokensSeparatedByDi :: Rule
ruleTwoTimeTokensSeparatedByDi = Rule
  { name = "two time tokens separated by `di`"
  , pattern =
    [ Predicate isNotLatent
    , regex "di"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleTimeofdayCirca :: Rule
ruleTimeofdayCirca = Rule
  { name = "<time-of-day> circa"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "circa"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween (- 10000) (-1)
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
    [ regex "ieri"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleSeason2 :: Rule
ruleSeason2 = Rule
  { name = "season"
  , pattern =
    [ regex "(in )?autunno"
    ]
  , prod = \_ ->
      let from = monthDay 9 23
          to = monthDay 12 21
      in Token Time <$> interval TTime.Open from to
  }

ruleCircaPerLeTimeofday :: Rule
ruleCircaPerLeTimeofday = Rule
  { name = "circa per le <time-of-day>"
  , pattern =
    [ regex "(circa )?per( le)?|circa( alle)?|verso( le)?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleVersoPartOfDay :: Rule
ruleVersoPartOfDay = Rule
  { name = "verso <part-of-day>"
  , pattern =
    [ regex "verso( la| il)?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleChristmas :: Rule
ruleChristmas = Rule
  { name = "christmas"
  , pattern =
    [ regex "((il )?giorno di )?natale"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleNight :: Rule
ruleNight = Rule
  { name = "night"
  , pattern =
    [ regex "nott(e|ata)"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day 1
      td2 <- interval TTime.Open (hour False 0) (hour False 4)
      Token Time . partOfDay . mkLatent <$> intersect td1 td2
  }

ruleOgnissanti :: Rule
ruleOgnissanti = Rule
  { name = "ognissanti"
  , pattern =
    [ regex "(tutti i |ognis|festa dei |([ia]l )?giorno dei )santi"
    ]
  , prod = \_ -> tt $ monthDay 11 1
  }

ruleIntegerDelPartOfDay :: Rule
ruleIntegerDelPartOfDay = Rule
  { name = "<integer 0 12> del <part of day>"
  , pattern =
    [ Predicate $ isIntegerBetween 0 12
    , regex "d(i|el(la)?) (pomeriggio|(sta)?(sera|notte))"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        tt $ hour False (12 + floor v)
      _ -> Nothing
  }

ruleOrdinalCycleOfTime :: Rule
ruleOrdinalCycleOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "di|del(l[a'])?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleGliNUltimiCycle :: Rule
ruleGliNUltimiCycle = Rule
  { name = "gli <n> ultimi <cycle>"
  , pattern =
    [ regex "([nd]e)?i|([nd]el)?le"
    , Predicate $ isIntegerBetween 2 9999
    , regex "(scors|ultim)[ei]"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:_:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleDalleTimeofdayAlleTimeofdayInterval :: Rule
ruleDalleTimeofdayAlleTimeofdayInterval = Rule
  { name = "dalle <time-of-day> alle <time-of-day> (interval)"
  , pattern =
    [ regex "da(ll[ae'])?"
    , Predicate isATimeOfDay
    , regex "\\-|([fs]ino )?a(ll[ae'])?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleEntroLeTimeofday :: Rule
ruleEntroLeTimeofday = Rule
  { name = "entro le <time-of-day>"
  , pattern =
    [ regex "entro( l[ea'])?|prima d(i|ell['e])"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt . withDirection TTime.Before $ notLatent td
      _ -> Nothing
  }

ruleHalloweenDay :: Rule
ruleHalloweenDay = Rule
  { name = "halloween day"
  , pattern =
    [ regex "hall?owe?en"
    ]
  , prod = \_ -> tt $ monthDay 10 31
  }

ruleLastDayofweekOfTime :: Rule
ruleLastDayofweekOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "(([nd]el)?l')?ultim[oa]"
    , Predicate isADayOfWeek
    , regex "di|del(l[a'])?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleNameddayDayofmonth :: Rule
ruleNameddayDayofmonth = Rule
  { name = "<named-day> <day-of-month>"
  , pattern =
    [ Predicate isADayOfWeek
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleLastDayofweekOfTime2 :: Rule
ruleLastDayofweekOfTime2 = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "(l')ultim[oa]"
    , Predicate isADayOfWeek
    , regex "di"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd[/-]mm[/-]yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/-](0?[1-9]|1[0-2])[/-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m3
        m <- parseInt m2
        d <- parseInt m1
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTwoTimeTokensInARow :: Rule
ruleTwoTimeTokensInARow = Rule
  { name = "two time tokens in a row"
  , pattern =
    [ Predicate isNotLatent
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleIleNCyclePassatipassate :: Rule
ruleIleNCyclePassatipassate = Rule
  { name = "i|le n <cycle> passati|passate"
  , pattern =
    [ regex "([nd]e)?i|([nd]el)?le"
    , Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(scors|passat)[ie]"
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleEoyendOfYear :: Rule
ruleEoyendOfYear = Rule
  { name = "EOY|End of year"
  , pattern =
    [ regex "fine dell' ?anno"
    ]
  , prod = \_ -> tt $ cycleNth TG.Year 1
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "domani"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleNextNCycle2 :: Rule
ruleNextNCycle2 = Rule
  { name = "next n <cycle>"
  , pattern =
    [ regex "([nd]e)?i|([nd]el)?le"
    , Predicate $ isIntegerBetween 2 9999
    , regex "prossim[ei]"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:_:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleProssimiUnitofduration :: Rule
ruleProssimiUnitofduration = Rule
  { name = "prossimi <unit-of-duration>"
  , pattern =
    [ regex "((([nd]e)?i|([nd]el)?le) )?prossim[ie]"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        let from = cycleNth grain 1
            to = cycleNth grain 3
        in Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleMothersDay :: Rule
ruleMothersDay = Rule
  { name = "Mother's Day"
  , pattern =
    [ regex "festa della mamma"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 2 7 5
  }

ruleFestaDelLavoro :: Rule
ruleFestaDelLavoro = Rule
  { name = "festa del lavoro"
  , pattern =
    [ regex "festa del lavoro|(festa|giorno) dei lavoratori"
    ]
  , prod = \_ -> tt $ monthDay 5 1
  }

ruleIntersectByDiDellaDel :: Rule
ruleIntersectByDiDellaDel = Rule
  { name = "intersect by \"di\", \"della\", \"del\""
  , pattern =
    [ Predicate isNotLatent
    , regex "di|del(la)?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
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
  [ ruleAfternoon
  , ruleAtTimeofday
  , ruleChristmas
  , ruleChristmasEve
  , ruleCircaPerLeTimeofday
  , ruleCommemorazioneDeiDefunti
  , ruleDalDatetimeAlDatetimeInterval
  , ruleDalleTimeofdayAlleTimeofdayInterval
  , ruleDatetimeDatetimeInterval
  , ruleDayOfMonthSt
  , ruleDayofmonthNamedmonth
  , ruleDdddMonthInterval
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleDimTimeAlPartofday
  , ruleDimTimeDelMattino
  , ruleDimTimePartofday
  , ruleDopoLeTimeofday
  , ruleDurationAgo
  , ruleEntroIlDuration
  , ruleEntroLeTimeofday
  , ruleEomendOfMonth
  , ruleEoyendOfYear
  , ruleEpifania
  , ruleEvening
  , ruleFerragosto
  , ruleFestaDelLavoro
  , ruleFestaDelPap
  , ruleFestaDellaLiberazione
  , ruleFestaDellaRepubblica
  , ruleFinoAlDatetimeInterval
  , ruleGliNUltimiCycle
  , ruleGliUltimiNCycle
  , ruleHalloweenDay
  , ruleHhRelativeminutesDelPomeriggiotimeofday
  , ruleHhRelativeminutesDelPomeriggiotimeofday2
  , ruleHhhmmTimeofday
  , ruleHhhmmTimeofday2
  , ruleHhmmMilitaryTimeofday
  , ruleHourofdayAndRelativeMinutes
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleHourofdayMinusIntegerAsRelativeMinutes
  , ruleIlCycleDopoTime
  , ruleIlDayofmonthDeNamedmonth
  , ruleIlTime
  , ruleIleNCyclePassatipassate
  , ruleInDuration
  , ruleInNamedmonth
  , ruleInThePartofdayOfDimTime
  , ruleInafterDuration
  , ruleIntegerDelPartOfDay
  , ruleIntegerLatentTimeofday
  , ruleIntersectByDiDellaDel
  , ruleLastCycleOfTime
  , ruleLastDayofweekOfTime
  , ruleLastDayofweekOfTime2
  , ruleLastTime
  , ruleLeIdiDiNamedmonth
  , ruleLunch
  , ruleMidnight
  , ruleMorning
  , ruleMothersDay
  , ruleNameddayDayofmonth
  , ruleNewYearsDay
  , ruleNewYearsEve
  , ruleNextCycle
  , ruleNextNCycle
  , ruleNextNCycle2
  , ruleNextTime
  , ruleNextTime2
  , ruleNight
  , ruleNoon
  , ruleNthTimeAfterTime
  , ruleNthTimeAfterTime2
  , ruleNthTimeOfTime
  , ruleNthTimeOfTime2
  , ruleNthTimeOfTime3
  , ruleOgnissanti
  , ruleOrdinalCycleOfTime
  , rulePartofdayOfDimTime
  , ruleProssimiUnitofduration
  , ruleRelativeMinutesToIntegerAsHourofday
  , ruleRightNow
  , ruleSantoStefano
  , ruleSeason
  , ruleSeason2
  , ruleSeason3
  , ruleSeason4
  , ruleStamattina
  , ruleStanotte
  , ruleStasera
  , ruleTheCycleLast
  , ruleTheCycleNext
  , ruleTheCycleOfTime
  , ruleTheDayAfterTomorrow
  , ruleTheDayBeforeYesterday
  , ruleTheDayofmonth
  , ruleTheLastCycle
  , ruleTheNthTimeOfTime
  , ruleTheOrdinalCycleOfTime
  , ruleThisCycle
  , ruleThisDayofweek
  , ruleThisPartofday
  , ruleThisTime
  , ruleTimeLast
  , ruleTimeNotte
  , ruleTimeNotte2
  , ruleTimeofdayCirca
  , ruleTimeofdayOra
  , ruleTimeofdayPrecise
  , ruleTimeofdayTimeofdayDayofmonthInterval
  , ruleTomorrow
  , ruleTraIlDatetimeEIlDatetimeInterval
  , ruleTwoTimeTokensInARow
  , ruleTwoTimeTokensSeparatedByDi
  , ruleUna
  , ruleValentinesDay
  , ruleWeekend
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYearNotLatent
  , ruleYesterday
  , ruleYyyymmdd
  , ruleHhThreeQuarterDelPomeriggiotimeofday
  , ruleHhThreeQuarterDelPomeriggiotimeofday2
  , ruleHourofdayMinusThreeQuarter
  , ruleThreeQuarterToIntegerAsHourofday
  , ruleHourofdayAndThreeQuarter
  , ruleHourofdayThreeQuarters
  , ruleHhQuartDelPomeriggiotimeofday
  , ruleHhQuartDelPomeriggiotimeofday2
  , ruleHourofdayMinusQuart
  , ruleQuartAsHourofday
  , ruleHourofdayAndAQuart
  , ruleHourofdayQuart
  , ruleHhHalfDelPomeriggiotimeofday
  , ruleHhHalfDelPomeriggiotimeofday2
  , ruleHourofdayMinusHalf
  , ruleHalfToIntegerAsHourofday
  , ruleHourofdayAndHalf
  , ruleHourofdayHalf
  , ruleHhIntegerminutesDelPomeriggiotimeofday
  , ruleHhIntegerminutesDelPomeriggiotimeofday2
  , ruleHourofdayMinusIntegerMinutes
  , ruleMinutesToIntegerAsHourofday
  , ruleHourofdayAndIntegerMinutes
  , ruleHourofdayIntegerMinutes
  , ruleTimezone
  , ruleTimeEntroLeTime
  , ruleImmacolataConcezione
  , ruleOrdinalQuarter
  , ruleTheOrdinalQuarter
  , ruleCycleOrdinalQuarterYear
  , ruleCycleTheOrdinalTime
  , ruleCycleNext
  , ruleDomattina
  , ruleVersoPartOfDay
  , ruleEntroIlInt
  , ruleEntroTime
  , ruleDalInt
  , ruleDopoTime
  , ruleToday
  , ruleDalIntAlInt
  , ruleTraIlIntEIlInt
  , ruleIlWeekendDelTime
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
