-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ES.Rules
  ( rules ) where

import Control.Monad (liftM2)
import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Types (OrdinalData (..))
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
    [ regex "lunes|lun?\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 1
  }

ruleTheDayAfterTomorrow :: Rule
ruleTheDayAfterTomorrow = Rule
  { name = "the day after tomorrow"
  , pattern =
    [ regex "pasado\\s?ma(n|\x00f1)ana"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleHaceDuration :: Rule
ruleHaceDuration = Rule
  { name = "hace <duration>"
  , pattern =
    [ regex "hace"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleNamedmonth12 :: Rule
ruleNamedmonth12 = Rule
  { name = "named-month"
  , pattern =
    [ regex "diciembre|dic\\.?"
    ]
  , prod = \_ -> tt $ month 12
  }

ruleCeTime :: Rule
ruleCeTime = Rule
  { name = "ce <time>"
  , pattern =
    [ regex "este"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNamedday2 :: Rule
ruleNamedday2 = Rule
  { name = "named-day"
  , pattern =
    [ regex "martes|mar?\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 2
  }

ruleThisDayofweek :: Rule
ruleThisDayofweek = Rule
  { name = "this <day-of-week>"
  , pattern =
    [ regex "este"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleNamedday6 :: Rule
ruleNamedday6 = Rule
  { name = "named-day"
  , pattern =
    [ regex "s(\x00e1|a)bado|s(\x00e1|a)b\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 6
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|al?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Open td1 td2
      _ -> Nothing
  }

ruleNamedmonth7 :: Rule
ruleNamedmonth7 = Rule
  { name = "named-month"
  , pattern =
    [ regex "julio|jul\\.?"
    ]
  , prod = \_ -> tt $ month 7
  }

ruleEvening :: Rule
ruleEvening = Rule
  { name = "evening"
  , pattern =
    [ regex "noche"
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
    [ regex "primero|uno|prem\\.?|1o"
    ]
  , prod = \_ -> tt $ dayOfMonth 1
  }

ruleEnDuration :: Rule
ruleEnDuration = Rule
  { name = "en <duration>"
  , pattern =
    [ regex "en"
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
    [ regex "(hoy)|(en este momento)"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 0
  }

ruleUltimoDayofweekDeTime :: Rule
ruleUltimoDayofweekDeTime = Rule
  { name = "ultimo <day-of-week> de <time>"
  , pattern =
    [ regex "(\x00fa|u)ltimo"
    , Predicate isADayOfWeek
    , regex "de|en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleEntreDatetimeEtDatetimeInterval :: Rule
ruleEntreDatetimeEtDatetimeInterval = Rule
  { name = "entre <datetime> et <datetime> (interval)"
  , pattern =
    [ regex "entre"
    , dimension Time
    , regex "y"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Open td1 td2
      _ -> Nothing
  }

ruleHhhmmTimeofday :: Rule
ruleHhhmmTimeofday = Rule
  { name = "hh(:|.|h)mm (time-of-day)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:h\\.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleNamedday4 :: Rule
ruleNamedday4 = Rule
  { name = "named-day"
  , pattern =
    [ regex "jueves|jue|jue\\."
    ]
  , prod = \_ -> tt $ dayOfWeek 4
  }

ruleElDayofmonthDeNamedmonth :: Rule
ruleElDayofmonthDeNamedmonth = Rule
  { name = "el <day-of-month> de <named-month>"
  , pattern =
    [ regex "el"
    , Predicate isDOMInteger
    , regex "de"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleNPasadosCycle :: Rule
ruleNPasadosCycle = Rule
  { name = "n pasados <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "pasad(a|o)s?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleElProximoCycle :: Rule
ruleElProximoCycle = Rule
  { name = "el proximo <cycle> "
  , pattern =
    [ regex "(el|los|la|las) ?"
    , regex "pr(\x00f3|o)xim(o|a)s?|siguientes?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

rulePasadosNCycle :: Rule
rulePasadosNCycle = Rule
  { name = "pasados n <cycle>"
  , pattern =
    [ regex "pasad(a|o)s?"
    , Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleElDayofmonthNonOrdinal :: Rule
ruleElDayofmonthNonOrdinal = Rule
  { name = "el <day-of-month> (non ordinal)"
  , pattern =
    [ regex "el"
    , Predicate $ isIntegerBetween 1 31
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleSeason4 :: Rule
ruleSeason4 = Rule
  { name = "season"
  , pattern =
    [ regex "primavera"
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
    [ regex "mediod(\x00ed|i)a"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleProximasNCycle :: Rule
ruleProximasNCycle = Rule
  { name = "proximas n <cycle>"
  , pattern =
    [ regex "pr(\x00f3|o)xim(o|a)s?"
    , Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleNochevieja :: Rule
ruleNochevieja = Rule
  { name = "Nochevieja"
  , pattern =
    [ regex "nochevieja"
    ]
  , prod = \_ -> tt $ monthDay 12 31
  }

ruleTheDayBeforeYesterday :: Rule
ruleTheDayBeforeYesterday = Rule
  { name = "the day before yesterday"
  , pattern =
    [ regex "anteayer|antes de (ayer|anoche)|antier"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
  }

ruleHourofdayMinusIntegerAsRelativeMinutes :: Rule
ruleHourofdayMinusIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> minus <integer> (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "menos\\s?"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleHourofdayMinusIntegerAsRelativeMinutes2 :: Rule
ruleHourofdayMinusIntegerAsRelativeMinutes2 = Rule
  { name = "<hour-of-day> minus <integer> (as relative minutes) minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "menos\\s?"
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(uto)?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleHourofdayMinusQuarter :: Rule
ruleHourofdayMinusQuarter = Rule
  { name = "<hour-of-day> minus quarter (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "menos\\s? cuarto"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        t <- minutesBefore 15 td
        Just $ Token Time t
      _ -> Nothing
  }

ruleHourofdayMinusHalf :: Rule
ruleHourofdayMinusHalf = Rule
  { name = "<hour-of-day> minus half (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "menos\\s? media"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        t <- minutesBefore 30 td
        Just $ Token Time t
      _ -> Nothing
  }

ruleHourofdayMinusThreeQuarter :: Rule
ruleHourofdayMinusThreeQuarter = Rule
  { name = "<hour-of-day> minus three quarter (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "menos\\s? (3|tres) cuartos?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        t <- minutesBefore 45 td
        Just $ Token Time t
      _ -> Nothing
  }

ruleHourofdayIntegerAsRelativeMinutes :: Rule
ruleHourofdayIntegerAsRelativeMinutes = Rule
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

ruleHourofdayIntegerAsRelativeMinutes2 :: Rule
ruleHourofdayIntegerAsRelativeMinutes2 = Rule
  { name = "<hour-of-day> <integer> (as relative minutes) minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(uto)?s?"
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
  { name = "<hour-of-day> quarter (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "cuarto"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHourofdayHalf :: Rule
ruleHourofdayHalf = Rule
  { name = "<hour-of-day> half (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "media"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHourofdayThreeQuarter :: Rule
ruleHourofdayThreeQuarter = Rule
  { name = "<hour-of-day> three quarters (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(3|tres) cuartos?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 45
      _ -> Nothing
  }

ruleHourofdayAndRelativeMinutes :: Rule
ruleHourofdayAndRelativeMinutes = Rule
  { name = "<hour-of-day> and <relative minutes>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "y"
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

ruleHourofdayAndRelativeMinutes2 :: Rule
ruleHourofdayAndRelativeMinutes2 = Rule
  { name = "<hour-of-day> and <relative minutes> minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "y"
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(uto)?s?"
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

ruleHourofdayAndQuarter :: Rule
ruleHourofdayAndQuarter = Rule
  { name = "<hour-of-day> and quarter"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "y cuarto"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHourofdayAndHalf :: Rule
ruleHourofdayAndHalf = Rule
  { name = "<hour-of-day> and half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "y media"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHourofdayAndThreeQuarter :: Rule
ruleHourofdayAndThreeQuarter = Rule
  { name = "<hour-of-day> and 3 quarters"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "y (3|tres) cuartos?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 45
      _ -> Nothing
  }

ruleNamedmonth :: Rule
ruleNamedmonth = Rule
  { name = "named-month"
  , pattern =
    [ regex "enero|ene\\.?"
    ]
  , prod = \_ -> tt $ month 1
  }

ruleInThePartofday :: Rule
ruleInThePartofday = Rule
  { name = "in the <part-of-day>"
  , pattern =
    [ regex "(a|en|de|por) la"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleDelYear :: Rule
ruleDelYear = Rule
  { name = "del <year>"
  , pattern =
    [ regex "del( a(\x00f1|n)o)?"
    , Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt $ year v
      _ -> Nothing
  }

ruleNamedmonth3 :: Rule
ruleNamedmonth3 = Rule
  { name = "named-month"
  , pattern =
    [ regex "marzo|mar\\.?"
    ]
  , prod = \_ -> tt $ month 3
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

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "tarde"
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
    [ regex "abril|abr\\.?"
    ]
  , prod = \_ -> tt $ month 4
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "midnight"
  , pattern =
    [ regex "medianoche"
    ]
  , prod = \_ -> tt $ hour False 0
  }

ruleAnoNuevo :: Rule
ruleAnoNuevo = Rule
  { name = "ano nuevo"
  , pattern =
    [ regex "a(n|\x00f1)o nuevo"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleNamedday5 :: Rule
ruleNamedday5 = Rule
  { name = "named-day"
  , pattern =
    [ regex "viernes|vie|vie\\."
    ]
  , prod = \_ -> tt $ dayOfWeek 5
  }

ruleDdddMonthinterval :: Rule
ruleDdddMonthinterval = Rule
  { name = "dd-dd <month>(interval)"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-|al?"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "de"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       _:
       Token RegexMatch (GroupMatch (m2:_)):
       _:
       Token Time td:
       _) -> do
        d1 <- parseInt m1
        d2 <- parseInt m2
        from <- intersect (dayOfMonth d1) td
        to <- intersect (dayOfMonth d2) td
        Token Time <$> interval TTime.Closed from to
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
        v <- getIntValue token
        tt . mkLatent $ hour True v
      _ -> Nothing
  }

ruleNamedmonth2 :: Rule
ruleNamedmonth2 = Rule
  { name = "named-month"
  , pattern =
    [ regex "febrero|feb\\.?"
    ]
  , prod = \_ -> tt $ month 2
  }

ruleNamedmonthnameddayPast :: Rule
ruleNamedmonthnameddayPast = Rule
  { name = "<named-month|named-day> past"
  , pattern =
    [ dimension Time
    , regex "pasad(o|a)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleSeason3 :: Rule
ruleSeason3 = Rule
  { name = "season"
  , pattern =
    [ regex "invierno"
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
    [ regex "verano"
    ]
  , prod = \_ ->
      let from = monthDay 6 21
          to = monthDay 9 23
      in Token Time <$> interval TTime.Open from to
  }

ruleRightNow :: Rule
ruleRightNow = Rule
  { name = "right now"
  , pattern =
    [ regex "ahor(it)?a|ya|en\\s?seguida|cuanto antes"
    ]
  , prod = \_ -> tt $ cycleNth TG.Second 0
  }

ruleDimTimeDeLaTarde :: Rule
ruleDimTimeDeLaTarde = Rule
  { name = "<dim time> de la tarde"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(a|en|de) la tarde"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        tarde <- interval TTime.Open (hour False 12) (hour False 21)
        Token Time <$> intersect td (mkLatent $ partOfDay tarde)
      _ -> Nothing
  }

ruleIntegerInThePartofday :: Rule
ruleIntegerInThePartofday = Rule
  { name = "<integer> in the <part-of-day>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "(a|en|de|por) la"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleNCycleProximoqueViene :: Rule
ruleNCycleProximoqueViene = Rule
  { name = "n <cycle> (proximo|que viene)"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(pr(\x00f3|o)xim(o|a)s?|que vienen?|siguientes?)"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleNamedmonthnameddayNext :: Rule
ruleNamedmonthnameddayNext = Rule
  { name = "<named-month|named-day> next"
  , pattern =
    [ dimension Time
    , regex "que vienen?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 False td
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

ruleTimeofdayPartofday :: Rule
ruleTimeofdayPartofday = Rule
  { name = "<time-of-day> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleDimTimeDeLaManana :: Rule
ruleDimTimeDeLaManana = Rule
  { name = "<dim time> de la manana"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(a|en|de) la ma(\x00f1|n)ana"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        manana <- interval TTime.Open (hour False 0) (hour False 12)
        Token Time <$> intersect td (mkLatent $ partOfDay manana)
      _ -> Nothing
  }

ruleDeDatetimeDatetimeInterval :: Rule
ruleDeDatetimeDatetimeInterval = Rule
  { name = "de <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "del?"
    , dimension Time
    , regex "\\-|al?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Open td1 td2
      _ -> Nothing
  }

ruleNthTimeDeTime2 :: Rule
ruleNthTimeDeTime2 = Rule
  { name = "nth <time> de <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension Time
    , regex "de|en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Ordinal (OrdinalData {TOrdinal.value = v}):
       Token Time td1:
       _:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNamedmonth6 :: Rule
ruleNamedmonth6 = Rule
  { name = "named-month"
  , pattern =
    [ regex "junio|jun\\.?"
    ]
  , prod = \_ -> tt $ month 6
  }

ruleDentroDeDuration :: Rule
ruleDentroDeDuration = Rule
  { name = "dentro de <duration>"
  , pattern =
    [ regex "dentro de"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        Token Time <$>
          interval TTime.Open (cycleNth TG.Second 0) (inDuration dd)
      _ -> Nothing
  }

ruleNamedmonth8 :: Rule
ruleNamedmonth8 = Rule
  { name = "named-month"
  , pattern =
    [ regex "agosto|ago\\.?"
    ]
  , prod = \_ -> tt $ month 8
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "week[ -]?end|fin de semana"
    ]
  , prod = \_ -> do
      from <- intersect (dayOfWeek 5) (hour False 18)
      to <- intersect (dayOfWeek 1) (hour False 0)
      Token Time <$> interval TTime.Open from to
  }

ruleOrdinalQuarterYear :: Rule
ruleOrdinalQuarterYear = Rule
  { name = "<ordinal> quarter <year>"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    , regex "del? ?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal (OrdinalData {TOrdinal.value = v}):_:Token Time td:_) ->
        tt $ cycleNthAfter False TG.Quarter (v - 1) td
      _ -> Nothing
  }

ruleYyyymmdd :: Rule
ruleYyyymmdd = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})-(0?[1-9]|1[0-2])-(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m1
        m <- parseInt m2
        d <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTimeofdayHoras :: Rule
ruleTimeofdayHoras = Rule
  { name = "<time-of-day> horas"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "h\\.?(ora)?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleNavidad :: Rule
ruleNavidad = Rule
  { name = "Navidad"
  , pattern =
    [ regex "(la )?navidad"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleElCycleAntesTime :: Rule
ruleElCycleAntesTime = Rule
  { name = "el <cycle> antes <time>"
  , pattern =
    [ regex "l[ea']? ?"
    , dimension TimeGrain
    , regex "antes de"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleTwoTimeTokensSeparatedBy :: Rule
ruleTwoTimeTokensSeparatedBy = Rule
  { name = "two time tokens separated by \",\""
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

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "ma(\x00f1|n)ana"
    ]
  , prod = \_ ->
      let from = hour False 4
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleALasHourmintimeofday :: Rule
ruleALasHourmintimeofday = Rule
  { name = "a las <hour-min>(time-of-day)"
  , pattern =
    [ regex "((al?)( las?)?|las?)"
    , Predicate isATimeOfDay
    , regex "horas?"
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "est(e|a)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay <$>
        intersect (cycleNth TG.Day 0) td
      _ -> Nothing
  }

ruleLaCyclePasado :: Rule
ruleLaCyclePasado = Rule
  { name = "la <cycle> pasado"
  , pattern =
    [ regex "(el|los|la|las) ?"
    , dimension TimeGrain
    , regex "pasad(a|o)s?|(u|\x00fa)ltim[ao]s?"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween (- 10000) 999
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
    [ regex "ayer"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleSeason2 :: Rule
ruleSeason2 = Rule
  { name = "season"
  , pattern =
    [ regex "oto(\x00f1|n)o"
    ]
  , prod = \_ ->
      let from = monthDay 9 23
          to = monthDay 12 21
      in Token Time <$> interval TTime.Open from to
  }

ruleDayofweekDayofmonth :: Rule
ruleDayofweekDayofmonth = Rule
  { name = "<day-of-week> <day-of-month>"
  , pattern =
    [ Predicate isADayOfWeek
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleTimeofdayAmpm :: Rule
ruleTimeofdayAmpm = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "([ap])\\.?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
        tt . timeOfDayAMPM td $ Text.toLower ap == "a"
      _ -> Nothing
  }

ruleDayofmonthDeNamedmonth :: Rule
ruleDayofmonthDeNamedmonth = Rule
  { name = "<day-of-month> de <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , regex "de"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleEntreDdEtDdMonthinterval :: Rule
ruleEntreDdEtDdMonthinterval = Rule
  { name = "entre dd et dd <month>(interval)"
  , pattern =
    [ regex "entre( el)?"
    , regex "(0?[1-9]|[12]\\d|3[01])"
    , regex "y( el)?"
    , regex "(0?[1-9]|[12]\\d|3[01])"
    , regex "de"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token RegexMatch (GroupMatch (m1:_)):
       _:
       Token RegexMatch (GroupMatch (m2:_)):
       _:
       Token Time td:
       _) -> do
        v1 <- parseInt m1
        v2 <- parseInt m2
        from <- intersect (dayOfMonth v1) td
        to <- intersect (dayOfMonth v2) td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleNamedmonthDayofmonth :: Rule
ruleNamedmonthDayofmonth = Rule
  { name = "<named-month> <day-of-month>"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleNamedmonth5 :: Rule
ruleNamedmonth5 = Rule
  { name = "named-month"
  , pattern =
    [ regex "mayo?\\.?"
    ]
  , prod = \_ -> tt $ month 5
  }

ruleNamedday7 :: Rule
ruleNamedday7 = Rule
  { name = "named-day"
  , pattern =
    [ regex "domingo|dom\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 7
  }

ruleElTime :: Rule
ruleElTime = Rule
  { name = "el <time>"
  , pattern =
    [ regex "d?el"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
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
        v <- getIntValue token
        tt $ year v
      _ -> Nothing
  }

ruleNamedmonth10 :: Rule
ruleNamedmonth10 = Rule
  { name = "named-month"
  , pattern =
    [ regex "octubre|oct\\.?"
    ]
  , prod = \_ -> tt $ month 10
  }

ruleEsteenUnCycle :: Rule
ruleEsteenUnCycle = Rule
  { name = "este|en un <cycle>"
  , pattern =
    [ regex "(est(e|a|os)|en (el|los|la|las) ?)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleNProximasCycle :: Rule
ruleNProximasCycle = Rule
  { name = "n proximas <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "pr(\x00f3|o)xim(o|a)s?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleLaPasadoCycle :: Rule
ruleLaPasadoCycle = Rule
  { name = "la pasado <cycle>"
  , pattern =
    [ regex "(el|los|la|las) ?"
    , regex "pasad(a|o)s?|(u|\x00fa)ltim[ao]s?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleALasTimeofday :: Rule
ruleALasTimeofday = Rule
  { name = "a las <time-of-day>"
  , pattern =
    [ regex "(al?)( las?)?|las?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd[/-.]mm[/-.]yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[\\./-](0?[1-9]|1[0-2])[\\./-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        d <- parseInt m1
        m <- parseInt m2
        y <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleNamedmonth11 :: Rule
ruleNamedmonth11 = Rule
  { name = "named-month"
  , pattern =
    [ regex "noviembre|nov\\.?"
    ]
  , prod = \_ -> tt $ month 11
  }

ruleOrdinalQuarter :: Rule
ruleOrdinalQuarter = Rule
  { name = "<ordinal> quarter"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal (OrdinalData {TOrdinal.value = v}):_) ->
        tt . cycleNthAfter False TG.Quarter (v - 1)
          $ cycleNth TG.Year 0
      _ -> Nothing
  }

ruleElCycleProximoqueViene :: Rule
ruleElCycleProximoqueViene = Rule
  { name = "el <cycle> (proximo|que viene)"
  , pattern =
    [ regex "(el|los|la|las) ?"
    , dimension TimeGrain
    , regex "(pr(\x00f3|o)xim(o|a)s?|que vienen?|siguientes?)"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleElCycleProximoqueVieneTime :: Rule
ruleElCycleProximoqueVieneTime = Rule
  { name = "el <cycle> proximo|que viene <time>"
  , pattern =
    [ regex "(el|los|la|las)"
    , dimension TimeGrain
    , regex "(pr(\x00f3|o)xim(o|a)s?|que vienen?|siguientes?)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleDelMedioda :: Rule
ruleDelMedioda = Rule
  { name = "del mediodía"
  , pattern =
    [ regex "del mediod(i|\x00ed)a"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 17
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleNamedday3 :: Rule
ruleNamedday3 = Rule
  { name = "named-day"
  , pattern =
    [ regex "mi(e|\x00e9)\\.?(rcoles)?|mx|mier?\\."
    ]
  , prod = \_ -> tt $ dayOfWeek 3
  }

ruleIntersectByDe :: Rule
ruleIntersectByDe = Rule
  { name = "intersect by `de`"
  , pattern =
    [ Predicate isNotLatent
    , regex "de"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "ma(n|\x00f1)ana"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleNthTimeDeTime :: Rule
ruleNthTimeDeTime = Rule
  { name = "nth <time> de <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "de|en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal (OrdinalData {TOrdinal.value = v}):
       Token Time td1:
       _:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNamedmonth9 :: Rule
ruleNamedmonth9 = Rule
  { name = "named-month"
  , pattern =
    [ regex "septiembre|sept?\\.?"
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
  [ ruleALasHourmintimeofday
  , ruleALasTimeofday
  , ruleAfternoon
  , ruleAnoNuevo
  , ruleCeTime
  , ruleDatetimeDatetimeInterval
  , ruleDayOfMonthSt
  , ruleDayofmonthDeNamedmonth
  , ruleDayofweekDayofmonth
  , ruleDdddMonthinterval
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleDeDatetimeDatetimeInterval
  , ruleDelMedioda
  , ruleDelYear
  , ruleDentroDeDuration
  , ruleDimTimeDeLaManana
  , ruleDimTimeDeLaTarde
  , ruleElCycleAntesTime
  , ruleElCycleProximoqueViene
  , ruleElCycleProximoqueVieneTime
  , ruleElDayofmonthDeNamedmonth
  , ruleElDayofmonthNonOrdinal
  , ruleElProximoCycle
  , ruleElTime
  , ruleEnDuration
  , ruleEntreDatetimeEtDatetimeInterval
  , ruleEntreDdEtDdMonthinterval
  , ruleEsteenUnCycle
  , ruleEvening
  , ruleHaceDuration
  , ruleHhhmmTimeofday
  , ruleHourofdayAndRelativeMinutes
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleHourofdayMinusIntegerAsRelativeMinutes
  , ruleInThePartofday
  , ruleIntegerInThePartofday
  , ruleIntersect
  , ruleIntersectByDe
  , ruleLaCyclePasado
  , ruleLaPasadoCycle
  , ruleMidnight
  , ruleMorning
  , ruleNCycleProximoqueViene
  , ruleNPasadosCycle
  , ruleNProximasCycle
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
  , ruleNamedmonthnameddayNext
  , ruleNamedmonthnameddayPast
  , ruleNavidad
  , ruleNochevieja
  , ruleNoon
  , ruleNow
  , ruleNthTimeDeTime
  , ruleNthTimeDeTime2
  , ruleOrdinalQuarter
  , ruleOrdinalQuarterYear
  , rulePasadosNCycle
  , ruleProximasNCycle
  , ruleRightNow
  , ruleSeason
  , ruleSeason2
  , ruleSeason3
  , ruleSeason4
  , ruleTheDayAfterTomorrow
  , ruleTheDayBeforeYesterday
  , ruleThisDayofweek
  , ruleThisPartofday
  , ruleTimeofdayAmpm
  , ruleTimeofdayHoras
  , ruleTimeofdayLatent
  , ruleTimeofdayPartofday
  , ruleTomorrow
  , ruleTwoTimeTokensSeparatedBy
  , ruleUltimoDayofweekDeTime
  , ruleWeekend
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYesterday
  , ruleYyyymmdd
  , ruleHourofdayAndThreeQuarter
  , ruleHourofdayAndHalf
  , ruleHourofdayAndQuarter
  , ruleHourofdayAndRelativeMinutes2
  , ruleHourofdayThreeQuarter
  , ruleHourofdayHalf
  , ruleHourofdayQuarter
  , ruleHourofdayIntegerAsRelativeMinutes2
  , ruleHourofdayMinusThreeQuarter
  , ruleHourofdayMinusHalf
  , ruleHourofdayMinusQuarter
  , ruleHourofdayMinusIntegerAsRelativeMinutes2
  , ruleTimezone
  ]
