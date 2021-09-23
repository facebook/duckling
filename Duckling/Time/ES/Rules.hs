-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Time.ES.Rules
  ( rules
  ) where

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (isGrain)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Types (OrdinalData(..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData(..))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleTheDayAfterTomorrow :: Rule
ruleTheDayAfterTomorrow = Rule
  { name = "the day after tomorrow"
  , pattern =
    [ regex "pasado\\s?ma(n|ñ)ana"
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

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Lunes"     , "lunes|lun?\\.?"                        )
  , ( "Martes"    , "martes|mar?\\.?"                       )
  , ( "Miercoles" , "mi(e|é)\\.?(rcoles)?|mx|mier?\\." )
  , ( "Jueves"    , "jueves|jue|jue\\."                     )
  , ( "Viernes"   , "viernes|vie|vie\\."                    )
  , ( "Sabado"    , "s(á|a)bado|s(á|a)b\\.?"      )
  , ( "Domingo"   , "domingo|dom\\.?"                       )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Enero"     , "enero|ene\\.?")
  , ( "Febrero"   , "febrero|feb\\.?")
  , ( "Marzo"     , "marzo|mar\\.?")
  , ( "Abril"     , "abril|abr\\.?")
  , ( "Mayo"      , "mayo?\\.?")
  , ( "Junio"     , "junio|jun\\.?")
  , ( "Julio"     , "julio|jul\\.?")
  , ( "Agosto"    , "agosto|ago\\.?")
  , ( "Septiembre", "septiembre|sept?\\.?")
  , ( "Octubre"   , "octubre|oct\\.?")
  , ( "Noviembre" , "noviembre|nov\\.?")
  , ( "Diciembre" , "diciembre|dic\\.?")
  ]

ruleThisOrNextDayOfWeek :: Rule
ruleThisOrNextDayOfWeek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "(este|pr(o|ó)ximo)"
    , Predicate isADayOfWeek
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):Token Time dow:_) -> do
        td <- case Text.toLower match of
          "este" -> Just $ predNth 0 True dow
          "próximo" -> intersect dow $ cycleNth TG.Week 1
          "proximo" -> intersect dow $ cycleNth TG.Week 1
          _ -> Nothing
        tt td
      _ -> Nothing
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
  , prod = \_ -> tt today
  }

ruleUltimoDayofweekDeTime :: Rule
ruleUltimoDayofweekDeTime = Rule
  { name = "ultimo <day-of-week> de <time>"
  , pattern =
    [ regex "(ú|u)ltimo"
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
    , regex "pr(ó|o)xim(o|a)s?|siguientes?"
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

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "medio(\\s*)d(í|i)a"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleProximasNCycle :: Rule
ruleProximasNCycle = Rule
  { name = "proximas n <cycle>"
  , pattern =
    [ regex "pr(ó|o)xim(o|a)s?"
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
        Token Time <$> minutesBefore n td
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
        Token Time <$> minutesBefore n td
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
      (Token Time td:_) -> Token Time <$> minutesBefore 15 td
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
      (Token Time td:_) -> Token Time <$> minutesBefore 30 td
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
      (Token Time td:_) -> Token Time <$> minutesBefore 45 td
      _ -> Nothing
  }

ruleHourofdayIntegerAsRelativeMinutes :: Rule
ruleHourofdayIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
  , pattern =
    [ Predicate $ and . sequence [isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       minutes:
       _) -> do
        n <- getIntValue minutes
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
    [ regex "del( a(ñ|n)o)?"
    , Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt $ year v
      _ -> Nothing
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
    [ regex "a(n|ñ)o nuevo"
    ]
  , prod = \_ -> tt $ monthDay 1 1
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
        tt . mkLatent $ hour (v < 13) v
      _ -> Nothing
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

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "verano"   , "verano"   , monthDay  6 21, monthDay  9 23 )
  , ( "otoño"    , "oto(ñ|n)o", monthDay  9 23, monthDay 12 21 )
  , ( "invierno" , "invierno" , monthDay 12 21, monthDay  3 20 )
  , ( "primavera", "primavera", monthDay  3 20, monthDay  6 21 )
  ]

ruleRightNow :: Rule
ruleRightNow = Rule
  { name = "right now"
  , pattern =
    [ regex "ahor(it)?a|ya|en\\s?seguida|cuanto antes"
    ]
  , prod = \_ -> tt now
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

ruleDimTimeDeLaTarde2 :: Rule
ruleDimTimeDeLaTarde2 = Rule
  { name = "<time-of-day> de la tarde"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    , regex "(a|en|de) la tarde"
    ]
  , prod = \case
      (Token Time TimeData {
          TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)
        }:
        minutesToken:
       _:
       _) -> do
         minutes <- getIntValue minutesToken
         let td = hourMinute is12H hours minutes
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
    , regex "(pr(ó|o)xim(o|a)s?|que vienen?|siguientes?)"
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
    , regex "(a|en|de) la ma(ñ|n)ana"
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
       Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
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
        Token Time <$> interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "week[ -]?end|fin de semana"
    ]
  , prod = \_ -> tt weekend
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
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_:Token Time td:_) ->
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
        tt $ mkLatent td
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
    [ regex "ma(ñ|n)ana"
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
  , prod = \case
      ( _:
        x:
        _) -> Just x
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
      (_:Token Time td:_) -> Token Time . partOfDay <$> intersect today td
      _ -> Nothing
  }

ruleLaCyclePasado :: Rule
ruleLaCyclePasado = Rule
  { name = "la <cycle> pasado"
  , pattern =
    [ regex "(el|los|la|las) ?"
    , dimension TimeGrain
    , regex "pasad(a|o)s?|(u|ú)ltim[ao]s?"
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
      [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 10000]
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
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
  }

ruleDOMOfMonth :: Rule
ruleDOMOfMonth = Rule
  { name = "<day-of-month> (ordinal or number) de <named-month>"
  , pattern =
    [ Predicate isDOMValue
    , regex "de"
    , Predicate isAMonth
    ]
  , prod = \case
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
  , prod = \case
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
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

ruleMonthDOM :: Rule
ruleMonthDOM = Rule
  { name = "<named-month> <day-of-month>"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
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
    , regex "pr(ó|o)xim(o|a)s?"
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
    , regex "pasad(a|o)s?|(u|ú)ltim[ao]s?"
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
    [ regex "((para)|(al?))( las?)?|las?"
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

ruleOrdinalQuarter :: Rule
ruleOrdinalQuarter = Rule
  { name = "<ordinal> quarter"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
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
    , regex "(pr(ó|o)xim(o|a)s?|que vienen?|siguientes?)"
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
    , regex "(pr(ó|o)xim(o|a)s?|que vienen?|siguientes?)"
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
    [ regex "del mediod(i|í)a"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 17
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
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
    [ regex "ma(n|ñ)ana"
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
      (Token Ordinal OrdinalData{TOrdinal.value = v}:
       Token Time td1:
       _:
       Token Time td2:
       _) -> Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isATimeOfDay, isNotLatent]
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleNextWeek :: Rule
ruleNextWeek = Rule
  {
    name = "next week"
  , pattern =
    [ Predicate $ isGrain TG.Week
    , regex "que viene"
    ]
  , prod = \_ -> tt $ cycleNth TG.Week 1
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "Día de África", "d(í|i)a de (la (libertad|liberacion) )?(á|a)frica(na)?", monthDay 5 25 )
  , ( "Día de la Industrialización de África", "d(í|i)a de (la )?industrializaci(ó|o)n de (á|a)frica(na)?", monthDay 11 20 )
  , ( "Día de Todos los Santos", "d(í|i)a de todos los santos", monthDay 11 1 )
  , ( "Día de las Almas", "d(í|i)a de las almas", monthDay 11 2 )
  , ( "Día de los Inocentes de Abril", "d(í|i)a de (los inocentes|las bromas)( de abril)?", monthDay 4 1 )
  , ( "Día Mundial de la Lengua Árabe", "d(í|i)a mundial de la lengua (á|a)rabe", monthDay 12 18 )
  , ( "Asunción de María", "asunci(ó|o)n de mar(í|i)a", monthDay 8 15 )
  , ( "Día del Boxeo", "(el )?d(í|i)a del (boxeo|boxeador)", monthDay 12 26 )
  , ( "Día de Lengua China", "d(í|i)a de lengua china", monthDay 4 20 )
  , ( "Nochebuena", "(la (cena de )?)?(nochebuena|noche buena)", monthDay 12 24 )
  , ( "Día de Conmemoración de las Víctimas de la Guerra Química", "d(í|i)a (de )?(conmemorativo|conmemoraci(ó|o)n) (en recuerdo )?de las v(í|i)ctimas de la guerra qu(í|i)mica", monthDay 4 29 )
  , ( "Día de Conmemoración de las Víctimas del Genocidio de Ruanda", "d(í|i)a (de )?(conmemorativo|conmemoraci(ó|o)n) (en recuerdo )?de las v(í|i)ctimas del genocidio de ruanda", monthDay 4 7 )
  , ( "Día de la Marina", "d(í|i)a (de la|del) marin(a|o)", monthDay 6 25 )
  , ( "Día de la Tierra", "d(í|i)a (internacional )?de la (madre )?tierra", monthDay 4 22 )
  , ( "Día de la Lengua Inglesa", "d(í|i)a de la lengua inglesa", monthDay 4 23 )
  , ( "Epifanía", "Epifan(í|i)a", monthDay 1 6 )
  , ( "Las Fiestas de San Francisco de Asís", "(las )?fiestas de san (francisco de as(í|i)s|pacho)", monthDay 10 4 )
  , ( "Fiesta de la Inmaculada Concepción", "(d(í|i)a|fiesta) de la inmaculada concepci(ó|o)n( de (la virgen )?mar(í|i)a)?", monthDay 12 8 )
  , ( "Día Mundial de las Madres y de los Padres", "d(í|i)a mundial de las madres y de los padres", monthDay 6 1 )
  , ( "Día de Todos los Santos", "(d(í|i)a|festividad) de todos los santos", monthDay 10 31 )
  , ( "Día de los Derechos Humanos", "d(í|i)a de los derechos humanos", monthDay 12 10 )
  , ( "Día Internacional de Sensibilización sobre el Albinismo", "d(í|i)a internacional de sensibilizaci(ó|o)n sobre el albinismo", monthDay 6 13 )
  , ( "Día Internacional contra la Corrupción", "d(í|i)a internacional contra la corrupci(ó|o)n", monthDay 12 9 )
  , ( "Día Internacional de los Asteroides", "d(í|i)a internacional de los asteroides", monthDay 6 30 )
  , ( "Celebra el Día Internacional de la Bisexualidad", "celebra el d(í|i)a internacional de la bisexualidad", monthDay 9 23 )
  , ( "Día Internacional de Recordación de Desastres de Chernóbil", "d(í|i)a internacional de recordaci(ó|o)n de desastres de chern(ó|o)bil ", monthDay 4 26 )
  , ( "Día Internacional de la Aviacion Civil", "d(í|i)a internacional de la aviacion civil", monthDay 12 7 )
  , ( "Día Internacional de la Aduana", "d(í|i)a internacional de la(s)? aduana(s)?", monthDay 1 26 )
  , ( "Día Internacional de la Lucha contra el Uso Indebido y el Tráfico Ilícito de Drogas", "d(í|i)a internacional de la lucha contra el uso indebido y el tr(á|a)fico il(í|i)cito de drogas", monthDay 6 26 )
  , ( "Día Internacional contra los Ensayos Nucleares", "d(í|i)a internacional contra los ensayos nucleares", monthDay 8 29 )
  , ( "Día Internacional de la Diversidad Biológica", "d(í|i)a internacional de la diversidad biol(ó|o)gica", monthDay 5 22 )
  , ( "Día Internacional de los Monumentos y Sitios", "d(í|i)a internacional de los monumentos y sitios", monthDay 4 18 )
  , ( "Día Internacional para la Prevención de la Explotación del medio ambiente en la Guerra y los Conflictos Armados", "d(í|i)a internacional para la prevenci(ó|o)n de la explotaci(ó|o)n del medio ambiente en la guerra y los conflictos armados", monthDay 11 6 )
  , ( "Día de las Naciones Unidas para la Cooperación Sur-Sur", "d(í|i)a de las naciones unidas para la cooperaci(ó|o)n sur(\\-|\\s)sur", monthDay 9 12 )
  , ( "Día Internacional para la Tolerancia", "d(í|i)a internacional (de|para) la tolerancia", monthDay 11 16 )
  , ( "Día Internacional para la Abolición de la Esclavitud", "d(í|i)a internacional para la abolici(ó|o)n de la esclavitud", monthDay 12 2 )
  , ( "Día Internacional de la Eliminación de la Discriminación Racial", "d(í|i)a internacional de la eliminaci(ó|o)n de la discriminaci(ó|o)n racial", monthDay 3 21 )
  , ( "Día Internacional de la Eliminación de la Violencia Sexual en los Conflictos", "d(í|i)a internacional de la eliminaci(ó|o)n de la violencia sexual en los conflictos", monthDay 6 19 )
  , ( "Día Internacional de la Eliminación de la Violencia contra la Mujer", "d(í|i)a internacional de la eliminaci(ó|o)n de la violencia contra la mujer.", monthDay 11 25 )
  , ( "Día Internacional para la Erradicación de la Pobreza", "d(í|i)a internacional para la erradicaci(ó|o)n de la pobreza", monthDay 10 17 )
  , ( "Día Internacional de la Preservación de la Capa de Ozono", "d(í|i)a internacional de la preservaci(ó|o)n de la capa de ozono", monthDay 9 16 )
  , ( "Día Internacional del Recuerdo de la Trata de Esclavos y de su Abolición", "d(í|i)a internacional del recuerdo de la trata de esclavos y de su abolici(ó|o)n", monthDay 8 23 )
  , ( "Día Internacional del Derecho a la Verdad en relación con Violaciones Graves de los Derechos Humanos y de la Dignidad de las Víctimas de las Naciones Unidas", "d(í|i)a internacional del derecho a la verdad en relaci(ó|o)n con violaciones graves de los derechos humanos y de la dignidad de las v(í|i)ctimas de las naciones unidas", monthDay 3 24 )
  , ( "Día Internacional para la Eliminación Total de las Armas Nucleares", "d(í|i)a internacional para la eliminaci(ó|o)n total de las armas nucleares", monthDay 9 26 )
  , ( "Día Internacional de las Naciones Unidas en Apoyo de las Víctimas de la Tortura", "d(í|i)a internacional de las naciones unidas en apoyo de las v(í|i)ctimas de la tortura", monthDay 6 26 )
  , ( "Día Internacional de la Beneficencia", "d(í|i)a internacional de la beneficencia", monthDay 9 5 )
  , ( "Día Internacional de Conmemoración anual en memoria de las Víctimas del Holocausto", "d(í|i)a internacional de conmemoraci(ó|o)n anual en memoria de las v(í|i)ctimas del holocausto", monthDay 1 27 )
  , ( "Día Internacional de la Democracia", "d(í|i)a (internacional )?de la democracia", monthDay 9 15 )
  , ( "Día Internacional de las Personas con Discapacidad", "d(í|i)a internacional de las personas con discapacidad", monthDay 12 3 )
  , ( "Día Internacional de la Familia", "d(í|i)a internacional de la(s)? familia(s)?", monthDay 5 15 )
  , ( "Día Internacional de las Remesas Familiares", "d(í|i)a internacional de las remesas familiares", monthDay 6 16 )
  , ( "Día Internacional de los Bosques", "d(í|i)a internacional de los bosques", monthDay 3 21 )
  , ( "Día Internacional de la Amistad", "d(í|i)a internacional de la amistad", monthDay 7 30 )
  , ( "Día Internacional de la Felicidad", "d(í|i)a (internacional|mundial) de la felicidad", monthDay 3 20 )
  , ( "Día Internacional de los Vuelos Espaciales", "d(í|i)a internacional (de los|del) vuelos espaciales", monthDay 4 12 )
  , ( "Día Internacional de los Niños Víctimas Inocentes de Agresión", "d(í|i)a internacional de los ni(ñ|n)os v(í|i)ctimas inocentes de agresi(ó|o)n", monthDay 6 4 )
  , ( "Día Internacional de la no Violencia", "d(í|i)a internacional de la no violencia", monthDay 10 2 )
  , ( "Día Internacional del Nowruz", "d(í|i)a internacional del nowruz", monthDay 3 21 )
  , ( "Día Internacional de las Personas Mayores", "d(í|i)a internacional de las personas mayores", monthDay 10 1 )
  , ( "Dia Internacional de la Paz", "d(í|i)a internacional de la paz", monthDay 9 21 )
  , ( "Día Internacional de las Personas con Discapacidad", "d(í|i)a internacional de las personas con discapacidad", monthDay 12 3 )
  , ( "Día Internacional de Recuerdo de las Víctimas de la Esclavitud y la Trata Transatlántica de Esclavos", "d(í|i)a internacional de recuerdo de las v(í|i)ctimas de la esclavitud y la trata transatl(á|a)ntica de esclavos", monthDay 3 25 )
  , ( "Día Internacional de las Mujeres Rurales", "d(í|i)a internacional de las mujeres rurales", monthDay 10 15 )
  , ( "Día Internacional de Solidaridad con los miembros del Personal Detenidos o Desaparecidos", "d(í|i)a internacional de solidaridad con los miembros del personal detenidos o desaparecidos", monthDay 3 25 )
  , ( "Día Internacional de Solidaridad con el Pueblo Palestino", "d(í|i)a internacional de solidaridad con el pueblo palestino", monthDay 11 29 )
  , ( "Día Internacional del Deporte para el Desarrollo y la Paz", "d(í|i)a internacional del deporte para el desarrollo y la paz", monthDay 4 6 )
  , ( "Día Internacional del Personal de Paz de las Naciones Unidas", "d(í|i)a internacional del personal de paz de las naciones unidas", monthDay 5 29 )
  , ( "Día Internacional de la Mujer y la Niña en la Ciencia", "d(í|i)a internacional de la mujer y la ni(ñ|n)a en la ciencia", monthDay 2 11 )
  , ( "Día Internacional del Yoga", "d(í|i)a Internacional del Yoga", monthDay 6 21 )
  , ( "Día Internacional de Tolerancia Cero con la Mutilación Genital Femenina", "d(í|i)a internacional de tolerancia cero con la mutilaci(ó|o)n genital femenina", monthDay 2 6 )
  , ( "Día Internacional de la Niña", "d(í|i)a internacional de la ni(ñ|n)a", monthDay 10 11 )
  , ( "Día Internacional de las Víctimas de Desapariciones Forzadas", "d(í|i)a internacional de las v(í|i)ctimas de desapariciones forzadas", monthDay 8 30 )
  , ( "Día Internacional de los Pueblos Indígenas del Mundo", "d(í|i)a internacional de los pueblos ind(í|i)genas del mundo", monthDay 8 9 )
  , ( "Día Internacional para poner fin a la Impunidad de los Crímenes contra Periodistas", "d(í|i)a internacional para poner fin a la impunidad de los cr(í|i)menes contra periodistas", monthDay 11 2 )
  , ( "Día Internacional para la Erradicación de la Fístula Obstétrica", "d(í|i)a internacional para la erradicaci(ó|o)n de la f(í|i)stula obst(é|e)trica", monthDay 5 23 )
  , ( "Día Internacional para la Reducción de los Desastres Naturales", "d(í|i)a internacional para la reducci(ó|o)n de los desastres (naturales)?", monthDay 10 13 )
  , ( "Día Internacional de la Solidaridad Humana", "d(í|i)a internacional de la solidaridad humana", monthDay 12 20 )
  , ( "Día Internacional del Jazz", "d(í|i)a internacional del jazz", monthDay 4 30 )
  , ( "Día Internacional de la Alfabetización", "d(í|i)a internacional de la alfabetizaci(ó|o)n", monthDay 9 8 )
  , ( "Día Internacional del Hombre", "d(í|i)a internacional de(l)? los hombre(s)?", monthDay 11 19 )
  , ( "Día Internacional del Migrante", "d(í|i)a internacional del migrante", monthDay 12 18 )
  , ( "Día Internacional de la Lengua Materna", "d(í|i)a (internacional|mundial) de la lengua materna", monthDay 2 21 )
  , ( "Día Internacional de las Montañas", "d(í|i)a internacional de las monta(ñ|n)as", monthDay 12 11 )
  , ( "Día Internacional de las Enfermeras", "d(í|i)a internacional de las enfermeras", monthDay 5 12 )
  , ( "Día de la Conciencia Internacional sobre la Sobredosis", "ioad|d(í|i)a de (la )?conciencia internacional (de|sobre) la sobredosis", monthDay 8 31 )
  , ( "Día Internacional del Voluntariado para el Desarrollo Económico y Social", "d(í|i)a internacional del voluntariado para el desarrollo econ(ó|o)mico y social", monthDay 12 5 )
  , ( "Día Internacional de las Viudas", "d(í|i)a internacional de las viudas", monthDay 6 23 )
  , ( "Día Internacional de la Mujer", "d(í|i)a internacional de la mujer", monthDay 3 8 )
  , ( "Día Internacional de la Juventud", "d(í|i)a internacional de la juventud", monthDay 8 12 )
  , ( "El Primero de Mayo", "(el )?primero de mayo", monthDay 5 1 )
  , ( "Día Internacional de Nelson Mandela", "d(í|i)a (internacional )?de nelson mandela", monthDay 7 18 )
  , ( "La Navidad Ortodoxa", "(la )?navidad ortodoxa", monthDay 1 7 )
  , ( "Año Nuevo Ortodoxo", "(el )?(a(ñ|n)o nuevo viejo|a(ñ|n)o viejo ortodoxo|a(ñ|n)o nuevo ortodoxo|a(ñ|n)o nuevo juliano)", monthDay 1 14 )
  , ( "Día Mundial de los Servicios Públicos", "d(í|i)a (mundial|nacional) de(l)? (los )?(servicios|servidor) p(ú|u)blico(s)?", monthDay 6 23 )
  , ( "Día de San Jorge", "d(í|i)a de san jorge|fiesta de san jorge", monthDay 4 23 )
  , ( "Día de San Patricio", "d(í|i)a de san patricio", monthDay 3 17 )
  , ( "Dia de San Esteban", "d(í|i)a de san esteban", monthDay 12 26 )
  , ( "Día Internacional del Tiempo del Recuerdo y la Reconciliación para Quienes Perdieron la Vida durante la Segunda Guerra Mundial", "d(í|i)a internacional del tiempo del recuerdo y la reconciliaci(ó|o)n para quienes perdieron la vida durante la segunda guerra mundial", monthDay 5 8 )
  , ( "Día de las Naciones Unidas", "d(í|i)a de las naciones unidas", monthDay 10 24 )
  , ( "Día Internacional de Información sobre el Peligro de las Minas y de Asistencia para las Actividades Relativas a las Minas", "d(í|i)a internacional de informaci(ó|o)n sobre el peligro de las minas y de asistencia para las actividades relativas a las minas", monthDay 4 4 )
  , ( "Día Mundial de la Salud", "d(í|i)a mundial de la salud( de las naciones unidas)?", monthDay 4 7 )
  , ( "Día Universal del Niño", "d(í|i)a universal del ni(ñ|n)o", monthDay 11 20 )
  , ( "Día de San Valentín", "(el )?d(í|i)a de san valent(í|i)n", monthDay 2 14 )
  , ( "Día Mundial de la lucha contra el SIDA", "d(í|i)a mundial de(l)? (la lucha contra el )?SIDA", monthDay 12 1 )
  , ( "Día Mundial de Concienciación sobre el Autismo", "d(í|i)a mundial de(l)? (concienciaci(ó|o)n sobre el )?autismo", monthDay 4 2 )
  , ( "Día Mundial de la Artritis Reumatoide", "d(í|i)a mundial de la artritis( reumatoide)?", monthDay 5 20 )
  , ( "Día Mundial del Donante de Sangre", "d(í|i)a mundial del donante de sangre", monthDay 6 14 )
  , ( "Día Internacional del Libro y del Derecho de Autor", "d(í|i)a internacional del libro y de(l)? (los )?derecho(s)? de autor", monthDay 4 23 )
  , ( "Día Mundial del Braille", "d(í|i)a mundial del braille", monthDay 1 4 )
  , ( "Día Mundial contra el Cáncer", "d(í|i)a mundial contra el c(á|a)ncer", monthDay 2 4 )
  , ( "Día Mundial de las Ciudades", "d(í|i)a mundial de las ciudades", monthDay 10 31 )
  , ( "Día Mundial de la PC", "d(í|i)a mundial (de la|del) (PC|Paralisia Cerebral)", monthDay 10 6 )
  , ( "Día Mundial contra el Trabajo Infantil", "d(í|i)a mundial contra el trabajo infantil", monthDay 6 12 )
  , ( "Día Mundial contra la Trata de Personas", "d(í|i)a mundial contra la trata de personas", monthDay 7 30 )
  , ( "Día Mundial del Patrimonio Audiovisual", "d(í|i)a mundial (del|sobre el) patrimonio audiovisual", monthDay 10 27 )
  , ( "Día Mundial de la Diversidad Cultural para el Diálogo y el Desarrollo", "d(í|i)a mundial de la diversidad cultural para el diálogo y el desarrollo", monthDay 5 21 )
  , ( "Día Mundial de la Seguridad y la Salud en el Trabajo", "d(í|i)a mundial de la seguridad y la salud en el trabajo", monthDay 4 28 )
  , ( "Día Mundial por la Abolición de la Esclavitud", "d(í|i)a (internacional|mundial) (para|por) la abolici(ó|o)n de la esclavitud", monthDay 12 2 )
  , ( "Día Mundial de la Justicia Social", "d(í|i)a mundial de la justicia social", monthDay 2 20 )
  , ( "Día Mundial del Enfermo", "d(í|i)a mundial (del|de los) enfermos", monthDay 2 11 )
  , ( "Día Mundial de Lucha contra la Desertificación y la Sequía", "d(í|i)a mundial (de lucha contra|para combatir) la desertificaci(ó|o)n y la sequ(í|i)a", monthDay 6 17 )
  , ( "Día Mundial de Información sobre el Desarrollo", "d(í|i)a mundial de informaci(ó|o)n sobre el desarrollo", monthDay 10 24 )
  , ( "Día Mundial de la Diabetes", "d(í|i)a mundial de la diabetes", monthDay 11 14 )
  , ( "Día Mundial del Síndrome de Down", "d(í|i)a mundial del s(í|i)ndrome de down", monthDay 3 21 )
  , ( "Día Mundial de Toma de Conciencia del Abuso y Maltrato en la Vejez", "d(í|i)a mundial de toma de conciencia del abuso y maltrato en la vejez", monthDay 6 15 )
  , ( "Día Mundial del Medio Ambiente", "d(í|i)a mundial del medio ambiente", monthDay 6 5 )
  , ( "Día Mundial de la Alimentación", "d(í|i)a mundial de la alimentaci(ó|o)n", monthDay 10 16 )
  , ( "Día para la Conmemoración y Dignificación de las Víctimas del Crimen de Genocidio y para su Prevención", "d(í|i)a para la conmemoraci(ó|o)n y dignificaci(ó|o)n de las v(í|i)ctimas del crimen de genocidio y para su prevenci(ó|o)n", monthDay 12 9 )
  , ( "Día Mundial del Corazón", "d(í|i)a mundial del coraz(ó|o)n", monthDay 9 29 )
  , ( "Día Mundial de la Hepatitis", "d(í|i)a mundial (contra|de) la hepatitis", monthDay 7 28 )
  , ( "Día Mundial de la Asistencia Humanitaria", "d(í|i)a mundial de la asistencia humanitaria", monthDay 8 19 )
  , ( "Día Internacional de la Sociedad de la Información", "d(í|i)a (internacional|mundial) de (las telecomunicaciones y )?la sociedad de la informaci(ó|o)n", monthDay 5 17 )
  , ( "Día Mundial de la Propiedad Intelectual", "d(í|i)a mundial de la propiedad intelectual", monthDay 4 26 )
  , ( "Día Mundial de la Malaria", "d(í|i)a mundial (de la|del) (malaria|paludismo)", monthDay 4 25 )
  , ( "Día Mundial de la Salud Mental", "d(í|i)a mundial de la salud mental", monthDay 10 10 )
  , ( "Día Mundial de la Meteorología", "d(í|i)a (mundial de (la )?meteorolog(í|i)a|meteorol(ó|o)gico mundial)", monthDay 3 23 )
  , ( "Día Mundial sin Tabaco", "d(í|i)a mundial sin tabaco", monthDay 5 31 )
  , ( "Dia Mundial de los Océanos", "d(í|i)a mundial de los oc(é|e)anos", monthDay 6 8 )
  , ( "Día Mundial del Cáncer de Ovario", "d(í|i)a mundial del c(á|a)ncer de ovario", monthDay 5 8 )
  , ( "Día Mundial contra la Neumonía", "d(í|i)a mundial (contra|de) la neumon(í|i)a", monthDay 11 12 )
  , ( "Dia Mundial de la Poesia", "d(í|i)a mundial de la poesia", monthDay 3 21 )
  , ( "Día Mundial de la Población", "d(í|i)a mundial de la poblaci(ó|o)n", monthDay 7 11 )
  , ( "Día Mundial del Correo", "d(í|i)a mundial del correo", monthDay 10 9 )
  , ( "Día de la Prematuridad Mundial", "d(í|i)a (de la prematuridad mundial|mundial del (ni(ñ|n)o )?prematuro)", monthDay 11 17 )
  , ( "Día Mundial de la Libertad de Prensa", "d(í|i)a mundial de la libertad de prensa", monthDay 5 3 )
  , ( "Día Mundial contra la Rabia", "d(í|i)a mundial (contra|de) la rabia", monthDay 9 28 )
  , ( "Día Mundial de la Radio", "d(í|i)a mundial de la radio", monthDay 2 13 )
  , ( "Día Mundial de los Refugiados", "d(í|i)a mundial (del|de los) refugiado(s)?", monthDay 6 20 )
  , ( "Día Mundial de la Ciencia para la Paz y el Desarrollo", "d(í|i)a mundial de la ciencia para la paz y el desarrollo", monthDay 11 10 )
  , ( "Día Mundial de la Salud Sexual", "dmss|d(í|i)a mundial de la salud sexual", monthDay 9 4 )
  , ( "Día Mundial del Suelo", "d(í|i)a mundial del suelo", monthDay 12 5 )
  , ( "Día Mundial del Accidente Cerebrovascular", "d(í|i)a mundial del (accidente cerebrovascular|acv)", monthDay 10 29 )
  , ( "Día Mundial para la Prevención del Suicidio", "d(í|i)a mundial para la prevenci(ó|o)n del suicidio", monthDay 9 10 )
  , ( "Día Mundial de los Docentes", "d(í|i)a mundial (de los docentes|del maestro)", monthDay 10 5 )
  , ( "Día Mundial de la Televisión", "d(í|i)a mundial de la televisi(ó|o)n", monthDay 11 21 )
  , ( "Día Mundial del Baño", "d(í|i)a mundial del (ba(ñ|n)o|retrete)", monthDay 11 19 )
  , ( "Día Mundial del Turismo", "dmt|d(í|i)a mundial del turismo", monthDay 9 27 )
  , ( "Día Mundial de la Tuberculosis", "d(í|i)a mundial de (lucha contra)?la (tuberculosis|tb)", monthDay 3 24 )
  , ( "Día Mundial del Atun", "d(í|i)a mundial del atun", monthDay 5 2 )
  , ( "Día Mundial del Veganismo", "d(í|i)a mundial del veganismo", monthDay 11 1 )
  , ( "Día Mundial del Vegetarianismo", "d(í|i)a mundial del vegetarianismo", monthDay 10 1 )
  , ( "Día Mundial del Agua", "d(í|i)a mundial del agua", monthDay 3 22 )
  , ( "Día Mundial de los Humedales", "d(í|i)a mundial de los humedales", monthDay 2 2 )
  , ( "Día Mundial de la Vida Silvestre", "d(í|i)a mundial de la vida silvestre", monthDay 3 3 )
  , ( "Día Mundial de las Habilidades de la Juventud", "d(í|i)a mundial de las habilidades de la juventud", monthDay 7 15 )
  , ( "Día de la Cero Discriminación", "d(í|i)a de la cero discriminaci(ó|o)n", monthDay 3 1 )

  -- Fixed day/week/month, year over year
  , ( "Día de la Commonwealth", "d(í|i)a de la commonwealth", nthDOWOfMonth 2 1 3 )
  , ( "Día Mundial en Recuerdo de las Víctimas de Accidentes de Tráfico"
    , "d(í|i)a (mundial )?en recuerdo de las v(í|i)ctimas (de accidentes )?de tr(á|a)fico"
    , nthDOWOfMonth 3 7 11 )
  , ( "Día Internacional de las Cooperativas"
    , "d(í|i)a internacional de las cooperativas", nthDOWOfMonth 1 6 7 )
  , ( "Día de Martin Luther King"
    , "(MLK|d(í|i)a de Martin Luther King,?)( Jr\\.?| J(ú|u)nior)?|d(í|i)a de los derechos humanos en Idaho"
    , nthDOWOfMonth 3 1 1
    )

  -- The day after Thanksgiving (not always the fourth Friday of November)
  , ( "Viernes negro", "viernes negro"
    , cycleNthAfter False TG.Day 1 $ nthDOWOfMonth 4 4 11
    )
  , ( "Día Mundial del Hábitat", "d(í|i)a mundial del h(á|a)bitat", nthDOWOfMonth 1 1 10 )
  , ( "Día Mundial del Riñón", "d(í|i)a mundial del ri(ñ|n)(ó|o)n", nthDOWOfMonth 2 4 3 )
  , ( "Día Mundial de la Lepra", "d(í|i)a mundial (contra|de) la lepra"
    , predLastOf (dayOfWeek 7) (month 1) )
  , ( "Día Marítimo Mundial", "d(í|i)a mar(í|i)timo mundial"
    , predLastOf (dayOfWeek 4) (month 9) )
  , ( "Día Mundial de las Aves Migratorias", "d(í|i)a mundial de las aves migratorias"
    , nthDOWOfMonth 2 6 5 )
  , ( "Día mundial de la filosofía", "d(í|i)a mundial de la filosof(í|i)a", nthDOWOfMonth 3 4 11 )
  , ( "Día mundial de la religion", "d(í|i)a mundial de la(s)? religion(es)?", nthDOWOfMonth 3 7 1 )
  , ( "Día mundial de la vista", "d(í|i)a mundial de la (vista|visi(ó|o)n)", nthDOWOfMonth 2 4 10 )

  -- Other
  , ( "Día del jefe", "(el )?d(í|i)a (nacional )?del jefe"
    , predNthClosest 0 weekday (monthDay 10 16) )
  ]

ruleElDayofmonthNonOrdinalWithDia :: Rule
ruleElDayofmonthNonOrdinalWithDia = Rule
  { name = "dia <day-of-month> (non ordinal)"
  , pattern =
    [ regex "día"
    , Predicate $ isIntegerBetween 1 31
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleNextWeekAlt :: Rule
ruleNextWeekAlt = Rule
  {
    name = "next week (alt)"
  , pattern =
    [
      regex "pr(ó|o)xim(o|a)s?|siguientes?"
    , Predicate $ isGrain TG.Week
    ]
  , prod = \_ -> tt $ cycleNth TG.Week 1
  }

ruleYearByAddingThreeNumbers :: Rule
ruleYearByAddingThreeNumbers = Rule
  { name    = "year (value by adding three composing numbers together)"
  , pattern =
    [
      regex "mil"
    , Predicate $ isIntegerBetween 100 1000
    , Predicate $ isIntegerBetween 1 100
    ]
  , prod    = \case
      (
        _:
        t1:
        t2:
        _) -> do
          v1 <- getIntValue t1
          v2 <- getIntValue t2
          tt $ year $ 1000 + v1 + v2
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
  , ruleDimTimeDeLaTarde2
  , ruleDOMOfMonth
  , ruleDOMMonth
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
  , ruleMonthDOM
  , ruleMorning
  , ruleNCycleProximoqueViene
  , ruleNPasadosCycle
  , ruleNProximasCycle
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
  , ruleTheDayAfterTomorrow
  , ruleTheDayBeforeYesterday
  , ruleThisOrNextDayOfWeek
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
  , ruleElDayofmonthNonOrdinalWithDia
  , ruleNextWeek
  , ruleNextWeekAlt
  , ruleYearByAddingThreeNumbers
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ rulePeriodicHolidays
