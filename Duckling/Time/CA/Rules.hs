-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Time.CA.Rules
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
    [ regex "dem(a|à) passat|l'endem(a|à)|passat dem(a|à)"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleHaceDuration :: Rule
ruleHaceDuration = Rule
  { name = "fa <duration>"
  , pattern =
    [ regex "fa"
    , dimension Duration
    ]
  , prod = \case
      (_:Token Duration dd:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleCeTime :: Rule
ruleCeTime = Rule
  { name = "ce <time>"
  , pattern =
    [ regex "aquest"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Dilluns"   , "dilluns|dl\\.?" )
  , ( "Dimarts"   , "dimarts|dm\\.?" )
  , ( "Dimecres"  , "dimecres|dc\\.?" )
  , ( "Dijous"    , "dijous|dj\\.?" )
  , ( "Divendres" , "divendres|dv\\.?" )
  , ( "Dissabte"  , "dissabte|ds\\.?" )
  , ( "Diumenge"  , "diumenge|dg\\.?" )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Gener"    , "gener|gen\\.?")
  , ( "Febrer"   , "febrer|feb\\.?")
  , ( "Març"     , "març|mar\\.?")
  , ( "Abril"    , "abril|abr\\.?")
  , ( "Maig"     , "maig?\\.?")
  , ( "Juny"     , "juny?\\.?")
  , ( "Juliol"   , "juliol|jul\\.?")
  , ( "Agost"    , "agost|ago\\.?")
  , ( "Setembre" , "setembre|set\\.?")
  , ( "Octubre"  , "octubre|oct\\.?")
  , ( "Novembre" , "novembre|nov\\.?")
  , ( "Desembre" , "desembre|des\\.?")
  ]

ruleThisDayofweek :: Rule
ruleThisDayofweek = Rule
  { name = "this <day-of-week>"
  , pattern =
    [ regex "aquest"
    , Predicate isADayOfWeek
    ]
  , prod = \case
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\s?-\\s?|\\sal?\\s"
    , Predicate isNotLatent
    ]
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Open td1 td2
      _ -> Nothing
  }

ruleEvening :: Rule
ruleEvening = Rule
  { name = "evening"
  , pattern =
    [ regex "vespre"
    ]
  , prod = \_ ->
      let from = hour False 19
          to = hour False 22
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

-- Afegit Xavier Plaza
ruleMatinada:: Rule
ruleMatinada = Rule
  { name = "matinada"
  , pattern =
    [ regex "matinada"
    ]
  , prod = \_ ->
      let from = hour False 1
          to = hour False 6
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleNit:: Rule
ruleNit = Rule
  { name = "nit"
  , pattern =
    [ regex "nit"
    ]
  , prod = \_ ->
      let from = hour False 22
          to = hour False 1
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleMigdia:: Rule
ruleMigdia = Rule
  { name = "migdia"
  , pattern =
    [ regex "migdia|mig dia"
    ]
  , prod = \_ ->
      let from = hour False 11
          to = hour False 14
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleDayOfMonthSt :: Rule
ruleDayOfMonthSt = Rule
  { name = "day of month (1st)"
  , pattern =
    [ regex "primer|u|1er( de)?"
    ]
  , prod = \_ -> tt $ dayOfMonth 1
  }

ruleEnDuration :: Rule
ruleEnDuration = Rule
  { name = "en <duration>"
  , pattern =
    [ regex "\\sen\\s"
    , dimension Duration
    ]
  , prod = \case
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleToday :: Rule
ruleToday = Rule
  { name = "today"
  , pattern =
    [ regex "avui"
    ]
  , prod = \_ -> tt today
  }

ruleUltimoDayofweekDeTime :: Rule
ruleUltimoDayofweekDeTime = Rule
  { name = "ultimo <day-of-week> de <time>"
  , pattern =
    [ regex "darrer"
    , Predicate isADayOfWeek
    , regex "de|d'"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleEntreDatetimeEtDatetimeInterval :: Rule
ruleEntreDatetimeEtDatetimeInterval = Rule
  { name = "entre <datetime> et <datetime> (interval)"
  , pattern =
    [ regex "(entre|entre les|entre la)"
    , dimension Time
    , regex "(i|i les|i la)"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Open td1 td2
      _ -> Nothing
  }

ruleHhhmmTimeOfDay :: Rule
ruleHhhmmTimeOfDay = Rule
  { name = "hh(:|.|h)mm (time-of-day)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:h\\.]([0-5]\\d)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleElDayOfMonthDeNamedMonth :: Rule
ruleElDayOfMonthDeNamedMonth = Rule
  { name = "el <day-of-month> de <named-month>"
  , pattern =
    [ regex "el|l'"
    , Predicate isDOMInteger
    , regex "de|d'"
    , Predicate isAMonth
    ]
  , prod = \case
      (_:t:_:Token Time td:_) -> Token Time <$> intersectDOM td t
      _ -> Nothing
  }

ruleNPasadosCycle :: Rule
ruleNPasadosCycle = Rule
  { name = "n pasados <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "passa(ts?|da|des)|darrer(s|a|es)?"
    , dimension TimeGrain
    ]
  , prod = \case
      (t:_:Token TimeGrain grain:_) -> do
        v <- getIntValue t
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleElProximoCycle :: Rule
ruleElProximoCycle = Rule
  { name = "el proximo <cycle> "
  , pattern =
    [ regex "((el|els|la|les) )?"
    , regex "proper(a|s|es)?"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

rulePasadosNCycle :: Rule
rulePasadosNCycle = Rule
  { name = "pasados n <cycle>"
  , pattern =
    [ regex "passa(ts?|da|des)|darrer(s|a|es)?"
    , Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    ]
  , prod = \case
      (_:t:Token TimeGrain grain:_) -> do
        v <- getIntValue t
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleElDayOfMonthNonOrdinal :: Rule
ruleElDayOfMonthNonOrdinal = Rule
  { name = "el <day-of-month> (non ordinal)"
  , pattern =
    [ regex "el|l'"
    , Predicate $ isIntegerBetween 1 31
    ]
  , prod = \case
      (_:t:_) -> do
        v <- getIntValue t
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "migdia|mig dia"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleProximasNCycle :: Rule
ruleProximasNCycle = Rule
  { name = "proximas n <cycle>"
  , pattern =
    [ regex "proper(a|s|es)?"
    , Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    ]
  , prod = \case
      (_:t:Token TimeGrain grain:_) -> do
        v <- getIntValue t
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleNochevieja :: Rule
ruleNochevieja = Rule
  { name = "Nochevieja"
  , pattern =
    [ regex "(nit de cap d'any|darrer dia de l'any)"
    ]
  , prod = \_ -> tt $ monthDay 12 31
  }

ruleTheDayBeforeYesterday :: Rule
ruleTheDayBeforeYesterday = Rule
  { name = "the day before yesterday"
  , pattern =
    [ regex "abans d'ahir"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day (-2)
  }

-- TODO: add within/after
ruleDurationIn :: Rule
ruleDurationIn = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "en"
    , dimension Duration
    ]
  , prod = \case
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ regex "fa"
    , dimension Duration
    ]
  , prod = \case
      (_:Token Duration dd:_) -> tt $ durationAgo dd
      _ -> Nothing
  }

ruleHourOfDayIntegerAsRelativeMinutes :: Rule
ruleHourOfDayIntegerAsRelativeMinutes = Rule
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

-- Hores format campanar --
ruleHourOfDayMigQuart :: Rule
ruleHourOfDayMigQuart = Rule
  { name = "mig quart de <hour-of-day>"
  , pattern =
    [ regex "mig quart (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 53 td
      _ -> Nothing
  }

ruleHourOfDayUnQuart :: Rule
ruleHourOfDayUnQuart = Rule
  { name = "(un|1)(\\squart|\\/4) de <hour-of-day>"
  , pattern =
    [ regex "un quart (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 45 td
      _ -> Nothing
  }

ruleHourOfDayUnQuartiCinc :: Rule
ruleHourOfDayUnQuartiCinc = Rule
  { name = "un quart i cinc de <hour-of-day>"
  , pattern =
    [ regex "((un quart|1\\/4) i cinc|(dos|2)(\\squarts|\\/4) menys deu) (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 40 td
      _ -> Nothing
  }

ruleHourOfDayUnQuartiMig :: Rule
ruleHourOfDayUnQuartiMig = Rule
  { name = "un quart i mig de <hour-of-day>"
  , pattern =
    [ regex "(un|1)(\\squart|\\/4) i mig (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 38 td
      _ -> Nothing
  }

ruleHourOfDayUnQuartiDeu :: Rule
ruleHourOfDayUnQuartiDeu = Rule
  { name = "un quart i deu de <hour-of-day>"
  , pattern =
    [ regex "((un|1)(\\squart|\\/4) i deu|(dos|2)(\\squarts|\\/4) menys cinc) (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 35 td
      _ -> Nothing
  }

ruleHourOfDayDosQuarts :: Rule
ruleHourOfDayDosQuarts = Rule
  { name = "dos quarts de <hour-of-day>"
  , pattern =
    [ regex "((dos|2)(\\squarts|\\/4)|quarts) (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleHourOfDayDosQuartsiCinc :: Rule
ruleHourOfDayDosQuartsiCinc = Rule
  { name = "dos quarts i cinc de <hour-of-day>"
  , pattern =
    [ regex "((dos|2)(\\squarts|\\/4) i cinc|(tres|3)(\\squarts|\\/4) menys deu) (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 25 td
      _ -> Nothing
  }

ruleHourOfDayDosQuartsiMig :: Rule
ruleHourOfDayDosQuartsiMig = Rule
  { name = "dos quarts i mig de <hour-of-day>"
  , pattern =
    [ regex "(dos|2)(\\squarts|\\/4) i mig (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 23 td
      _ -> Nothing
  }

ruleHourOfDayDosQuartsiDeu :: Rule
ruleHourOfDayDosQuartsiDeu = Rule
  { name = "Dos quarts i deu de <hour-of-day>"
  , pattern =
    [ regex "((dos|2)(\\squarts|\\/4) i deu|(tres|3)(\\squarts|\\/4) menys cinc) (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 20 td
      _ -> Nothing
  }

ruleHourOfDayTresQuarts :: Rule
ruleHourOfDayTresQuarts = Rule
  { name = "tres quarts de <hour-of-day>"
  , pattern =
    [ regex "(tres|3)(\\squarts|\\/4) (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleHourOfDayTresQuartsiCinc :: Rule
ruleHourOfDayTresQuartsiCinc = Rule
  { name = "Tres quarts i cinc de <hour-of-day>"
  , pattern =
    [ regex "(tres|3)(\\squarts|\\/4) i cinc (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 10 td
      _ -> Nothing
  }

ruleHourOfDayTresQuartsiMig :: Rule
ruleHourOfDayTresQuartsiMig = Rule
  { name = "Tres quarts i mig de <hour-of-day>"
  , pattern =
    [ regex "(tres|3)(\\squarts|\\/4) i mig (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 8 td
      _ -> Nothing
  }

ruleHourOfDayTresQuartsiDeu :: Rule
ruleHourOfDayTresQuartsiDeu = Rule
  { name = "Tres quarts i deu de <hour-of-day>"
  , pattern =
    [ regex "(tres|3)(\\squarts|\\/4) i deu (de |d')"
    , Predicate isAnHourOfDay
    ]
  , prod = \case
      (Token Time td:_) -> Token Time <$> minutesBefore 20 td
      _ -> Nothing
  }

-- Final Hores format campanar --

ruleHourOfDayIntegerAsRelativeMinutes2 :: Rule
ruleHourOfDayIntegerAsRelativeMinutes2 = Rule
  { name = "<hour-of-day> <integer> (as relative minutes) minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(ut)?s?"
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       t:
       _) -> do
        n <- getIntValue t
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHourOfDayQuarter :: Rule
ruleHourOfDayQuarter = Rule
  { name = "<hour-of-day> quarter (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "quart"
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHourOfDayHalf :: Rule
ruleHourOfDayHalf = Rule
  { name = "<hour-of-day> half (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "mitja|(2|dos)(\\squarts|\\/4)"
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHourOfDayThreeQuarter :: Rule
ruleHourOfDayThreeQuarter = Rule
  { name = "<hour-of-day> three quarters (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(3|tres)(\\squarts|\\/4)"
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 45
      _ -> Nothing
  }

ruleHourOfDayAndRelativeMinutes :: Rule
ruleHourOfDayAndRelativeMinutes = Rule
  { name = "<hour-of-day> and <relative minutes>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "i"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _:
       t:
       _) -> do
        n <- getIntValue t
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHourOfDayAndRelativeMinutes2 :: Rule
ruleHourOfDayAndRelativeMinutes2 = Rule
  { name = "<hour-of-day> and <relative minutes> minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "i"
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(ut)?s?"
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _:
       t:
       _) -> do
        n <- getIntValue t
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHourOfDayAndQuarter :: Rule
ruleHourOfDayAndQuarter = Rule
  { name = "<hour-of-day> and quarter"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "i quart"
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHourOfDayAndHalf :: Rule
ruleHourOfDayAndHalf = Rule
  { name = "<hour-of-day> and half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "i (mitja|(2|dos)(\\squarts|\\/4))"
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHourOfDayAndThreeQuarter :: Rule
ruleHourOfDayAndThreeQuarter = Rule
  { name = "<hour-of-day> and 3 quarters"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "i (tres|3)(\\squarts|\\/4)"
    ]
  , prod = \case
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:_) ->
        tt $ hourMinute is12H hours 45
      _ -> Nothing
  }

ruleDelYear :: Rule
ruleDelYear = Rule
  { name = "del <year>"
  , pattern =
    [ regex "de(l| l'any)? "
    , Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \case
      (_:t:_) -> do
        v <- getIntValue t
        tt $ year v
      _ -> Nothing
  }

-- Revisar per que diria que no va bé
ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd[/-]mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/-](0?[1-9]|1[0-2])( |$)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        d <- parseInt m1
        m <- parseInt m2
        tt $ monthDay m d
      _ -> Nothing
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd[/-.]mm[/-.]yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[\\./-](0?[1-9]|1[0-2])[\\./-](\\d{2,4})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        d <- parseInt m1
        m <- parseInt m2
        y <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "tarda"
    ]
  , prod = \_ ->
      let from = hour False 14
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "midnight"
  , pattern =
    [ regex "mitjanit|mitja nit"
    ]
  , prod = \_ -> tt $ hour False 0
  }

rulePODIn :: Rule
rulePODIn = Rule
  { name = "in the <part-of-day>"
  , pattern =
    [ regex "(a la|al|per la|pel|del)"
    , Predicate isAPartOfDay
    ]
  , prod = \case
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePODThis :: Rule
rulePODThis = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "aquesta?"
    , Predicate isAPartOfDay
    ]
  , prod = \case
      (_:Token Time td:_) -> Token Time . partOfDay . notLatent <$> intersect today td
      _ -> Nothing
  }

ruleTODPOD :: Rule
ruleTODPOD = Rule
  { name = "<time-of-day> <part-of-day>"
  , pattern =
    [ Predicate isATimeOfDay
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Time pod:_) -> Token Time . notLatent <$> intersect td pod
      _ -> Nothing
  }

ruleAnoNuevo :: Rule
ruleAnoNuevo = Rule
  { name = "ano nuevo"
  , pattern =
    [ regex "cap d'any"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleDdddMonthinterval :: Rule
ruleDdddMonthinterval = Rule
  { name = "dd-dd <month>(interval)"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])" -- Revisar per que diria que no va bé
    , regex "\\-|al?"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "de|d'"
    , Predicate isAMonth
    ]
  , prod = \case
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

ruleTimeOfDayLatent :: Rule
ruleTimeOfDayLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \case
      (t:_) -> do
        v <- getIntValue t
        tt $ mkLatent $ hour (v < 13) v
      _ -> Nothing
  }

ruleNamedMonthNamedDayPast :: Rule
ruleNamedMonthNamedDayPast = Rule
  { name = "<named-month|named-day> past"
  , pattern =
    [ dimension Time
    , regex "passat"
    ]
  , prod = \case
      (Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleNamedMonthNamedDayPast2 :: Rule
ruleNamedMonthNamedDayPast2 = Rule
  { name = "<named-month|named-day> past"
  , pattern =
    [ regex "passat|darrer|l'ultim"
    , dimension Time
    ]
  , prod = \case
      (Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "estiu"   , "estiu"   , monthDay  6 21, monthDay  9 23 )
  , ( "tardor"    , "tardor", monthDay  9 23, monthDay 12 21 )
  , ( "hivern" , "hivern" , monthDay 12 21, monthDay  3 20 )
  , ( "primavera", "primavera", monthDay  3 20, monthDay  6 21 )
  ]

ruleRightNow :: Rule
ruleRightNow = Rule
  { name = "right now"
  , pattern =
    [ regex "ara( mateix)?|ja|en aquests? moments?|tant aviat com puguis?"
    ]
  , prod = \_ -> tt now
  }

ruleNCycleVinent :: Rule
ruleNCycleVinent = Rule
  { name = "n <cycle> (proximo|que viene)"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(vinents?)"
    ]
  , prod = \case
      (t:Token TimeGrain grain:_) -> do
        v <- getIntValue t
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleNamedMonthNamedDayNext :: Rule
ruleNamedMonthNamedDayNext = Rule
  { name = "<named-month|named-day> next"
  , pattern =
    [ dimension Time
    , regex "vinents?"
    ]
  , prod = \case
      (Token Time td:_) ->
        tt $ predNth 1 False td
      _ -> Nothing
  }

-- afegir la mateixa regla però ficant devant proper

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isNotLatent
    , Predicate isNotLatent
    ]
  , prod = \case
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleDeDatetimeDatetimeInterval :: Rule
ruleDeDatetimeDatetimeInterval = Rule
  { name = "de <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "(des )?del?"
    , dimension Time
    , regex "\\-|(fins )?al?"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Open td1 td2
      _ -> Nothing
  }

ruleNthTimeDeTime2 :: Rule
ruleNthTimeDeTime2 = Rule
  { name = "nth <time> de <time>"
  , pattern =
    [ regex "(el|la)"
    , dimension Ordinal
    , dimension Time
    , regex "de|d'"
    , dimension Time
    ]
  , prod = \case
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
    [ regex "dintre de"
    , dimension Duration
    ]
  , prod = \case
      (_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "week[ -]?end|cap de setmana"
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
  , prod = \case
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
  , prod = \case
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m1
        m <- parseInt m2
        d <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTimeOfDayHoras :: Rule
ruleTimeOfDayHoras = Rule
  { name = "<time-of-day> horas"
  , pattern =
    [ regex "(la | les )?"
    , Predicate isATimeOfDay
    , regex "h?\\.?(or(e|a))?s?"
    ]
  , prod = \case
      (Token Time td:_) ->
        tt $ mkLatent td
      _ -> Nothing
  }

ruleElCycleAntesTime :: Rule
ruleElCycleAntesTime = Rule
  { name = "el <cycle> antes <time>"
  , pattern =
    [ regex "(el|la|l')? ?"
    , dimension TimeGrain
    , regex "abans de"
    , dimension Time
    ]
  , prod = \case
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
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "mat(i|í)"
    ]
  , prod = \_ ->
      let from = hour False 6
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleALasHourminTimeOfDay :: Rule
ruleALasHourminTimeOfDay = Rule
  { name = "a las <hour-min>(time-of-day)"
  , pattern =
    [ regex "a (l'|les)"   -- es pot escriure a la una
    , Predicate isATimeOfDay
    , regex "horas?"
    ]
  , prod = \case
      ( _:
        x:
        _) -> Just x
      _ -> Nothing
  }

-- Potser cal afegir el mateix però amb darrers
ruleLaCyclePasado :: Rule
ruleLaCyclePasado = Rule
  { name = "la <cycle> pasado"
  , pattern =
    [ regex "(el|els|la|les|l')?"
    , dimension TimeGrain
    , regex "passa(t|da|ts|des)"
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain (-1)
      _ -> Nothing
  }


ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
      [ Predicate $ isIntegerBetween 25 10000
      ]
  , prod = \case
      (t:_) -> do
        n <- getIntValue t
        tt $ mkLatent $ year n
      _ -> Nothing
  }

ruleYesterday :: Rule
ruleYesterday = Rule
  { name = "yesterday"
  , pattern =
    [ regex "ahir"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day (-1)
  }

ruleDayOfWeekDayOfMonth :: Rule
ruleDayOfWeekDayOfMonth = Rule
  { name = "<day-of-week> <day-of-month>"
  , pattern =
    [ Predicate isADayOfWeek
    , Predicate isDOMInteger
    ]
  , prod = \case
      (Token Time td:t:_) -> Token Time <$> intersectDOM td t
      _ -> Nothing
  }

ruleTimeOfDayAmpm :: Rule
ruleTimeOfDayAmpm = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "([ap])\\.?m?\\.?"
    ]
  , prod = \case
      (Token Time td:Token RegexMatch (GroupMatch (ap:_)):_) ->
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
  }

ruleDayOfMonthDeNamedMonth :: Rule
ruleDayOfMonthDeNamedMonth = Rule
  { name = "<day-of-month> de <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , regex "de|d'"
    , Predicate isAMonth
    ]
  , prod = \case
      (t:_:Token Time td:_) -> Token Time <$> intersectDOM td t
      _ -> Nothing
  }

ruleEntreDdEtDdMonthinterval :: Rule
ruleEntreDdEtDdMonthinterval = Rule
  { name = "entre dd et dd <month>(interval)"
  , pattern =
    [ regex "entre( el)?|des del?"
    , regex "(0?[1-9]|[12]\\d|3[01])"
    , regex "i( el)?|fins al?"
    , regex "(0?[1-9]|[12]\\d|3[01])"
    , regex "de|d'"
    , Predicate isAMonth
    ]
  , prod = \case
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

ruleNamedMonthDayOfMonth :: Rule
ruleNamedMonthDayOfMonth = Rule
  { name = "<named-month> <day-of-month>"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \case
      (Token Time td:t:_) -> Token Time <$> intersectDOM td t
      _ -> Nothing
  }

ruleElTime :: Rule
ruleElTime = Rule
  { name = "el <time>"
  , pattern =
    [ regex "d?el"
    , Predicate isNotLatent
    ]
  , prod = \case
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \case
      (t:_) -> do
        v <- getIntValue t
        tt $ year v
      _ -> Nothing
  }

ruleEsteenUnCycle :: Rule
ruleEsteenUnCycle = Rule
  { name = "este|en un <cycle>"
  , pattern =
    [ regex "(aquesta?|dins (els?|la|l'|les)?)"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleNProximasCycle :: Rule
ruleNProximasCycle = Rule
  { name = "n proximas <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "proper(a|es|s)?"
    , dimension TimeGrain
    ]
  , prod = \case
      (t:_:Token TimeGrain grain:_) -> do
        v <- getIntValue t
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleLaPasadoCycle :: Rule
ruleLaPasadoCycle = Rule
  { name = "la pasado <cycle>"
  , pattern =
    [ regex "(el|els|la|les) ?"
    , regex "passa(ts?|da|des)|darrer(a|s|es)?"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain (-1)
      _ -> Nothing
  }

ruleALasTimeOfDay :: Rule
ruleALasTimeOfDay = Rule
  { name = "a las <time-of-day>"
  , pattern =
    [ regex "a?( las?)?|las?"
    , Predicate isATimeOfDay
    ]
  , prod = \case
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleOrdinalQuarter :: Rule
ruleOrdinalQuarter = Rule
  { name = "<ordinal> quarter"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \case
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        tt $ cycleNthAfter False TG.Quarter (v - 1) $ cycleNth TG.Year 0
      _ -> Nothing
  }

ruleElCycleVinent :: Rule
ruleElCycleVinent = Rule
  { name = "el <cycle> (proximo|que viene)"
  , pattern =
    [ regex "(el|els|la|les|l') ?"
    , dimension TimeGrain
    , regex "(vinents?)"
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleElCycleVinentTime :: Rule
ruleElCycleVinentTime = Rule
  { name = "el <cycle> proximo|que viene <time>"
  , pattern =
    [ regex "(el|els|la|les|l') ?"
    , dimension TimeGrain
    , regex "(vinents?)"
    , dimension Time
    ]
  , prod = \case
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleDelMedioda :: Rule
ruleDelMedioda = Rule
  { name = "del mediodía"
  , pattern =
    [ regex "del migdia"
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
    , regex "de|d'"
    , Predicate isNotLatent
    ]
  , prod = \case
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "dem(a|à)"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleNthTimeDeTime :: Rule
ruleNthTimeDeTime = Rule
  { name = "nth <time> de <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "de|d'"
    , dimension Time
    ]
  , prod = \case
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
  , prod = \case
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleNextWeek :: Rule
ruleNextWeek = Rule
  { name = "next week"
  , pattern =
    [ Predicate $ isGrain TG.Week
    , regex "vinent"
    ]
  , prod = \_ -> tt $ cycleNth TG.Week 1
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "Navidad", "(el )?nadal", monthDay 12 25)
  , ( "Dia de África", "dia de (la (libertad|liberacion) )?(á|a)frica(na)?", monthDay 5 25 )
  , ( "Dia de la Industrialización de África", "dia de (la )?industrializaci(ó|o)n de (á|a)frica(na)?", monthDay 11 20 )
  -- dates assenyalades catalanes
  , ( "Diada de Tots Sants", "((dia|diada|festivitat) de )?tots sants", monthDay 11 1 )
  , ( "Dia de difunts", "(dia|diada) de(ls)? difunts", monthDay 11 2 )
  , ( "Epifanía", "Epifania|((dia|diada) dels)? reis( mags)?( d'orient)?", monthDay 1 6 )
  , ( "Festivitat de la Purissima", "(dia|diada|festivitat) de la (purissima|immaculada) (concepci(ó|o)( de (maria|la mare de déu))?)?", monthDay 12 8 )
  , ( "Assumpció de Maria", "assumpci(ó|o) de (mar(í|i)a|la mare de déu)", monthDay 8 15 )
  , ( "Diada de Sant Jordi", "((dia|diada|festivitat) de )?sant jordi", monthDay 4 23 )
  , ( "Diada de Sant Esteve", "((dia|diada|festivitat) de )?sant esteve", monthDay 12 26 )
  , ( "Diada de Sant Joan", "((dia|diada|festivitat) de )?sant joan", monthDay 6 24 )
  , ( "Revetlla de Sant Joan", "(revetlla|nit) de sant joan", monthDay 6 23 )
  , ( "Diada de Sant Narcís", "((dia|diada|festivitat) de )?sant narc(i|í)s", monthDay 10 29 )
  , ( "Diada de Sant Josep", "((dia|diada|festivitat) de )?sant? jos(e|é)p?", monthDay 3 19 )
  , ( "Diada de la Mercè", "((dia|diada|festivitat) de )?(la mare de déu de)? la merc(è|é)", monthDay 9 24 )
  , ( "Diada de Santa Tecla", "((dia|diada|festivitat) de )?santa tecla", monthDay 9 23 )
  , ( "Diada de Sant Anastasi", "((dia|diada|festivitat) de )?sant anastasi", monthDay 5 11 )
  , ( "Diada de Montserrat", "((dia|diada|festivitat) de )?(la mare de déu de)? montserrat", monthDay 4 27 )
  -- Fi dates catalanes
  , ( "Dia dels Innocents d'Abril", "dia (dels innocents|les bromes)( d'abril)?", monthDay 4 1 )
  , ( "Dia Mundial de la Lengua Àrab", "dia mundial de la lengua (á|a)rabe", monthDay 12 18 )
  , ( "Dia de la Boxa", "(el )?dia de (la boxa|l'(B|b)oxeador)", monthDay 12 26 )
  , ( "Dia de Llengua Xinesa", "dia de llengua xinesa", monthDay 4 20 )
  --, ( "Nochebuena", "(la (cena de )?)?(nochebuena|noche buena)", monthDay 12 24 )
  , ( "Dia de Commemoració de les Víctimes de la Guerra Química", "dia (de )?(commemoratiu|conmemoraci(ó|o)) (en record )?de las v(í|i)ctimes de la guerra qu(í|i)mica", monthDay 4 29 )
  , ( "Dia de Commemoració de les Víctimes de l'Genocidi de Rwanda", "dia (de )?(commemoratiu|conmemoraci(ó|o)) (en record )?de las v(í|i)ctimes de l'genocidi de (Rw|Ru|rw|ru)anda", monthDay 4 7 )
  , ( "Dia de la Marina", "dia (de la marina|l' mar(í|i))", monthDay 6 25 )
  , ( "Dia de la Terra", "dia (internacional )?de la (mare )?terra", monthDay 4 22 )
  , ( "Dia de la Llengua Anglesa", "dia de la llengua anglesa", monthDay 4 23 )
  , ( "Les Festes de Sant Francesc d'Assís", "(les )?festes de sant (francesc d'as(í|i)s|pacho)", monthDay 10 4 )
  , ( "Dia Mundial de les Mares i dels Pares", "dia mundial de les mades y dels pares", monthDay 6 1 )
 -- , ( "Dia de Todos los Santos", "(dia|festividad) de todos los santos", monthDay 10 31 ) Tots sants és l'1 de novembre
  , ( "Dia dels Drets Humans", "dia dels drets humans", monthDay 12 10 )
  , ( "Dia Internacional de Sensibilització sobre el Albinismo", "dia internacional de sensibilitzaci(ó|o) sobre l'albinisme", monthDay 6 13 )
  , ( "Dia Internacional contra la Corrupció", "dia internacional contra la corrupci(ó|o)", monthDay 12 9 )
  , ( "Dia Internacional dels Asteroides", "dia internacional dels asteroides", monthDay 6 30 )
  , ( "Celebra el Dia Internacional de la Bisexualitat", "celebra el dia internacional de la bisexualitat", monthDay 9 23 )
  , ( "Dia Internacional de Recordación de Desastres de Txernòbil", "dia internacional de record de desastres de Txern(ò|o)bil", monthDay 4 26 )
  , ( "Dia Internacional de l'Aviació Civil", "dia internacional de l'aviaci(ó|o) civil", monthDay 12 7 )
  , ( "Dia Internacional de la Duana", "dia internacional de l(a|es) duan(a|es)", monthDay 1 26 )
  , ( "Dia Internacional de la Lluita contra l'Ús Indegut i el Tràfic Il·lícit de Drogues", "dia internacional de la lluita contra l'(ú|u)s indegut i el tr(à|a)fic il·l(í|i)cit de drogues", monthDay 6 26 )
  , ( "Dia Internacional contra els Assajos Nuclears", "dia internacional contra els assajos nuclears", monthDay 8 29 )
  , ( "Dia Internacional de la Diversitat Biològica", "dia internacional de la diversitat biol(ò|o)gica", monthDay 5 22 )
  , ( "Dia Internacional dels Monuments i Llocs", "dia internacional dels monuments i llocs", monthDay 4 18 )
  , ( "Dia Internacional per a la Prevenció de l'Explotació de l'entorn en la Guerra i els Conflictes Armats", "dia internacional per a la prevenci(ó|o) de l'explotaci(ó|o) de l'entorn a la guerra i els conflictes armats", monthDay 11 6 )
  , ( "Dia de les Nacions Unides per a la Cooperació Sud-Sud", "dia de les nacions unides per a la cooperació sud(\\-|\\s)sud", monthDay 9 12 )
  , ( "Dia Internacional per la Tolerància", "dia internacional (de|per) la toler(à|a)ncia", monthDay 11 16 )
  , ( "Dia Internacional per a l'Abolició de l'Esclavitud", "dia internacional per a l'abolici(ó|o) de l'esclavitud", monthDay 12 2 )
  , ( "Dia Internacional de l'Eliminació de la Discriminació Racial", "dia internacional de l'eliminaci(ó|o) de la discriminaci(ó|o) racial", monthDay 3 21 )
  , ( "Dia Internacional de l'Eliminació de la Violència Sexual en els Conflictes", "dia internacional de l'eliminaci(ó|o) de la viol(è|e)ncia sexual en els conflictes", monthDay 6 19 )
  , ( "Dia Internacional de l'Eliminació de la Violència contra la Dona", "dia internacional de l'eliminaci(ó|o) de la viol(è|e)ncia contra (la dona|les dones)", monthDay 11 25 )
  , ( "Dia Internacional per a l'Eradicació de la Pobresa", "dia internacional per a l'eradicaci(ó|o) de la pobresa", monthDay 10 17 )
  , ( "Dia Internacional de la Preservació de la Capa d'Ozó", "dia internacional de la preservaci(ó|o) de la capa d'oz(ó|o)", monthDay 9 16 )
  , ( "Dia Internacional de l'Record del Tràfic d'Esclaus i la seva Abolició", "dia internacional de l'record del tr(à|a)fic d'esclaus i de la seva abolici(ó|o)", monthDay 8 23 )
  , ( "Dia Internacional del Derecho a la Verdad en relación con Violaciones Graves de los Derechos Humanos y de la Dignidad de las Víctimas de las Naciones Unidas", "dia internacional del derecho a la verdad en relaci(ó|o)n con violaciones graves de los derechos humanos y de la dignidad de las v(í|i)ctimas de las naciones unidas", monthDay 3 24 )
  , ( "Dia Internacional per a l'Eliminació Total de les Armes Nuclears", "dia internacional per a l'eliminaci(ó|o) total de les armes nuclears", monthDay 9 26 )
  , ( "Dia Internacional de les Nacions Unides a Suport de les Víctimes de la Tortura", "dia internacional de les Nacions Unides en suport de les v(í|i)ctimes de la tortura", monthDay 6 26 )
  , ( "Dia Internacional de la Beneficència", "dia internacional de la benefic(è|e)ncia", monthDay 9 5 )
  , ( "Dia Internacional de Commemoració anual en memòria de les Víctimes de l'Holocaust", "dia internacional de commemoraci(ó|o) anual en mem(ò|o)ria de les v(í|i)ctimes de l'holocaust", monthDay 1 27 )
  , ( "Dia Internacional de la Democràcia", "dia (internacional)?de la democr(à|a)cia", monthDay 9 15 )
  , ( "Dia Internacional de les Persones amb Discapacitat", "dia internacional de les persones amb discapacitat", monthDay 12 3 )
  , ( "Dia Internacional de la Família", "dia internacional de l(a|es)? fam(í|i)li(a|es)?", monthDay 5 15 )
  , ( "Dia Internacional de les Remeses Familiars", "dia internacional de les remeses familiars", monthDay 6 16 )
  , ( "Dia Internacional dels Boscos", "dia internacional dels boscos", monthDay 3 21 )
  , ( "Dia Internacional de l'Amistat", "dia internacional de l'amistat", monthDay 7 30 )
  , ( "Dia Internacional de la Felicitat", "dia (internacional|mundial) de la felicitat", monthDay 3 20 )
  , ( "Dia Internacional dels Vols Espacials", "dia internacional (dels|del) vol(s)? espacial(s)?", monthDay 4 12 )
  , ( "Dia Internacional dels Nens Víctimes Innocents d'Agressió", "dia internacional dels nens v(í|i)ctimes innocents d'agressi(ó|o)", monthDay 6 4 )
  , ( "Dia Internacional de la no Violència", "dia internacional de la no viol(è|e)ncia", monthDay 10 2 )
  , ( "Dia Internacional de l'Nowruz", "dia internacional de l'Nowruz", monthDay 3 21 )
  , ( "Dia Internacional de la Gent Gran", "dia internacional de la gent gran", monthDay 10 1 )
  , ( "Dia Internacional de la Pau", "dia internacional de la pau", monthDay 9 21 )
  , ( "Dia Internacional de les Persones amb Discapacitat", "dia internacional de les persones amb discapacitat", monthDay 12 3 )
  , ( "Dia Internacional de Record de les Víctimes de l'Esclavitud i el Tràfic Transatlàntica d'Esclaus", "dia internacional de record de les v(í|i)ctimes de l'esclavitud i la tracta transatl(à|a)ntica d'esclaus", monthDay 3 25 )
  , ( "Dia Internacional de les Dones Rurals", "dia internacional de les dones rurals", monthDay 10 15 )
  , ( "Dia Internacional de Solidaritat amb els membres de l'Personal Detinguts o Desapareguts", "dia internacional de solidaritat amb els membres de l'personal detinguts o desapareguts", monthDay 3 25 )
  , ( "Dia Internacional de Solidaritat amb el Poble Palestí", "dia internacional de solidaritat amb el poble palestí", monthDay 11 29 )
  , ( "Dia Internacional de l'Esport per al Desenvolupament i la Pau", "dia internacional de l'esport per al desenvolupament i la pau", monthDay 4 6 )
  , ( "Dia Internacional de l'Personal de Pau de les Nacions Unides", "dia internacional de el personal de pau de les Nacions Unides", monthDay 5 29 )
  , ( "Dia Internacional de la Dona i la Nena a la Ciència", "dia internacional de la dona i la nena en la ci(è|e)ncia", monthDay 2 11 )
  , ( "Dia Internacional de l'Ioga", "dia Internacional de l'(I|i|Y|i)oga", monthDay 6 21 )
  , ( "Dia Internacional de Tolerància Zero amb la Mutilació Genital Femenina", "dia internacional de toler(à|a)ncia zero amb la mutilaci(ó|o) genital femenina", monthDay 2 6 )
  , ( "Dia Internacional de la Nena", "dia internacional de la nena", monthDay 10 11 )
  , ( "Dia Internacional de les Víctimes de Desaparicions Forçades", "dia internacional de les v(í|i)ctimes de desaparicions forçades", monthDay 8 30 )
  , ( "Dia Internacional dels Pobles Indígenes de l'Món", "dia internacional dels pobles ind(í|i)genes de l'm(ó|o)n", monthDay 8 9 )
  , ( "Dia Internacional per posar fi a la impunitat dels Crims contra Periodistes", "dia internacional per posar fi a la impunitat dels crims contra periodistes", monthDay 11 2 )
  , ( "Dia Internacional per a l'Eradicació de la Fístula Obstètrica", "dia internacional per a l'eradicaci(ó|o) de la f(í|i)stula obst(è|e)trica", monthDay 5 23 )
  , ( "Dia Internacional per a la Reducció dels Desastres Naturals", "dia internacional per a la reducci(ó|o) dels desastres (naturals)?", monthDay 10 13 )
  , ( "Dia Internacional de la Solidaritat Humana", "dia internacional de la solidaritat humana", monthDay 12 20 )
  , ( "Dia Internacional de l'Jazz", "dia internacional de jazz", monthDay 4 30 )
  , ( "Dia Internacional de l'Alfabetització", "dia internacional de l'alfabetitzaci(ó|o)", monthDay 9 8 )
  , ( "Dia Internacional de l'Home", "dia internacional de(l)? els home(s)?", monthDay 11 19 )
  , ( "Dia Internacional de l'Migrant", "dia internacional de l'migrant", monthDay 12 18 )
  , ( "Dia Internacional de la Llengua Materna", "dia (internacional|mundial) de la llengua materna", monthDay 2 21 )
  , ( "Dia Internacional de les Muntanyes", "dia internacional de les muntanyes", monthDay 12 11 )
  , ( "Dia Internacional de les Infermeres", "dia internacional de les infermeres", monthDay 5 12 )
  , ( "Dia de la Consciència Internacional sobre la Sobredosi", "dia de (la)? consciència internacional (de|sobre) la sobredosi", monthDay 8 31 )
  , ( "Dia Internacional de l'Voluntariat per al Desenvolupament Econòmic i Social", "dia internacional de l'voluntariat per al desenvolupament econ(ò|o)mic i social", monthDay 12 5 )
  , ( "Dia Internacional de les Vídues", "dia internacional de les v(í|i)dues", monthDay 6 23 )
  , ( "Dia Internacional de la Dona", "dia internacional de la dona", monthDay 3 8 )
  , ( "Dia Internacional de la Joventut", "dia internacional de la joventut", monthDay 8 12 )
  , ( "El Primer de Maig", "(el )?primer de maig", monthDay 5 1 )
  , ( "Dia Internacional de Nelson Mandela", "dia (internacional)? de nelson mandela", monthDay 7 18 )
  , ( "El Nadal Ortodoxa", "(la )?nadal ortodoxa", monthDay 1 7 )
  , ( "Any Nou Ortodox", "(l')?any (nou vell|vell ortodox|nou ortodox|nou juli(à|a))", monthDay 1 14 )
  , ( "Dia Mundial dels Serveis Públics", "dia (mundial|nacional) de(ls )?(serveis|servidor) p(ú|u)blic(s)?", monthDay 6 23 )
  , ( "Dia de Sant Patrici", "dia de sant patrici", monthDay 3 17 )
  , ( "Dia Internacional de l'Temps de l'Record i la Reconciliació per qui Van perdre la Vida durant la Segona Guerra Mundial", "dia internacional de el temps de l'record i la reconciliaci(ó|o) per als que van perdre la vida durant la segona guerra mundial", monthDay 5 8 )
  , ( "Dia de les Nacions Unides", "dia de les nacions unides", monthDay 10 24 )
  , ( "Dia Internacional d'Informació sobre el Perill de les Mines i d'Assistència per a les Activitats Relatives a les Mines", "dia internacional d'informaci(ó|o) sobre el perill de les mines i d'assist(è|e)ncia per a les activitats relatives a les mines", monthDay 4 4 )
  , ( "Dia Mundial de la Salut", "dia mundial de la salut (de les nacions unides)?", monthDay 4 7 )
  , ( "Dia Universal del Nen", "dia universal de( l'infant|l nen)", monthDay 11 20 )
  , ( "Dia de Sant Valentí", "(el )?dia de sant valent(í|i)", monthDay 2 14 )
  , ( "Dia Mundial de la lluita contra la SIDA", "dia mundial de( la lluita contra)?( la)?SIDA", monthDay 12 1 )
  , ( "Dia Mundial de Conscienciació sobre l'Autisme", "dia mundial de (la concienciaci(ó|o) sobre )?l'autismo", monthDay 4 2 )
  , ( "Dia Mundial de la Artritis Reumatoide", "dia mundial de l'artritis (reumatoide)?", monthDay 5 20 )
  , ( "Dia Mundial de l'Donant de Sang", "dia mundial de l'donant de sang", monthDay 6 14 )
  , ( "Dia Internacional de l'Llibre i de el Dret d'Autor", "dia internacional de l'libro i de( el|ls) dret(s)? d'autor", monthDay 4 23 )
  , ( "Dia Mundial de l'Braille", "dia mundial de l'braille", monthDay 1 4 )
  , ( "Dia Mundial contra el Càncer", "dia mundial contra el c(à|a)ncer", monthDay 2 4 )
  , ( "Dia Mundial de les Ciutats", "dia mundial de les ciutats", monthDay 10 31 )
  , ( "Dia Mundial de la PC", "dia mundial de la (pc|par(à|a)lisi cerebral)", monthDay 10 6 )
  , ( "Dia Mundial contra el Treball Infantil", "dia mundial contra el treball infantil", monthDay 6 12 )
  , ( "Dia Mundial contra el Tràfic de Persones", "dia mundial contra el tr(à|a)fic de persones", monthDay 7 30 )
  , ( "Dia Mundial de l'Patrimoni Audiovisual", "dia mundial (de l'|sobre el) patrimoni audiovisual", monthDay 10 27 )
  , ( "Dia Mundial de la Diversitat Cultural per al Diàleg i el Desenvolupament", "dia mundial de la diversitat cultural per al di(a|à)leg i el desenvolupament", monthDay 5 21 )
  , ( "Dia Mundial de la Seguretat i la Salut en el Treball", "dia mundial de la seguretat i la salut en el treball", monthDay 4 28 )
  , ( "Dia Mundial per l'Abolició de l'Esclavitud", "dia mundial per l'abolici(ó|o) de l'esclavitud", monthDay 12 2 )
  , ( "Dia Mundial de la Justícia Social", "dia mundial de la just(í|i)cia social", monthDay 2 20 )
  , ( "Dia Mundial de l'Malalt", "dia mundial (de l'|dels) malalts", monthDay 2 11 )
  , ( "Dia Mundial de Lluita contra la Desertificació i la Sequera", "dia mundial (de lluita contra|per combatre) la desertificaci(ó|o) i la sequera", monthDay 6 17 )
  , ( "Dia Mundial d'Informació sobre el Desenvolupament", "dia mundial d'informaci(ó|o) sobre el desenvolupament", monthDay 10 24 )
  , ( "Dia Mundial de la Diabetis", "dia mundial de la diabetis", monthDay 11 14 )
  , ( "Dia Mundial de la Síndrome de Down", "dia mundial de la s (í|i)ndrome de Down", monthDay 3 21 )
  , ( "Dia Mundial de Presa de Consciència de l'Abús i Maltractament en la Vellesa", "dia mundial de presa de consci(è|e)ncia de l'ab(ú|u)s i maltractament en la vellesa", monthDay 6 15 )
  , ( "Dia Mundial de l'Medi Ambient", "dia mundial de l'(entorn|medi ambient)", monthDay 6 5 )
  , ( "Dia Mundial de l'Alimentació", "dia mundial de l'alimentaci(ó|o)", monthDay 10 16 )
  , ( "Dia per a la Commemoració i Dignificació de les Víctimes de l'Crim de Genocidi i per a la seva prevenció", "dia per a la commemoraci(ó|o) i dignificaci(ó|o) de les v(í|i)ctimes de l'crim de genocidi i per a la seva prevenci(ó|o)", monthDay 12 9 )
  , ( "Dia Mundial de l'Cor", "dia mundial de l'cor", monthDay 9 29 )
  , ( "Dia Mundial de l'Hepatitis", "dia mundial (contra|de) l'hepatitis", monthDay 7 28 )
  , ( "Dia Mundial de l'Assistència Humanitària", "dia mundial de l'assist(è|e)ncia humanit(à|a)ria", monthDay 8 19 )
  , ( "Dia Internacional de la Societat de la Informació", "dia (internacional|mundial) de (les telecomunicacions i)? la societat de la informaci(ó|o)", monthDay 5 17 )
  , ( "Dia Mundial de la Propietat Intel·lectual", "dia mundial de la propietat intel·lectual", monthDay 4 26 )
  , ( "Dia Mundial de la Malària", "dia mundial de la mal(à|a)ria", monthDay 4 25 )
  , ( "Dia Mundial de la Salut Mental", "dia mundial de la salut mental", monthDay 10 10 )
  , ( "Dia Mundial de la Meteorologia", "dia (mundial de (la )?meteorologia|meteorol(ò|o)gic mundial)", monthDay 3 23 )
  , ( "Dia Mundial sense Tabac", "dia mundial sense tabac", monthDay 5 31 )
  , ( "Dia Mundial dels Oceans", "dia mundial dels oceans", monthDay 6 8 )
  , ( "Dia Mundial de l'Càncer de Ovari", "dia mundial de l'c(à|a)ncer d'ovari", monthDay 5 8 )
  , ( "Dia Mundial contra la Pneumònia", "dia mundial contra la pneum(ò|o)nia", monthDay 11 12 )
  , ( "Dia Mundial de la Poesia", "dia mundial de la poesia", monthDay 3 21 )
  , ( "Dia Mundial de la Població", "dia mundial de la poblaci(ó|o)", monthDay 7 11 )
  , ( "Dia Mundial del Correo", "Dia Mundial de l'Correu", monthDay 10 9 )
  , ( "Dia de la Prematuridad Mundial", "dia (de la prematuritat mundial|mundial de l'infant prematur)", monthDay 11 17 )
  , ( "Dia Mundial de la Llibertat de Premsa", "dia mundial de la llibertat de premsa", monthDay 5 3 )
  , ( "Dia Mundial contra la Ràbia", "dia mundial (contra | de) la r(à|a)bia", monthDay 9 28 )
  , ( "Dia Mundial de la Ràdio", "dia mundial de la r(à|a)dio", monthDay 2 13 )
  , ( "Dia Mundial dels Refugiats", "dia mundial (de l'|dels) refugiat(s)?", monthDay 6 20 )
  , ( "Dia Mundial de la Ciència per la Pau i el Desenvolupament", "dia mundial de la ci(è|e)ncia per a la pau i el desenvolupament", monthDay 11 10 )
  , ( "Dia Mundial de la Salut Sexual", "dmss|dia mundial de la salut sexual", monthDay 9 4 )
  , ( "Dia Mundial de l'Sòl", "dia mundial de terra", monthDay 12 5 )
  , ( "Dia Mundial de l'Accident Cerebrovascular", "dia mundial del (accident cerebrovascular|acv)", monthDay 10 29 )
  , ( "Dia Mundial per a la Prevenció de l'Suïcidi", "dia mundial per a la prevenci(ó|o)de l'su(ï|i)cidi", monthDay 9 10 )
  , ( "Dia Mundial dels Docents", "dia mundial (dels docents|de l'mestre)", monthDay 10 5 )
  , ( "Dia Mundial de la Televisió", "dia mundial de la televisi(ó|o)", monthDay 11 21 )
  , ( "Dia Mundial de l'bany", "dia mundial del (lavabo|v(à|a)ter)", monthDay 11 19 )
  , ( "Dia Mundial de l'Turisme", "dmt|dia mundial de l'turisme", monthDay 9 27 )
  , ( "Dia Mundial de la Tuberculosi", "dia mundial de (lluita contra)?la (tuberculosi|tb)", monthDay 3 24 )
  , ( "Dia Mundial de la Tonyina", "dia mundial de la tonyina", monthDay 5 2 )
  , ( "Dia Mundial del Veganismo", "dia mundial del veganismo", monthDay 11 1 )
  , ( "Dia Mundial del Vegetarianismo", "dia mundial del vegetarianismo", monthDay 10 1 )
  , ( "Dia Mundial del Agua", "dia mundial del agua", monthDay 3 22 )
  , ( "Dia Mundial de les Zones Humides", "dia mundial de les zones humides", monthDay 2 2 )
  , ( "Dia Mundial de la Vida Silvestre", "dia mundial de la vida silvestre", monthDay 3 3 )
  , ( "Dia Mundial de les Habilitats de la Joventut", "dia mundial de les habilitats de la joventut", monthDay 7 15 )
  , ( "Dia de la Zero Discriminació", "dia de la zero discriminació", monthDay 3 1 )

  -- Fixed day/week/month, year over year
  , ( "Dia de la Commonwealth", "dia de la commonwealth", nthDOWOfMonth 2 1 3 )
  , ( "Dia Mundial en Record de les Víctimes d'Accidents de Trànsit"
    , "dia (mundial )?en recor de las v(í|i)ctimes (d'accidents )?de tr(à|a)nsit"
    , nthDOWOfMonth 3 7 11 )
  , ( "Dia Internacional de les Cooperatives"
    , "dia internacional de les cooperatives", nthDOWOfMonth 1 6 7 )
  , ( "Dia de Martin Luther King"
    , "(MLK|dia de Martin Luther King,?)( Jr\\.?| J(ú|u)nior)?|dia dels drets humans a Idaho"
    , nthDOWOfMonth 3 1 1
    )

  -- The day after Thanksgiving (not always the fourth Friday of November)
  , ( "Divendres negre", "divendres negre|black( |-)?friday"
    , cycleNthAfter False TG.Day 1 $ nthDOWOfMonth 4 4 11
    )
  , ( "Dia Mundial de l'Hàbitat", "dia mundial de l'h(à|a)bitat", nthDOWOfMonth 1 1 10 )
  , ( "Dia Mundial de l'Ronyó", "dia mundial de l'rony(ó|o)", nthDOWOfMonth 2 4 3 )
  , ( "Dia Mundial de la Lepra", "dia mundial (contra|de) la lepra"
    , predLastOf (dayOfWeek 7) (month 1) )
  , ( "Dia Marítim Mundial", "dia mar(í|i)tim mundial"
    , predLastOf (dayOfWeek 4) (month 9) )
  , ( "Dia Mundial de les Aus Migratòries", "dia mundial de les aus migrat(ò|o)ries"
    , nthDOWOfMonth 2 6 5 )
  , ( "Dia mundial de la filosofia", "Dia mundial de la filosofia", nthDOWOfMonth 3 4 11 )
  , ( "Dia mundial de la religió", "dia mundial de (la|les)? religi(ó|ons)", nthDOWOfMonth 3 7 1 )
  , ( "Dia mundial de la vista", "dia mundial de la (vista|visi(ó|o))", nthDOWOfMonth 2 4 10 )

  -- Other
  , ( "Dia de el cap", "(el )?dia (nacional )?de el cap", predNthClosest 0 weekday (monthDay 10 16) )
  ]

ruleElDayOfMonthNonOrdinalWithDia :: Rule
ruleElDayOfMonthNonOrdinalWithDia = Rule
  { name = "dia <day-of-month> (non ordinal)"
  , pattern =
    [ regex "dia"
    , Predicate $ isIntegerBetween 1 31
    ]
  , prod = \case
      (_:t:_) -> do
        v <- getIntValue t
        tt $ dayOfMonth v
      _ -> Nothing
  }

ruleNextWeekAlt :: Rule
ruleNextWeekAlt = Rule
  { name = "next week (alt)"
  , pattern =
    [ regex "proper(a|s|es)|següents?"
    , Predicate $ isGrain TG.Week
    ]
  , prod = \_ -> tt $ cycleNth TG.Week 1
  }


-- vaig per aquí
ruleYearByAddingThreeNumbers :: Rule
ruleYearByAddingThreeNumbers = Rule
  { name    = "year (value by adding three composing numbers together)"
  , pattern =
    [ regex "mil"
    , Predicate $ isIntegerBetween 100 1000
    , Predicate $ isIntegerBetween 1 100
    ]
  , prod    = \case
      (_:t1:t2:_) -> do
        v1 <- getIntValue t1
        v2 <- getIntValue t2
        tt $ year $ 1000 + v1 + v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleALasHourminTimeOfDay
  , ruleALasTimeOfDay
  , ruleAfternoon
  , ruleAnoNuevo
  , ruleCeTime
  , ruleDatetimeDatetimeInterval
  , ruleDayOfMonthSt
  , ruleDayOfMonthDeNamedMonth
  , ruleDayOfWeekDayOfMonth
  , ruleDdddMonthinterval
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleDeDatetimeDatetimeInterval
  , ruleElCycleVinentTime
  , ruleDelMedioda
  , ruleDelYear
  , ruleDentroDeDuration
  , ruleElCycleAntesTime
  , ruleElCycleVinent
  , ruleElCycleVinent
  , ruleElDayOfMonthDeNamedMonth
  , ruleElDayOfMonthNonOrdinal
  , ruleElProximoCycle
  , ruleElTime
  , ruleEnDuration
  , ruleEntreDatetimeEtDatetimeInterval
  , ruleEntreDdEtDdMonthinterval
  , ruleEsteenUnCycle
  , ruleEvening
  , ruleHaceDuration
  , ruleHhhmmTimeOfDay
  , ruleHourOfDayMigQuart
  , ruleHourOfDayUnQuart
  , ruleHourOfDayUnQuartiCinc
  , ruleHourOfDayUnQuartiMig
  , ruleHourOfDayUnQuartiDeu
  , ruleHourOfDayDosQuarts
  , ruleHourOfDayDosQuartsiCinc
  , ruleHourOfDayDosQuartsiMig
  , ruleHourOfDayDosQuartsiDeu
  , ruleHourOfDayTresQuarts
  , ruleHourOfDayTresQuartsiCinc
  , ruleHourOfDayTresQuartsiMig
  , ruleHourOfDayTresQuartsiDeu
  , ruleHourOfDayAndRelativeMinutes
  , ruleHourOfDayIntegerAsRelativeMinutes
  , ruleIntersect
  , ruleIntersectByDe
  , ruleLaCyclePasado
  , ruleLaPasadoCycle
  , ruleMatinada
  , ruleNit
  , ruleMigdia
  , ruleMidnight
  , ruleMorning
  , rulePODIn
  , rulePODThis
  , ruleTODPOD
  , ruleNCycleVinent
  , ruleNPasadosCycle
  , ruleNProximasCycle
  , ruleNamedMonthDayOfMonth
  , ruleNamedMonthNamedDayNext
  , ruleNamedMonthNamedDayPast
  , ruleNamedMonthNamedDayPast2
  , ruleNochevieja
  , ruleNoon
  , ruleToday
  , ruleNthTimeDeTime
  , ruleNthTimeDeTime2
  , ruleOrdinalQuarter
  , ruleOrdinalQuarterYear
  , rulePasadosNCycle
  , ruleProximasNCycle
  , ruleRightNow
  , ruleTheDayAfterTomorrow
  , ruleTheDayBeforeYesterday
  , ruleThisDayofweek
  , ruleTimeOfDayAmpm
  , ruleTimeOfDayHoras
  , ruleTimeOfDayLatent
  , ruleTomorrow
  , ruleTwoTimeTokensSeparatedBy
  , ruleUltimoDayofweekDeTime
  , ruleWeekend
  , ruleYear
  , ruleYearLatent
  , ruleYesterday
  , ruleYyyymmdd
  , ruleHourOfDayAndThreeQuarter
  , ruleHourOfDayAndHalf
  , ruleHourOfDayAndQuarter
  , ruleHourOfDayAndRelativeMinutes2
  , ruleHourOfDayThreeQuarter
  , ruleHourOfDayHalf
  , ruleHourOfDayQuarter
  , ruleHourOfDayIntegerAsRelativeMinutes2
  , ruleTimezone
  , ruleElDayOfMonthNonOrdinalWithDia
  , ruleNextWeek
  , ruleNextWeekAlt
  , ruleYearByAddingThreeNumbers
  , ruleDurationIn
  , ruleDurationAgo
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ rulePeriodicHolidays
