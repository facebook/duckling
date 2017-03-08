-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.PT.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Number.Helpers (parseInt)
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
    [ regex "segunda((\\s|\\-)feira)?|seg\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 1
  }

ruleSHourmintimeofday :: Rule
ruleSHourmintimeofday = Rule
  { name = "às <hour-min>(time-of-day)"
  , pattern =
    [ regex "(\x00e0|a)s?"
    , Predicate isATimeOfDay
    , regex "horas?"
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleTheDayAfterTomorrow :: Rule
ruleTheDayAfterTomorrow = Rule
  { name = "the day after tomorrow"
  , pattern =
    [ regex "depois de amanh(\x00e3|a)"
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Day 2
  }

ruleNamedmonth12 :: Rule
ruleNamedmonth12 = Rule
  { name = "named-month"
  , pattern =
    [ regex "dezembro|dez\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 12
  }

ruleNamedday2 :: Rule
ruleNamedday2 = Rule
  { name = "named-day"
  , pattern =
    [ regex "ter(\x00e7|c)a((\\s|\\-)feira)?|ter\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 2
  }

ruleNatal :: Rule
ruleNatal = Rule
  { name = "natal"
  , pattern =
    [ regex "natal"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 12 25
  }

ruleNaoDate :: Rule
ruleNaoDate = Rule
  { name = "n[ao] <date>"
  , pattern =
    [ regex "n[ao]"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleIntersectByDaOrDe :: Rule
ruleIntersectByDaOrDe = Rule
  { name = "intersect by `da` or `de`"
  , pattern =
    [ Predicate isNotLatent
    , regex "d[ae]"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Just . Token Time $ intersect (td1, td2)
      _ -> Nothing
  }

rulePassadosNCycle :: Rule
rulePassadosNCycle = Rule
  { name = "passados n <cycle>"
  , pattern =
    [ regex "passad(a|o)s?"
    , Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        Just . Token Time $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "(\x00fa|u)ltim[ao]s?"
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
    [ regex "s(\x00e1|a)bado|s(\x00e1|a)b\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 6
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|a"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Just . Token Time $ interval False (td1, td2)
      _ -> Nothing
  }

ruleNamedmonth7 :: Rule
ruleNamedmonth7 = Rule
  { name = "named-month"
  , pattern =
    [ regex "julho|jul\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 7
  }

ruleEvening :: Rule
ruleEvening = Rule
  { name = "evening"
  , pattern =
    [ regex "noite"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Just . Token Time . mkLatent . partOfDay $ interval False (from, to)
  }

ruleDayOfMonthSt :: Rule
ruleDayOfMonthSt = Rule
  { name = "day of month (1st)"
  , pattern =
    [ regex "primeiro|um|1o"
    ]
  , prod = \_ -> Just . Token Time $ dayOfMonth 1
  }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "(hoje)|(neste|nesse) momento"
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Day 0
  }

ruleDimTimeDaMadrugada :: Rule
ruleDimTimeDaMadrugada = Rule
  { name = "<dim time> da madrugada"
  , pattern =
    [ dimension Time
    , regex "(da|na|pela) madruga(da)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        let td2 = mkLatent . partOfDay $ interval False (hour False 1, hour False 4)
        in Just . Token Time $ intersect (td, td2)
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
        Just . Token Time $ hourMinute True h m
      _ -> Nothing
  }

ruleNamedday4 :: Rule
ruleNamedday4 = Rule
  { name = "named-day"
  , pattern =
    [ regex "quinta((\\s|\\-)feira)?|qui\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 4
  }

ruleProximoCycle :: Rule
ruleProximoCycle = Rule
  { name = "proximo <cycle> "
  , pattern =
    [ regex "pr(\x00f3|o)xim(o|a)s?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        Just . Token Time $ cycleNth grain 1
      _ -> Nothing
  }

ruleCycleAntesDeTime :: Rule
ruleCycleAntesDeTime = Rule
  { name = "<cycle> antes de <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "antes d[eo]"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        Just . Token Time $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleEsteCycle :: Rule
ruleEsteCycle = Rule
  { name = "este <cycle>"
  , pattern =
    [ regex "(n?es[st](es?|as?))"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        Just . Token Time $ cycleNth grain 0
      _ -> Nothing
  }

ruleSHourminTimeofday :: Rule
ruleSHourminTimeofday = Rule
  { name = "às <hour-min> <time-of-day>"
  , pattern =
    [ regex "(\x00e0|a)s"
    , Predicate isNotLatent
    , regex "da"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Just . Token Time $ intersect (td1, td2)
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
      in Just . Token Time $ interval False (from, to)
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
        Just . Token Time . mkLatent $ year n
      _ -> Nothing
  }

ruleDiaDayofmonthDeNamedmonth :: Rule
ruleDiaDayofmonthDeNamedmonth = Rule
  { name = "dia <day-of-month> de <named-month>"
  , pattern =
    [ regex "dia"
    , Predicate isDOMInteger
    , regex "de|\\/"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "meio[\\s\\-]?dia"
    ]
  , prod = \_ -> Just . Token Time $ hour False 12
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
        Just . Token Time $ cycleN True grain v
      _ -> Nothing
  }

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "es[ts][ae]|pr(\x00f3|o)xim[ao]"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time $ predNth 0 True td
      _ -> Nothing
  }

ruleTheDayBeforeYesterday :: Rule
ruleTheDayBeforeYesterday = Rule
  { name = "the day before yesterday"
  , pattern =
    [ regex "anteontem|antes de ontem"
    ]
  , prod = \_ -> Just . Token Time . cycleNth TG.Day $ - 2
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
        Just . Token Time $ hourMinute is12H hours n
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
        Just . Token Time $ hourMinute is12H hours n
      _ -> Nothing
  }
ruleIntegerParaAsHourofdayAsRelativeMinutes :: Rule
ruleIntegerParaAsHourofdayAsRelativeMinutes = Rule
  { name = "<integer> para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "para ((o|a|\x00e0)s?)?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleHourofdayIntegerAsRelativeMinutes2 :: Rule
ruleHourofdayIntegerAsRelativeMinutes2 = Rule
  { name = "<hour-of-day> <integer> (as relative minutes) minutos"
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
        Just . Token Time $ hourMinute is12H hours n
      _ -> Nothing
  }
ruleHourofdayAndRelativeMinutes2 :: Rule
ruleHourofdayAndRelativeMinutes2 = Rule
  { name = "<hour-of-day> and <relative minutes> minutos"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e"
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(uto)?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        Just . Token Time $ hourMinute is12H hours n
      _ -> Nothing
  }
ruleIntegerParaAsHourofdayAsRelativeMinutes2 :: Rule
ruleIntegerParaAsHourofdayAsRelativeMinutes2 = Rule
  { name = "<integer> minutos para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(uto)?s?"
    , regex "para ((o|a|\x00e0)s?)?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:_:Token Time td:_) -> do
        n <- getIntValue token
        t <- minutesBefore n td
        Just $ Token Time t
      _ -> Nothing
  }

ruleHourofdayQuarter :: Rule
ruleHourofdayQuarter = Rule
  { name = "<hour-of-day> quarter (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "quinze"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> Just . Token Time $ hourMinute is12H hours 15
      _ -> Nothing
  }
ruleHourofdayAndQuarter :: Rule
ruleHourofdayAndQuarter = Rule
  { name = "<hour-of-day> and quinze"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e quinze"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> Just . Token Time $ hourMinute is12H hours 15
      _ -> Nothing
  }
ruleQuarterParaAsHourofdayAsRelativeMinutes :: Rule
ruleQuarterParaAsHourofdayAsRelativeMinutes = Rule
  { name = "quinze para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ regex "quinze para ((o|a|\x00e0)s?)?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleHourofdayHalf :: Rule
ruleHourofdayHalf = Rule
  { name = "<hour-of-day> half (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "meia|trinta"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> Just . Token Time $ hourMinute is12H hours 30
      _ -> Nothing
  }
ruleHourofdayAndHalf :: Rule
ruleHourofdayAndHalf = Rule
  { name = "<hour-of-day> and half"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e (meia|trinta)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> Just . Token Time $ hourMinute is12H hours 30
      _ -> Nothing
  }
ruleHalfParaAsHourofdayAsRelativeMinutes :: Rule
ruleHalfParaAsHourofdayAsRelativeMinutes = Rule
  { name = "half para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ regex "(meia|trinta) para ((o|a|\x00e0)s?)?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleHourofdayThreeQuarter :: Rule
ruleHourofdayThreeQuarter = Rule
  { name = "<hour-of-day> 3/4 (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "quarenta e cinco"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> Just . Token Time $ hourMinute is12H hours 45
      _ -> Nothing
  }
ruleHourofdayAndThreeQuarter :: Rule
ruleHourofdayAndThreeQuarter = Rule
  { name = "<hour-of-day> and 3/4"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "e quarenta e cinco"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> Just . Token Time $ hourMinute is12H hours 45
      _ -> Nothing
  }
ruleThreeQuarterParaAsHourofdayAsRelativeMinutes :: Rule
ruleThreeQuarterParaAsHourofdayAsRelativeMinutes = Rule
  { name = "3/4 para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ regex "quarenta e cinco para ((o|a|\x00e0)s?)?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 45 td
      _ -> Nothing
  }

ruleNamedmonth :: Rule
ruleNamedmonth = Rule
  { name = "named-month"
  , pattern =
    [ regex "janeiro|jan\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 1
  }

ruleTiradentes :: Rule
ruleTiradentes = Rule
  { name = "Tiradentes"
  , pattern =
    [ regex "tiradentes"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 4 21
  }

ruleInThePartofday :: Rule
ruleInThePartofday = Rule
  { name = "in the <part-of-day>"
  , pattern =
    [ regex "(de|pela)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time $ notLatent td
      _ -> Nothing
  }

rulePartofdayDessaSemana :: Rule
rulePartofdayDessaSemana = Rule
  { name = "<part-of-day> dessa semana"
  , pattern =
    [ Predicate isNotLatent
    , regex "(d?es[ts]a semana)|agora"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        Just . Token Time . partOfDay $ intersect (cycleNth TG.Day 0, td)
      _ -> Nothing
  }

ruleNamedmonth3 :: Rule
ruleNamedmonth3 = Rule
  { name = "named-month"
  , pattern =
    [ regex "mar(\x00e7|c)o|mar\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 3
  }

ruleDepoisDasTimeofday :: Rule
ruleDepoisDasTimeofday = Rule
  { name = "depois das <time-of-day>"
  , pattern =
    [ regex "(depois|ap(\x00f3|o)s) d?((a|\x00e1|\x00e0)[so]?|os?)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time $ withDirection TTime.After td
      _ -> Nothing
  }

ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd[/-]mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[\\/\\-](0?[1-9]|1[0-2])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        Just . Token Time $ monthDay m d
      _ -> Nothing
  }

ruleEmDuration :: Rule
ruleEmDuration = Rule
  { name = "em <duration>"
  , pattern =
    [ regex "em"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        Just . Token Time $ inDuration dd
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
      in Just . Token Time . mkLatent . partOfDay $ interval False (from, to)
  }

ruleNamedmonth4 :: Rule
ruleNamedmonth4 = Rule
  { name = "named-month"
  , pattern =
    [ regex "abril|abr\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 4
  }

ruleDimTimeDaManha :: Rule
ruleDimTimeDaManha = Rule
  { name = "<dim time> da manha"
  , pattern =
    [ dimension Time
    , regex "(da|na|pela) manh(\x00e3|a)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        let td2 = mkLatent . partOfDay $ interval False (hour False 4, hour False 12)
        in Just . Token Time $ intersect (td, td2)
      _ -> Nothing
  }

ruleNCycleProximoqueVem :: Rule
ruleNCycleProximoqueVem = Rule
  { name = "n <cycle> (proximo|que vem)"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(pr(\x00f3|o)xim(o|a)s?|que vem?|seguintes?)"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        Just . Token Time $ cycleN True grain v
      _ -> Nothing
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "midnight"
  , pattern =
    [ regex "meia[\\s\\-]?noite"
    ]
  , prod = \_ -> Just . Token Time $ hour False 0
  }

ruleNamedday5 :: Rule
ruleNamedday5 = Rule
  { name = "named-day"
  , pattern =
    [ regex "sexta((\\s|\\-)feira)?|sex\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 5
  }

ruleDdddMonthinterval :: Rule
ruleDdddMonthinterval = Rule
  { name = "dd-dd <month>(interval)"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-|a"
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
        let from = intersect (dayOfMonth d1, td)
            to = intersect (dayOfMonth d2, td)
        Just . Token Time $ interval True (from, to)
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
        Just . Token Time . mkLatent $ hour True v
      _ -> Nothing
  }

ruleUltimoTime :: Rule
ruleUltimoTime = Rule
  { name = "ultimo <time>"
  , pattern =
    [ regex "(u|\x00fa)ltimo"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time $ predNth (-1) False td
      _ -> Nothing
  }

ruleNamedmonth2 :: Rule
ruleNamedmonth2 = Rule
  { name = "named-month"
  , pattern =
    [ regex "fevereiro|fev\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 2
  }

ruleNamedmonthnameddayPast :: Rule
ruleNamedmonthnameddayPast = Rule
  { name = "<named-month|named-day> past"
  , pattern =
    [ Predicate isNotLatent
    , regex "(da semana)? passad(o|a)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        Just . Token Time $ predNth (-1) False td
      _ -> Nothing
  }

ruleSeason3 :: Rule
ruleSeason3 = Rule
  { name = "season"
  , pattern =
    [ regex "inverno"
    ]
  , prod = \_ ->
      let from = monthDay 12 21
          to = monthDay 3 20
      in Just . Token Time $ interval False (from, to)
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "season"
  , pattern =
    [ regex "ver(\x00e3|a)o"
    ]
  , prod = \_ ->
      let from = monthDay 6 21
          to = monthDay 9 23
      in Just . Token Time $ interval False (from, to)
  }

ruleRightNow :: Rule
ruleRightNow = Rule
  { name = "right now"
  , pattern =
    [ regex "agora|j(\x00e1|a)|(nesse|neste) instante"
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Second 0
  }

ruleFazemDuration :: Rule
ruleFazemDuration = Rule
  { name = "fazem <duration>"
  , pattern =
    [ regex "faz(em)?"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        Just . Token Time $ durationAgo dd
      _ -> Nothing
  }

ruleAmanhPelaPartofday :: Rule
ruleAmanhPelaPartofday = Rule
  { name = "amanhã pela <part-of-day>"
  , pattern =
    [ dimension Time
    , regex "(da|na|pela|a)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Just . Token Time $ intersect (td1, td2)
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
        Just . Token Time . mkLatent $ hourMinute False h m
      _ -> Nothing
  }

ruleNamedmonthnameddayNext :: Rule
ruleNamedmonthnameddayNext = Rule
  { name = "<named-month|named-day> next"
  , pattern =
    [ dimension Time
    , regex "(da pr(o|\x00f3)xima semana)|(da semana)? que vem"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        Just . Token Time $ predNth 1 False td
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
        Just . Token Time $ intersect (td1, td2)
      _ -> Nothing
  }

ruleIntersect2 :: Rule
ruleIntersect2 = Rule
  { name = "intersect2"
  , pattern =
    [ Predicate isNotLatent
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Just . Token Time $ intersect (td2, td1)
      _ -> Nothing
  }

ruleTimeofdayPartofday :: Rule
ruleTimeofdayPartofday = Rule
  { name = "<time-of-day> <part-of-day>"
  , pattern =
    [ dimension Time
    , regex "(da|na|pela)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Just . Token Time $ intersect (td1, td2)
      _ -> Nothing
  }

ruleDeDatetimeDatetimeInterval :: Rule
ruleDeDatetimeDatetimeInterval = Rule
  { name = "de <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "de?"
    , dimension Time
    , regex "\\-|a"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Just . Token Time $ interval False (td1, td2)
      _ -> Nothing
  }

ruleNamedmonth6 :: Rule
ruleNamedmonth6 = Rule
  { name = "named-month"
  , pattern =
    [ regex "junho|jun\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 6
  }

ruleDentroDeDuration :: Rule
ruleDentroDeDuration = Rule
  { name = "dentro de <duration>"
  , pattern =
    [ regex "(dentro de)|em"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        let from = cycleNth TG.Second 0
            to = inDuration dd
        in Just . Token Time $ interval False (from, to)
      _ -> Nothing
  }

ruleSTimeofday :: Rule
ruleSTimeofday = Rule
  { name = "às <time-of-day>"
  , pattern =
    [ regex "(\x00e0|a)s?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time $ notLatent td
      _ -> Nothing
  }

ruleNamedmonth8 :: Rule
ruleNamedmonth8 = Rule
  { name = "named-month"
  , pattern =
    [ regex "agosto|ago\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 8
  }

ruleDimTimeDaTarde :: Rule
ruleDimTimeDaTarde = Rule
  { name = "<dim time> da tarde"
  , pattern =
    [ dimension Time
    , regex "(da|na|pela) tarde"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        let td2 = mkLatent . partOfDay $ interval False (hour False 12, hour False 18)
        in Just . Token Time $ intersect (td, td2)
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "final de semana|fim de semana|fds"
    ]
  , prod = \_ ->
      let from = intersect (dayOfWeek 5, hour False 18)
          to = intersect (dayOfWeek 1, hour False 0)
      in Just . Token Time $ interval False (from, to)
  }

ruleDayofweekSHourmin :: Rule
ruleDayofweekSHourmin = Rule
  { name = "<day-of-week> às <hour-min>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(\x00e0|a)s"
    , Predicate isNotLatent
    , regex "da|pela"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_:Token Time td3:_) ->
        let td = intersect (td1, td2)
        in Just . Token Time $ intersect (td, td3)
      _ -> Nothing
  }

ruleCycleQueVem :: Rule
ruleCycleQueVem = Rule
  { name = "<cycle> (que vem)"
  , pattern =
    [ dimension TimeGrain
    , regex "que vem|seguintes?"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> Just . Token Time $ cycleNth grain 1
      _ -> Nothing
  }

ruleAnoNovo :: Rule
ruleAnoNovo = Rule
  { name = "ano novo"
  , pattern =
    [ regex "ano novo|reveillon"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 1 1
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "(d[ao]) pr(\x00f3|o)xim[ao]s?|que vem"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time $ predNth 0 True td
      _ -> Nothing
  }

ruleDeYear :: Rule
ruleDeYear = Rule
  { name = "de <year>"
  , pattern =
    [ regex "de|do ano"
    , Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        Just . Token Time $ year n
      _ -> Nothing
  }

ruleVesperaDeAnoNovo :: Rule
ruleVesperaDeAnoNovo = Rule
  { name = "vespera de ano novo"
  , pattern =
    [ regex "v(\x00e9|e)spera d[eo] ano[\\s\\-]novo"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 12 31
  }

ruleNPassadosCycle :: Rule
ruleNPassadosCycle = Rule
  { name = "n passados <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "passad(a|o)s?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        Just . Token Time $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleDiaDayofmonthNonOrdinal :: Rule
ruleDiaDayofmonthNonOrdinal = Rule
  { name = "dia <day-of-month> (non ordinal)"
  , pattern =
    [ regex "dia"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        Just . Token Time . mkLatent $ dayOfMonth v
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
        Just . Token Time $ yearMonthDay y m d
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
        Just . Token Time $ notLatent td
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
        Just . Token Time $ intersect (td1, td2)
      _ -> Nothing
  }

ruleTwoTimeTokensSeparatedBy2 :: Rule
ruleTwoTimeTokensSeparatedBy2 = Rule
  { name = "two time tokens separated by \",\"2"
  , pattern =
    [ Predicate isNotLatent
    , regex ","
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Just . Token Time $ intersect (td2, td1)
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "manh(\x00e3|a)"
    ]
  , prod = \_ ->
      let from = hour False 4
          to = hour False 12
      in Just . Token Time . mkLatent .partOfDay $ interval False (from, to)
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "es[ts]a"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time . partOfDay $ intersect (cycleNth TG.Day 0, td)
      _ -> Nothing
  }

ruleThisTime :: Rule
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "es[ts][ae]"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time $ predNth 0 False td
      _ -> Nothing
  }

ruleNaoNamedday :: Rule
ruleNaoNamedday = Rule
  { name = "n[ao] named-day"
  , pattern =
    [ regex "n[ao]"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleProclamaoDaRepblica :: Rule
ruleProclamaoDaRepblica = Rule
  { name = "Proclamação da República"
  , pattern =
    [ regex "proclama(c|\x00e7)(a|\x00e3)o da rep(\x00fa|u)blica"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 11 15
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
        Just . Token Time . mkLatent $ year n
      _ -> Nothing
  }

ruleYesterday :: Rule
ruleYesterday = Rule
  { name = "yesterday"
  , pattern =
    [ regex "ontem"
    ]
  , prod = \_ -> Just . Token Time . cycleNth TG.Day $ - 1
  }

ruleSeason2 :: Rule
ruleSeason2 = Rule
  { name = "season"
  , pattern =
    [ regex "outono"
    ]
  , prod = \_ ->
      let from = monthDay 9 23
          to = monthDay 12 21
      in Just . Token Time $ interval False (from, to)
  }

ruleDiaDoTrabalhador :: Rule
ruleDiaDoTrabalhador = Rule
  { name = "Dia do trabalhador"
  , pattern =
    [ regex "dia do trabalh(o|ador)"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 5 1
  }

ruleNCycleAtras :: Rule
ruleNCycleAtras = Rule
  { name = "n <cycle> atras"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "atr(a|\x00e1)s"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        Just . Token Time $ cycleN True grain (- v)
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
        Just . Token Time . timeOfDayAMPM td $ Text.toLower ap == "a"
      _ -> Nothing
  }

ruleDayofmonthDeNamedmonth :: Rule
ruleDayofmonthDeNamedmonth = Rule
  { name = "<day-of-month> de <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , regex "de|\\/"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleEntreDatetimeEDatetimeInterval :: Rule
ruleEntreDatetimeEDatetimeInterval = Rule
  { name = "entre <datetime> e <datetime> (interval)"
  , pattern =
    [ regex "entre"
    , dimension Time
    , regex "e"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Just . Token Time $ interval False (td1, td2)
      _ -> Nothing
  }

ruleEntreDdEtDdMonthinterval :: Rule
ruleEntreDdEtDdMonthinterval = Rule
  { name = "entre dd et dd <month>(interval)"
  , pattern =
    [ regex "entre"
    , regex "(0?[1-9]|[12]\\d|3[01])"
    , regex "e?"
    , regex "(0?[1-9]|[12]\\d|3[01])"
    , regex "de"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token RegexMatch (GroupMatch (d1:_)):
       _:
       Token RegexMatch (GroupMatch (d2:_)):
       _:
       Token Time td:
       _) -> do
        dd1 <- parseInt d1
        dd2 <- parseInt d2
        let dom1 = intersect (dayOfMonth dd1, td)
            dom2 = intersect (dayOfMonth dd2, td)
         in Just . Token Time $ interval True (dom1, dom2)
      _ -> Nothing
  }

ruleCyclePassado :: Rule
ruleCyclePassado = Rule
  { name = "<cycle> passado"
  , pattern =
    [ dimension TimeGrain
    , regex "passad(a|o)s?"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) ->
        Just . Token Time . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleNamedmonth5 :: Rule
ruleNamedmonth5 = Rule
  { name = "named-month"
  , pattern =
    [ regex "maio|mai\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 5
  }

ruleNamedday7 :: Rule
ruleNamedday7 = Rule
  { name = "named-day"
  , pattern =
    [ regex "domingo|dom\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 7
  }

ruleNossaSenhoraAparecida :: Rule
ruleNossaSenhoraAparecida = Rule
  { name = "Nossa Senhora Aparecida"
  , pattern =
    [ regex "nossa senhora (aparecida)?"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 10 12
  }

ruleFinados :: Rule
ruleFinados = Rule
  { name = "Finados"
  , pattern =
    [ regex "finados|dia dos mortos"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 11 2
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
        Just . Token Time $ year n
      _ -> Nothing
  }

ruleNamedmonth10 :: Rule
ruleNamedmonth10 = Rule
  { name = "named-month"
  , pattern =
    [ regex "outubro|out\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 10
  }

ruleAntesDasTimeofday :: Rule
ruleAntesDasTimeofday = Rule
  { name = "antes das <time-of-day>"
  , pattern =
    [ regex "(antes|at(e|\x00e9)|n(a|\x00e3)o mais que) (d?(o|a|\x00e0)s?)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        Just . Token Time $ withDirection TTime.Before td
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
        Just . Token Time $ cycleN True grain v
      _ -> Nothing
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd[/-.]mm[/-.]yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[\\.\\/\\-](0?[1-9]|1[0-2])[\\.\\/\\-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m3
        m <- parseInt m2
        d <- parseInt m1
        Just . Token Time $ yearMonthDay y m d
      _ -> Nothing
  }

ruleNamedmonth11 :: Rule
ruleNamedmonth11 = Rule
  { name = "named-month"
  , pattern =
    [ regex "novembro|nov\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 11
  }

ruleIndependecia :: Rule
ruleIndependecia = Rule
  { name = "Independecia"
  , pattern =
    [ regex "independ(\x00ea|e)ncia"
    ]
  , prod = \_ -> Just . Token Time $ monthDay 9 7
  }

ruleNamedday3 :: Rule
ruleNamedday3 = Rule
  { name = "named-day"
  , pattern =
    [ regex "quarta((\\s|\\-)feira)?|qua\\.?"
    ]
  , prod = \_ -> Just . Token Time $ dayOfWeek 3
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "amanh(\x00e3|a)"
    ]
  , prod = \_ -> Just . Token Time $ cycleNth TG.Day 1
  }

ruleNamedmonth9 :: Rule
ruleNamedmonth9 = Rule
  { name = "named-month"
  , pattern =
    [ regex "setembro|set\\.?"
    ]
  , prod = \_ -> Just . Token Time $ month 9
  }

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate isATimeOfDay
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
  [ ruleAfternoon
  , ruleAmanhPelaPartofday
  , ruleAnoNovo
  , ruleAntesDasTimeofday
  , ruleDatetimeDatetimeInterval
  , ruleDayOfMonthSt
  , ruleDayofmonthDeNamedmonth
  , ruleDayofweekSHourmin
  , ruleDdddMonthinterval
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleDeDatetimeDatetimeInterval
  , ruleDeYear
  , ruleDentroDeDuration
  , ruleDepoisDasTimeofday
  , ruleDiaDayofmonthDeNamedmonth
  , ruleDiaDayofmonthNonOrdinal
  , ruleDiaDoTrabalhador
  , ruleDimTimeDaMadrugada
  , ruleDimTimeDaManha
  , ruleDimTimeDaTarde
  , ruleEmDuration
  , ruleEntreDatetimeEDatetimeInterval
  , ruleEntreDdEtDdMonthinterval
  , ruleEsteCycle
  , ruleEvening
  , ruleFazemDuration
  , ruleFinados
  , ruleHhhmmTimeofday
  , ruleHhmmMilitaryTimeofday
  , ruleHourofdayAndRelativeMinutes
  , ruleHourofdayAndRelativeMinutes2
  , ruleHourofdayAndQuarter
  , ruleHourofdayAndThreeQuarter
  , ruleHourofdayAndHalf
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleHourofdayIntegerAsRelativeMinutes2
  , ruleHourofdayQuarter
  , ruleHourofdayHalf
  , ruleHourofdayThreeQuarter
  , ruleInThePartofday
  , ruleIndependecia
  , ruleIntegerParaAsHourofdayAsRelativeMinutes
  , ruleIntegerParaAsHourofdayAsRelativeMinutes2
  , ruleHalfParaAsHourofdayAsRelativeMinutes
  , ruleQuarterParaAsHourofdayAsRelativeMinutes
  , ruleThreeQuarterParaAsHourofdayAsRelativeMinutes
  , ruleIntersect
  , ruleIntersect2
  , ruleIntersectByDaOrDe
  , ruleLastTime
  , ruleMidnight
  , ruleMorning
  , ruleNCycleAtras
  , ruleNCycleProximoqueVem
  , ruleNPassadosCycle
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
  , ruleNamedmonthnameddayNext
  , ruleNamedmonthnameddayPast
  , ruleNaoDate
  , ruleNaoNamedday
  , ruleNatal
  , ruleNextTime
  , ruleCycleQueVem
  , ruleProximoCycle
  , ruleNoon
  , ruleNossaSenhoraAparecida
  , ruleNow
  , ruleCycleAntesDeTime
  , ruleCyclePassado
  , rulePartofdayDessaSemana
  , rulePassadosNCycle
  , ruleProclamaoDaRepblica
  , ruleProximasNCycle
  , ruleRightNow
  , ruleSHourminTimeofday
  , ruleSHourmintimeofday
  , ruleSTimeofday
  , ruleSeason
  , ruleSeason2
  , ruleSeason3
  , ruleSeason4
  , ruleTheDayAfterTomorrow
  , ruleTheDayBeforeYesterday
  , ruleThisPartofday
  , ruleThisTime
  , ruleThisnextDayofweek
  , ruleTimeofdayAmpm
  , ruleTimeofdayHoras
  , ruleTimeofdayLatent
  , ruleTimeofdayPartofday
  , ruleTiradentes
  , ruleTomorrow
  , ruleTwoTimeTokensSeparatedBy
  , ruleTwoTimeTokensSeparatedBy2
  , ruleUltimoTime
  , ruleVesperaDeAnoNovo
  , ruleWeekend
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYesterday
  , ruleYyyymmdd
  , ruleTimezone
  ]
