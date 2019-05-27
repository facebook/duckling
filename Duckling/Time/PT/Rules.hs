-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.PT.Rules
  ( rules
  ) where

import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import Duckling.Ordinal.Types (OrdinalData (..))
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG
import qualified Duckling.Ordinal.Types as TOrdinal

ruleSHourmintimeofday :: Rule
ruleSHourmintimeofday = Rule
  { name = "às <hour-min>(time-of-day)"
  , pattern =
    [ regex "(à|a)s?"
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
    [ regex "depois de amanh(ã|a)"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleNatal :: Rule
ruleNatal = Rule
  { name = "natal"
  , pattern =
    [ regex "natal"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleNaoDate :: Rule
ruleNaoDate = Rule
  { name = "n[ao] <date>"
  , pattern =
    [ regex "n[ao]"
    , Predicate $ isGrainOfTime TG.Day
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleIntersectByDaOrDe :: Rule
ruleIntersectByDaOrDe = Rule
  { name = "intersect by `da` or `de`"
  , pattern =
    [ dimension Time
    , regex "d[ae]"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

rulePassadosNCycle :: Rule
rulePassadosNCycle = Rule
  { name = "passados n <cycle>"
  , pattern =
    [ regex "(passad|[úu]ltim)[ao]s?"
    , Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (-v)
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "last <time>"
  , pattern =
    [ regex "[úu]ltim[ao]s?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|até( ao?)?|ao?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDeDatetimeDatetimeInterval :: Rule
ruleDeDatetimeDatetimeInterval = Rule
  { name = "de <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "de?"
    , Predicate isATimeOfDay
    , regex "\\-|até( ao?)?|ao?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
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
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleDayOfMonthSt :: Rule
ruleDayOfMonthSt = Rule
  { name = "day of month (1st)"
  , pattern =
    [ regex "primeiro|um|1o"
    ]
  , prod = \_ -> tt . mkLatent $ dayOfMonth 1
  }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "(hoje)|(neste|nesse) momento"
    ]
  , prod = \_ -> tt today
  }

ruleDimTimeDaMadrugada :: Rule
ruleDimTimeDaMadrugada = Rule
  { name = "<dim time> da madrugada"
  , pattern =
    [ dimension Time
    , regex "(da|na|pela) madruga(da)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        td2 <- mkLatent . partOfDay <$>
          interval TTime.Open (hour False 1) (hour False 4)
        Token Time <$> intersect td td2
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

ruleProximoCycle :: Rule
ruleProximoCycle = Rule
  { name = "proximo <cycle> "
  , pattern =
    [ regex "pr(ó|o)xim(o|a)s?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
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
        tt $ cycleNthAfter False grain (-1) td
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
      (_:Token TimeGrain grain:_) -> tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleCycleActual :: Rule
ruleCycleActual = Rule
  { name = "<cycle> actual"
  , pattern =
    [ dimension TimeGrain
    , regex "ac?tual"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleSHourminTimeofday :: Rule
ruleSHourminTimeofday = Rule
  { name = "às <hour-min> <time-of-day>"
  , pattern =
    [ regex "[àa]s"
    , Predicate isNotLatent
    , regex "da"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) -> Token Time <$> intersect td1 td2
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
        n <- getIntValue token
        tt . mkLatent $ year n
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

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "es[ts][ae]|pr(ó|o)xim[ao]"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleTheDayBeforeYesterday :: Rule
ruleTheDayBeforeYesterday = Rule
  { name = "the day before yesterday"
  , pattern =
    [ regex "anteontem|antes de ontem"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
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
ruleIntegerParaAsHourofdayAsRelativeMinutes :: Rule
ruleIntegerParaAsHourofdayAsRelativeMinutes = Rule
  { name = "<integer> para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "para ((o|a|à)s?)?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }

ruleHourofdayIntegerAsRelativeMinutes2 :: Rule
ruleHourofdayIntegerAsRelativeMinutes2 = Rule
  { name = "<hour-of-day> <integer> (as relative minutes) minutos"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
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
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }
ruleIntegerParaAsHourofdayAsRelativeMinutes2 :: Rule
ruleIntegerParaAsHourofdayAsRelativeMinutes2 = Rule
  { name = "<integer> minutos para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(uto)?s?"
    , regex "para ((o|a|à)s?)?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
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
       _) -> tt $ hourMinute is12H hours 15
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
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }
ruleQuarterParaAsHourofdayAsRelativeMinutes :: Rule
ruleQuarterParaAsHourofdayAsRelativeMinutes = Rule
  { name = "quinze para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ regex "quinze para ((o|a|à)s?)?"
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
       _) -> tt $ hourMinute is12H hours 30
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
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }
ruleHalfParaAsHourofdayAsRelativeMinutes :: Rule
ruleHalfParaAsHourofdayAsRelativeMinutes = Rule
  { name = "half para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ regex "(meia|trinta) para ((o|a|à)s?)?"
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
       _) -> tt $ hourMinute is12H hours 45
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
       _) -> tt $ hourMinute is12H hours 45
      _ -> Nothing
  }
ruleThreeQuarterParaAsHourofdayAsRelativeMinutes :: Rule
ruleThreeQuarterParaAsHourofdayAsRelativeMinutes = Rule
  { name = "3/4 para as <hour-of-day> (as relative minutes)"
  , pattern =
    [ regex "quarenta e cinco para ((o|a|à)s?)?"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 45 td
      _ -> Nothing
  }

ruleTiradentes :: Rule
ruleTiradentes = Rule
  { name = "Tiradentes"
  , pattern =
    [ regex "tiradentes"
    ]
  , prod = \_ -> tt $ monthDay 4 21
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
        tt $ notLatent td
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
      (Token Time td:_) -> Token Time . partOfDay <$> intersect today td
      _ -> Nothing
  }

ruleDepoisDasTimeofday :: Rule
ruleDepoisDasTimeofday = Rule
  { name = "depois das <time-of-day>"
  , pattern =
    [ regex "(depois|ap(ó|o)s) d?((a|á|à)[so]?|os?)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ withDirection TTime.After td
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
        tt $ monthDay m d
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
        tt $ inDuration dd
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

ruleDimTimeDaManha :: Rule
ruleDimTimeDaManha = Rule
  { name = "<dim time> da manha"
  , pattern =
    [ Predicate $ isGrainFinerThan TG.Year
    , regex "(da|na|pela) manh(ã|a)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        td2 <- mkLatent . partOfDay <$>
          interval TTime.Open (hour False 4) (hour False 12)
        Token Time <$> intersect td td2
      _ -> Nothing
  }

ruleNCycleProximoqueVem :: Rule
ruleNCycleProximoqueVem = Rule
  { name = "n <cycle> (proximo|que vem)"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(pr(ó|o)xim(o|a)s?|que vem?|seguintes?)"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "midnight"
  , pattern =
    [ regex "meia[\\s\\-]?noite"
    ]
  , prod = \_ -> tt $ hour False 0
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

ruleUltimoTime :: Rule
ruleUltimoTime = Rule
  { name = "ultimo <time>"
  , pattern =
    [ regex "(u|ú)ltimo"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
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
        tt $ predNth (-1) False td
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
      in Token Time <$> interval TTime.Open from to
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "season"
  , pattern =
    [ regex "ver(ã|a)o"
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
    [ regex "agora|j(á|a)|(nesse|neste) instante"
    ]
  , prod = \_ -> tt now
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
        tt $ durationAgo dd
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
        Token Time <$> intersect td1 td2
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

ruleNamedmonthnameddayNext :: Rule
ruleNamedmonthnameddayNext = Rule
  { name = "<named-month|named-day> next"
  , pattern =
    [ dimension Time
    , regex "(da pr(o|ó)xima semana)|(da semana)? que vem"
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
    , regex "(da|na|pela)"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
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
        Token Time <$> interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }

ruleSTimeofday :: Rule
ruleSTimeofday = Rule
  { name = "às <time-of-day>"
  , pattern =
    [ regex "(à|a)s?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleDimTimeDaTarde :: Rule
ruleDimTimeDaTarde = Rule
  { name = "<dim time> da tarde"
  , pattern =
    [ dimension Time
    , regex "(da|na|pela) tarde"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        td2 <- mkLatent . partOfDay <$>
          interval TTime.Open (hour False 12) (hour False 18)
        Token Time <$> intersect td td2
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "final de semana|fim de semana|fds"
    ]
  , prod = \_ -> tt weekend
  }

ruleDayofweekSHourmin :: Rule
ruleDayofweekSHourmin = Rule
  { name = "<day-of-week> às <hour-min>"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(à|a)s"
    , Predicate isNotLatent
    , regex "da|pela"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_:Token Time td3:_) -> do
        td <- intersect td1 td2
        Token Time <$> intersect td td3
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
      (Token TimeGrain grain:_) -> tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleAnoNovo :: Rule
ruleAnoNovo = Rule
  { name = "ano novo"
  , pattern =
    [ regex "ano novo|reveillon"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "(d[ao]) pr(ó|o)xim[ao]s?|que vem"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
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
        tt $ year n
      _ -> Nothing
  }

ruleVesperaDeAnoNovo :: Rule
ruleVesperaDeAnoNovo = Rule
  { name = "vespera de ano novo"
  , pattern =
    [ regex "v(é|e)spera d[eo] ano[\\s\\-]novo"
    ]
  , prod = \_ -> tt $ monthDay 12 31
  }

ruleNPassadosCycle :: Rule
ruleNPassadosCycle = Rule
  { name = "n passados <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "(passad|[úu]ltim)[ao]s?|anterior(es)?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleNCyclePassados :: Rule
ruleNCyclePassados = Rule
  { name = "n <cycle> passados"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(passad|[úu]ltim)[ao]s?|anterior(es)?"
    ]
  , prod = \tokens -> case tokens of
      (token: Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
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
        tt . mkLatent $ dayOfMonth v
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
        Token Time <$> intersect td2 td1
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "manh(ã|a)"
    ]
  , prod = \_ ->
      let from = hour False 4
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "es[ts]a"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay <$> intersect today td
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
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleProclamaoDaRepblica :: Rule
ruleProclamaoDaRepblica = Rule
  { name = "Proclamação da República"
  , pattern =
    [ regex "proclama(c|ç)(a|ã)o da rep(ú|u)blica"
    ]
  , prod = \_ -> tt $ monthDay 11 15
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
    [ regex "ontem"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
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
      in Token Time <$> interval TTime.Open from to
  }

ruleDiaDoTrabalhador :: Rule
ruleDiaDoTrabalhador = Rule
  { name = "Dia do trabalhador"
  , pattern =
    [ regex "dia do trabalh(o|ador)"
    ]
  , prod = \_ -> tt $ monthDay 5 1
  }

ruleNCycleAtras :: Rule
ruleNCycleAtras = Rule
  { name = "n <cycle> atras"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "atr(a|á)s"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
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
    [ regex "entre( [ao])?|desde|(a partir )?d[eo]"
    , dimension Time
    , regex "e|\\-|até( ao?)?|ao?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
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
        dom1 <- intersect (dayOfMonth dd1) td
        dom2 <- intersect (dayOfMonth dd2) td
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleCyclePassado :: Rule
ruleCyclePassado = Rule
  { name = "<cycle> passado"
  , pattern =
    [ dimension TimeGrain
    , regex "passad[ao]s?|anterior(es)?"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> tt . cycleNth grain $ - 1
      _ -> Nothing
  }

rulePassadoCycle :: Rule
rulePassadoCycle = Rule
  { name = "passado <cycle>"
  , pattern =
    [ regex "(passad|[úu]ltim)[ao]s?|anterior(es)?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleNossaSenhoraAparecida :: Rule
ruleNossaSenhoraAparecida = Rule
  { name = "Nossa Senhora Aparecida"
  , pattern =
    [ regex "nossa senhora( aparecida)?"
    ]
  , prod = \_ -> tt $ monthDay 10 12
  }

ruleFinados :: Rule
ruleFinados = Rule
  { name = "Finados"
  , pattern =
    [ regex "finados|dia dos mortos"
    ]
  , prod = \_ -> tt $ monthDay 11 2
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
        tt $ year n
      _ -> Nothing
  }

ruleAntesDasTimeofday :: Rule
ruleAntesDasTimeofday = Rule
  { name = "antes das <time-of-day>"
  , pattern =
    [ regex "(antes|at(e|é)|n(a|ã)o mais que) (d?(o|a|à)s?)?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ withDirection TTime.Before td
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
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleIndependecia :: Rule
ruleIndependecia = Rule
  { name = "Independecia"
  , pattern =
    [ regex "independ(ê|e)ncia"
    ]
  , prod = \_ -> tt $ monthDay 9 7
  }

ruleTomorrow :: Rule
ruleTomorrow = Rule
  { name = "tomorrow"
  , pattern =
    [ regex "amanh(ã|a)"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
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
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleCycleOrdinalOfTime :: Rule
ruleCycleOrdinalOfTime = Rule
    { name = "<ordinal> <cycle> de <time>"
    , pattern =
      [ dimension Ordinal
      , dimension TimeGrain
      , regex "d[eo]|em"
      , dimension Time
      ]
    , prod = \tokens -> case tokens of
        (token:Token TimeGrain grain:_:Token Time td:_) -> do
          n <- getIntValue token
          tt $ cycleNthAfter True grain (n - 1) td
        _ -> Nothing
    }

ruleCycleOrdinalTime :: Rule
ruleCycleOrdinalTime = Rule
    { name = "<ordinal> <cycle> <time>"
    , pattern =
      [ dimension Ordinal
      , dimension TimeGrain
      , dimension Time
      ]
    , prod = \tokens -> case tokens of
        (token:Token TimeGrain grain: Token Time td:_) -> do
          n <- getIntValue token
          tt $ cycleNthAfter True grain (n - 1) td
        _ -> Nothing
    }

ruleCycleTheOrdinalOfTime :: Rule
ruleCycleTheOrdinalOfTime = Rule
    { name = "o <ordinal> <cycle> de <time>"
    , pattern =
      [ regex "o|a"
      , dimension Ordinal
      , dimension TimeGrain
      , regex "d[eo]|em"
      , dimension Time
      ]
    , prod = \tokens -> case tokens of
        (_:token:Token TimeGrain grain:_:Token Time td:_) -> do
          n <- getIntValue token
          tt $ cycleNthAfter True grain (n - 1) td
        _ -> Nothing
    }

ruleOrdinalTrimestre :: Rule
ruleOrdinalTrimestre = Rule
  { name = "<ordinal> trimestre"
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

ruleTheOrdinalTrimestre :: Rule
ruleTheOrdinalTrimestre = Rule
  { name = "o <ordinal> trimestre"
  , pattern =
    [ regex "o|a"
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

ruleOrdinalTrimestreYear :: Rule
ruleOrdinalTrimestreYear = Rule
  { name = "<ordinal> trimestre <year>"
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

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
    { name = "último <cycle> de <time>"
    , pattern =
      [ regex "[úu]ltim[ao]s?"
      , dimension TimeGrain
      , regex "d[eo]|em"
      , dimension Time
      ]
    , prod = \tokens -> case tokens of
        (_:Token TimeGrain grain:_:Token Time td:_) ->
          tt $ cycleLastOf grain td
        _ -> Nothing
    }

ruleLastCycleTime :: Rule
ruleLastCycleTime = Rule
    { name = "último <cycle> <time>"
    , pattern =
      [ regex "[úu]ltim[ao]s?"
      , dimension TimeGrain
      , dimension Time
      ]
    , prod = \tokens -> case tokens of
        (_:Token TimeGrain grain: Token Time td:_) ->
          tt $ cycleLastOf grain td
        _ -> Nothing
    }

ruleIntervalFromMonthDDDDOf :: Rule
ruleIntervalFromMonthDDDDOf = Rule
  { name = "desde <month> dd-dd de (interval)"
  , pattern =
    [ regex "desde|a partir d[eo]"
    , Predicate isDOMValue
    , regex "até( ao?)?|ao?"
    , Predicate isDOMValue
    , regex "d[eo]|em"
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

ruleIntervalDDDDMonthOf :: Rule
ruleIntervalDDDDMonthOf = Rule
    { name = "dd-dd <month> de (interval)"
    , pattern =
      [ Predicate isDOMValue
      , regex "\\-|a(té)?"
      , Predicate isDOMValue
      , regex "d[eo]|em"
      , Predicate isAMonth
      ]
    , prod = \tokens -> case tokens of
        (token1:
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

ruleIntervalFromMonthDDDD :: Rule
ruleIntervalFromMonthDDDD = Rule
  { name = "desde <month> dd-dd (interval)"
  , pattern =
    [ regex "desde|a partir d[eo]"
    , Predicate isDOMValue
    , regex "até( ao?)?|ao?"
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

ruleIntervalDDDDMonth :: Rule
ruleIntervalDDDDMonth = Rule
    { name = "dd-dd <month> (interval)"
    , pattern =
      [ Predicate isDOMValue
      , regex "\\-|a(té)?"
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

ruleOrdinalCycleOfYearOrdinalCycleOfYear :: Rule
ruleOrdinalCycleOfYearOrdinalCycleOfYear = Rule
    { name = "<ordinal> <cycle> de <year> - <ordinal> <cycle> de <year>"
    , pattern =
      [ dimension Ordinal
      , dimension TimeGrain
      , regex "d[eo]|em"
      , dimension Time
      , regex "\\-|até( ao?)?|ao?"
      , dimension Ordinal
      , dimension TimeGrain
      , regex "d[eo]|em"
      , dimension Time
      ]
    , prod = \tokens -> case tokens of
        (token1:
         Token TimeGrain grain1:
         _:
         Token Time td1:
         _:
         token2:
         Token TimeGrain grain2:
         _:
         Token Time td2:
         _) -> do
          n1 <- getIntValue token1
          n2 <- getIntValue token2
          let from = cycleNthAfter False grain1 (n1 - 1) td1
              to = cycleNthAfter False grain2 (n2 - 1) td2
          Token Time <$> interval TTime.Closed from to
        _ -> Nothing
    }

ruleOrdinalCycleYearOrdinalCycleYear :: Rule
ruleOrdinalCycleYearOrdinalCycleYear = Rule
    { name = "<ordinal> <cycle> <year> - <ordinal> <cycle> <year>"
    , pattern =
      [ dimension Ordinal
      , dimension TimeGrain
      , dimension Time
      , regex "\\-|até( ao?)?|ao?"
      , dimension Ordinal
      , dimension TimeGrain
      , dimension Time
      ]
    , prod = \tokens -> case tokens of
        (token1:
         Token TimeGrain grain1:
         Token Time td1:
         _:
         token2:
         Token TimeGrain grain2:
         Token Time td2:
         _) -> do
          n1 <- getIntValue token1
          n2 <- getIntValue token2
          let from = cycleNthAfter False grain1 (n1 - 1) td1
              to = cycleNthAfter  False grain2 (n2 - 1) td2
          Token Time <$> interval TTime.Closed from to
        _ -> Nothing
    }

daysOfWeek :: [(Text, String)]
daysOfWeek =
  [ ( "Segunda-feira", "segunda((\\s|\\-)feira)?|seg\\.?" )
  , ( "Terça-feira", "ter(ç|c)a((\\s|\\-)feira)?|ter\\."  )
  , ( "Quart-feira", "quarta((\\s|\\-)feira)?|qua\\.?"    )
  , ( "Quinta-feira", "quinta((\\s|\\-)feira)?|qui\\.?"   )
  , ( "Sexta-feira", "sexta((\\s|\\-)feira)?|sex\\.?"     )
  , ( "Sábada", "s(á|a)bado|s(á|a)b\\.?"                  )
  , ( "Domingo", "domingo|dom\\.?"                        )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek daysOfWeek

months :: [(Text, String)]
months =
  [ ( "Janeiro"   , "janeiro|jan\\.?"   )
  , ( "Fevereiro" , "fevereiro|fev\\.?" )
  , ( "Março"     , "mar(ç|c)o|mar\\.?" )
  , ( "Abril"     , "abril|abr\\.?"     )
  , ( "Maio"      , "maio|mai\\.?"      )
  , ( "Junho"     , "junho|jun\\.?"     )
  , ( "Julho"     , "julho|jul\\.?"     )
  , ( "Agosto"    , "agosto|ago\\.?"    )
  , ( "Setembro"  , "setembro|set\\.?"  )
  , ( "Outubro"   , "outubro|out\\.?"   )
  , ( "Novembro"  , "novembro|nov\\.?"  )
  , ( "Dezembro"  , "dezembro|dez\\.?"  )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths months

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
  , ruleCycleActual
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
  , ruleIntersectByDaOrDe
  , ruleLastTime
  , ruleMidnight
  , ruleMorning
  , ruleNCycleAtras
  , ruleNCycleProximoqueVem
  , ruleNPassadosCycle
  , ruleNProximasCycle
  , ruleNamedmonthnameddayNext
  , ruleNamedmonthnameddayPast
  , ruleNaoDate
  , ruleNatal
  , ruleNextTime
  , ruleCycleQueVem
  , ruleProximoCycle
  , ruleNoon
  , ruleNossaSenhoraAparecida
  , ruleNow
  , ruleCycleAntesDeTime
  , ruleCyclePassado
  , rulePassadoCycle
  , rulePartofdayDessaSemana
  , rulePassadosNCycle
  , ruleNCyclePassados
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
  , ruleCycleOrdinalOfTime
  , ruleCycleOrdinalTime
  , ruleCycleTheOrdinalOfTime
  , ruleOrdinalTrimestre
  , ruleTheOrdinalTrimestre
  , ruleOrdinalTrimestreYear
  , ruleLastCycleOfTime
  , ruleLastCycleTime
  , ruleIntervalFromMonthDDDDOf
  , ruleIntervalDDDDMonthOf
  , ruleIntervalFromMonthDDDD
  , ruleIntervalDDDDMonth
  , ruleOrdinalCycleOfYearOrdinalCycleOfYear
  , ruleOrdinalCycleYearOrdinalCycleYear
  ]
  ++ ruleMonths
  ++ ruleDaysOfWeek
