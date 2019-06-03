-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RO.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Data.Text as Text
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleAcum :: Rule
ruleAcum = Rule
  { name = "acum"
  , pattern =
    [ regex "(chiar)? ?acum|imediat"
    ]
  , prod = \_ -> tt now
  }

ruleDupamiaza :: Rule
ruleDupamiaza = Rule
  { name = "dupamiaza"
  , pattern =
    [ regex "dupamiaz(a|ă)|dup(a|ă) amiaz(a|ă)"
    ]
  , prod = \_ -> Token Time . mkLatent . partOfDay <$>
      interval TTime.Open (hour False 12) (hour False 19)
  }

ruleValentinesDay :: Rule
ruleValentinesDay = Rule
  { name = "valentine's day"
  , pattern =
    [ regex "sf\\.?((a|â)ntul)? Valentin"
    ]
  , prod = \_ -> tt $ monthDay 2 14
  }

ruleSinceTimeofday :: Rule
ruleSinceTimeofday = Rule
  { name = "since <time-of-day>"
  , pattern =
    [ regex "de|din"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleNewYearsDay :: Rule
ruleNewYearsDay = Rule
  { name = "new year's day"
  , pattern =
    [ regex "(siua de )? an(ul)? nou"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleOrdinalTrimestruYear :: Rule
ruleOrdinalTrimestruYear = Rule
  { name = "<ordinal> trimestru <year>"
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

ruleInNamedmonth :: Rule
ruleInNamedmonth = Rule
  { name = "in <named-month>"
  , pattern =
    [ regex "(i|î)n"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleLastCycleOfTime :: Rule
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "ultim(ul|a)"
    , dimension TimeGrain
    , regex "din"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleIntroNamedday :: Rule
ruleIntroNamedday = Rule
  { name = "intr-o <named-day>"
  , pattern =
    [ regex "((i|î)n(tr)?(\\-?o)?)"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleMonthDdddInterval :: Rule
ruleMonthDdddInterval = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ Predicate isAMonth
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (m1:_)):
       _:
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
        d1 <- parseInt m1
        d2 <- parseInt m2
        dom1 <- intersect (dayOfMonth d1) td
        dom2 <- intersect (dayOfMonth d2) td
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "(i|î)n"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleIeri :: Rule
ruleIeri = Rule
  { name = "ieri"
  , pattern =
    [ regex "ieri"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleCycleAcesta :: Rule
ruleCycleAcesta = Rule
  { name = "<cycle> acesta"
  , pattern =
    [ regex "aceasta|acest|(a|ă)sta"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt $ cycleNth grain 0
      _ -> Nothing
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

ruleAzi :: Rule
ruleAzi = Rule
  { name = "azi"
  , pattern =
    [ regex "a(st(a|ă))?zi"
    ]
  , prod = \_ -> tt today
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "noon"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleTimeTrecuta :: Rule
ruleTimeTrecuta = Rule
  { name = "<time> trecut[aă]?"
  , pattern =
    [ dimension Time
    , regex "(trecut(a|ă)?)"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleThisnextDayofweek :: Rule
ruleThisnextDayofweek = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "aceasta|(a|ă)sta|urm(a|ă)toare"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleBetweenTimeofdayAndTimeofdayInterval :: Rule
ruleBetweenTimeofdayAndTimeofdayInterval = Rule
  { name = "between <time-of-day> and <time-of-day> (interval)"
  , pattern =
    [ regex "(i|î)ntre"
    , Predicate isATimeOfDay
    , regex "(s|ș)i"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleUrmatoareaCycle :: Rule
ruleUrmatoareaCycle = Rule
  { name = "urmatoarea <cycle>"
  , pattern =
    [ regex "(urm(a|ă)to(area|rul)|viito(are|r))"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleCycleAcesta2 :: Rule
ruleCycleAcesta2 = Rule
  { name = "<cycle> acesta"
  , pattern =
    [ dimension TimeGrain
    , regex "aceasta|acest|(a|ă)sta|curent(a|ă)"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleCraciun :: Rule
ruleCraciun = Rule
  { name = "craciun"
  , pattern =
    [ regex "(ziua de )?cr(a|ă)ciun"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleTrimestruNumeralYear :: Rule
ruleTrimestruNumeralYear = Rule
  { name = "trimestru <number> <year>"
  , pattern =
    [ Predicate $ isGrain TG.Quarter
    , dimension Numeral
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token Time td:_) -> do
        v <- getIntValue token
        tt $ cycleNthAfter False TG.Quarter (v - 1) td
      _ -> Nothing
  }

ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd/mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])/(0?[1-9]|1[0-2])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleLaTimeofday :: Rule
ruleLaTimeofday = Rule
  { name = "la <time-of-day>"
  , pattern =
    [ regex "la|@ (ora)?"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleBlackFriday :: Rule
ruleBlackFriday = Rule
  { name = "black friday"
  , pattern =
    [ regex "black frid?day"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 4 5 11
  }

ruleChristmasEve :: Rule
ruleChristmasEve = Rule
  { name = "christmas eve"
  , pattern =
    [ regex "ajun(ul)? (de )?cr(a|ă)ciun"
    ]
  , prod = \_ -> tt $ monthDay 12 24
  }

rulePentruDuration :: Rule
rulePentruDuration = Rule
  { name = "pentru <duration>"
  , pattern =
    [ regex "pentru"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt $ inDuration dd
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

ruleHourofdaySfert :: Rule
ruleHourofdaySfert = Rule
  { name = "<hour-of-day> sfert"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "((s|ș)i )?(un )?sfert"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHourofdayJumatate :: Rule
ruleHourofdayJumatate = Rule
  { name = "<hour-of-day> sfert"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "((s|ș)i )?jum(a|ă)tate|jumate"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleDiseara :: Rule
ruleDiseara = Rule
  { name = "diseara"
  , pattern =
    [ regex "disear(a|ă)|((i|î)n aceas(a|ă) )?sear(a|ă)"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect today td2
  }

ruleIntersectBy :: Rule
ruleIntersectBy = Rule
  { name = "intersect by \",\""
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

ruleDayofmonthNumeral :: Rule
ruleDayofmonthNumeral = Rule
  { name = "<day-of-month> (number)"
  , pattern =
    [ Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ dayOfMonth v
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "dup(a|ă)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td1:_:Token Time td2:_) -> do
        v <- getIntValue token
        tt $ predNthAfter (v - 1) td1 td2
      _ -> Nothing
  }

ruleMmdd :: Rule
ruleMmdd = Rule
  { name = "mm/dd"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])/(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
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

ruleFromTimeofdayTimeofdayInterval :: Rule
ruleFromTimeofdayTimeofdayInterval = Rule
  { name = "from <time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ regex "(dup(a|ă)|(i|î)ncep(a|â)nd cu)"
    , Predicate isATimeOfDay
    , regex "(dar |(s|ș)i )?((i|î)nainte|p(a|â)n(a|ă) la( de)?)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleSeason3 :: Rule
ruleSeason3 = Rule
  { name = "season"
  , pattern =
    [ regex "primavar(a|ă)"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (monthDay 3 20) (monthDay 6 21)
  }

ruleUrmatoareleNCycle :: Rule
ruleUrmatoareleNCycle = Rule
  { name = "urmatoarele n <cycle>"
  , pattern =
    [ regex "urm(a|ă)to(arele|rii|area)"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "season"
  , pattern =
    [ regex "toamn(a|ă)"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (monthDay 9 23) (monthDay 12 21)
  }

ruleDupaDuration :: Rule
ruleDupaDuration = Rule
  { name = "dupa <duration>"
  , pattern =
    [ regex "dup(a|ă)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt . withDirection TTime.After $ inDuration dd
      _ -> Nothing
  }

ruleNewYearsEve :: Rule
ruleNewYearsEve = Rule
  { name = "new year's eve"
  , pattern =
    [ regex "(ajun(ul)? )?(de )?an(ul)? nou"
    ]
  , prod = \_ -> tt $ monthDay 12 31
  }

ruleByTheEndOfTime :: Rule
ruleByTheEndOfTime = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "p(a|î)n(a|ă) ((i|î)n|la)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Closed now td
      _ -> Nothing
  }

ruleNameddayPeDayofmonthNumeral :: Rule
ruleNameddayPeDayofmonthNumeral = Rule
  { name = "<named-day> pe <day-of-month> (number)"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "pe"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDayofmonthNonOrdinalNamedmonth :: Rule
ruleDayofmonthNonOrdinalNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
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

ruleAboutTimeofday :: Rule
ruleAboutTimeofday = Rule
  { name = "about <time-of-day>"
  , pattern =
    [ regex "(cam|aproximativ|(i|î)n jur de)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleUntilTimeofday :: Rule
ruleUntilTimeofday = Rule
  { name = "until <time-of-day>"
  , pattern =
    [ regex "p(a|î)n(a|ă) ((i|î)n|la)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleDayofmonthnumberNamedmonth :: Rule
ruleDayofmonthnumberNamedmonth = Rule
  { name = "<day-of-month>(number) <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleIntreDatetimeSiDatetimeInterval :: Rule
ruleIntreDatetimeSiDatetimeInterval = Rule
  { name = "intre <datetime> si <datetime> (interval)"
  , pattern =
    [ regex "(i|î)nre"
    , dimension Time
    , regex "(s|ș)i"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "din"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td1:_:Token Time td2:_) -> do
        v <- getIntValue token
        Token Time . predNth (v - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleTimePartofday :: Rule
ruleTimePartofday = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "(week(\\s|\\-)?end|wkend)"
    ]
  , prod = \_ -> tt weekend
  }

rulePeDayofmonthNonOrdinal :: Rule
rulePeDayofmonthNonOrdinal = Rule
  { name = "pe <day-of-month> (non ordinal)"
  , pattern =
    [ regex "pe"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ dayOfMonth v
      _ -> Nothing
  }

ruleTimeAceastaacestaasta :: Rule
ruleTimeAceastaacestaasta = Rule
  { name = "<time> (aceasta|acesta|[aă]sta)"
  , pattern =
    [ dimension Time
    , regex "aceasta|(a|ă)sta|urm(a|ă)toare"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleEomendOfMonth :: Rule
ruleEomendOfMonth = Rule
  { name = "EOM|End of month"
  , pattern =
    [ regex "sf(a|â)r(s|ș)itul lunii"
    ]
  , prod = \_ -> tt $ cycleNth TG.Month 1
  }

rulePartofdayAsta :: Rule
rulePartofdayAsta = Rule
  { name = "<part-of-day> asta"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "asta"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        Token Time . partOfDay <$> intersect today td
      _ -> Nothing
  }

ruleUltimaCycle :: Rule
ruleUltimaCycle = Rule
  { name = "ultima <cycle>"
  , pattern =
    [ regex "ultim(a|ul)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleCycleUrmatoare :: Rule
ruleCycleUrmatoare = Rule
  { name = "<cycle> urmatoare"
  , pattern =
    [ dimension TimeGrain
    , regex "(urm(a|ă)to(are|r)|viito(are|r))"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleTheDayofmonthNumeral :: Rule
ruleTheDayofmonthNumeral = Rule
  { name = "the <day-of-month> (number)"
  , pattern =
    [ regex "pe"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt $ dayOfMonth v
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

ruleCycleTrecut :: Rule
ruleCycleTrecut = Rule
  { name = "<cycle> trecut"
  , pattern =
    [ dimension TimeGrain
    , regex "trecut(a|ă)?"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleDayofmonthNumeralOfNamedmonth :: Rule
ruleDayofmonthNumeralOfNamedmonth = Rule
  { name = "<day-of-month> (number) of <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , regex "din"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDurationInainteDeTime :: Rule
ruleDurationInainteDeTime = Rule
  { name = "<duration> inainte de <time>"
  , pattern =
    [ dimension Duration
    , regex "(i|î)nainte de"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleDayofmonthNonOrdinalOfNamedmonth :: Rule
ruleDayofmonthNonOrdinalOfNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) of <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , regex "din"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDurationInUrma :: Rule
ruleDurationInUrma = Rule
  { name = "<duration> in urma"
  , pattern =
    [ dimension Duration
    , regex "(i|î)n urm(a|ă)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ durationAgo dd
      _ -> Nothing
  }

ruleDurationDeAcum :: Rule
ruleDurationDeAcum = Rule
  { name = "<duration> de acum"
  , pattern =
    [ dimension Duration
    , regex "de (acum|azi)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleSezonAnotimp :: Rule
ruleSezonAnotimp = Rule
  { name = "sezon anotimp"
  , pattern =
    [ regex "var(a|ă)"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 6 21) (monthDay 9 23)
  }

ruleSearaNoapte :: Rule
ruleSearaNoapte = Rule
  { name = "sear[aă] noapte"
  , pattern =
    [ regex "sear(a|ă)|noapte"
    ]
  , prod = \_ -> Token Time . mkLatent . partOfDay <$>
      interval TTime.Open (hour False 18) (hour False 0)
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween (- 10000) 999
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ year v
      _ -> Nothing
  }

ruleSeason2 :: Rule
ruleSeason2 = Rule
  { name = "season"
  , pattern =
    [ regex "iarn(a|ă)"
    ]
  , prod = \_ ->
      Token Time <$> interval TTime.Open (monthDay 12 21) (monthDay 3 20)
  }

ruleUltimeleNCycle :: Rule
ruleUltimeleNCycle = Rule
  { name = "ultimele n <cycle>"
  , pattern =
    [ regex "ultim(ele|ii|a)"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain (- v)
      _ -> Nothing
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "dup(a|ă)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleDimineata :: Rule
ruleDimineata = Rule
  { name = "diminea[tț][aă]"
  , pattern =
    [ regex "diminea(t|ț)(a|ă)"
    ]
  , prod = \_ -> Token Time . mkLatent . partOfDay <$>
      interval TTime.Open (hour False 4) (hour False 12)
  }

ruleTimeUrmatoarer :: Rule
ruleTimeUrmatoarer = Rule
  { name = "<time> urm[aă]to(are|r)"
  , pattern =
    [ regex "urm(a|ă)to(are|r)"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleTimeofdayFix :: Rule
ruleTimeofdayFix = Rule
  { name = "<time-of-day> fix"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(fix|exact)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleMaine :: Rule
ruleMaine = Rule
  { name = "maine"
  , pattern =
    [ regex "m(a|â)ine"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
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

ruleHalloweenDay :: Rule
ruleHalloweenDay = Rule
  { name = "halloween day"
  , pattern =
    [ regex "hall?owe?en"
    ]
  , prod = \_ -> tt $ monthDay 10 31
  }

ruleNamedmonthDayofmonthNonOrdinal :: Rule
ruleNamedmonthDayofmonthNonOrdinal = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

rulePeDate :: Rule
rulePeDate = Rule
  { name = "pe <date>"
  , pattern =
    [ regex "pe"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleDayofmonthnumberNamedmonthYear :: Rule
ruleDayofmonthnumberNamedmonthYear = Rule
  { name = "<day-of-month>(number) <named-month> year"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:
       Token Time td:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
        v <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year v)
      _ -> Nothing
  }

ruleAbsorptionOfAfterNamedDay :: Rule
ruleAbsorptionOfAfterNamedDay = Rule
  { name = "absorption of , after named day"
  , pattern =
    [ Predicate isADayOfWeek
    , regex ","
    ]
  , prod = \tokens -> case tokens of
      (x:_) -> Just x
      _ -> Nothing
  }

ruleLastDayofweekOfTime :: Rule
ruleLastDayofweekOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "ultima"
    , Predicate isADayOfWeek
    , regex "din"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleOrdinalTrimestru :: Rule
ruleOrdinalTrimestru = Rule
  { name = "<ordinal> trimestru"
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

ruleByTime :: Rule
ruleByTime = Rule
  { name = "by <time>"
  , pattern =
    [ regex "p(a|â)n(a|ă) (la|(i|î)n)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[-/](0?[1-9]|1[0-2])[/-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleMmddyyyy :: Rule
ruleMmddyyyy = Rule
  { name = "mm/dd/yyyy"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])[/-](3[01]|[12]\\d|0?[1-9])[-/](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleEoyendOfYear :: Rule
ruleEoyendOfYear = Rule
  { name = "EOY|End of year"
  , pattern =
    [ regex "sf(a|â)r(s|ș)itul anului"
    ]
  , prod = \_ -> tt $ cycleNth TG.Year 1
  }

ruleMothersDay :: Rule
ruleMothersDay = Rule
  { name = "Mother's Day"
  , pattern =
    [ regex "ziua (mamei|memeii)"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 1 7 5
  }

ruleNameddayDayofmonthNumeral :: Rule
ruleNameddayDayofmonthNumeral = Rule
  { name = "<named-day> <day-of-month> (number)"
  , pattern =
    [ Predicate isADayOfWeek
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleHhmmss :: Rule
ruleHhmmss = Rule
  { name = "hh:mm:ss"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)[:.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        s <- parseInt ss
        tt $ hourMinuteSecond True h m s
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "luni"  , "lu(n(ea|i)?)?"                        )
  , ( "marti"  , "ma(r((t|ț)(ea|i))?)?"                )
  , ( "miercuri"  , "mi(e(rcur(ea|i))?)?"              )
  , ( "joi"  , "jo(ia?)?"                              )
  , ( "vineri"  , "vi(n(er(ea|i))?)?"                  )
  , ( "sambata" , "s(a|â)mb(a|ă)t(a|ă)|s(a|â)m"        )
  , ( "duminica"  , "du(m(inic(a|ă))?)?"               )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "ianuarie"  , "ian(uarie)?"     )
  , ( "februarie"  , "feb(ruarie)?"   )
  , ( "martie"  , "martie|mar"        )
  , ( "aprilie"  , "apr(ilie)?"       )
  , ( "mai"  , "mai"                  )
  , ( "iunie"  , "iun(ie)?"           )
  , ( "iulie"  , "iul(ie)?"           )
  , ( "august"  , "aug(ust)?"         )
  , ( "septembrie"  , "sept(embrie)?" )
  , ( "octombrie"  , "oct(ombrie)?"   )
  , ( "noiembrie"  , "noi(embrie)?"   )
  , ( "decembrie"  , "dec(embrie)?"   )
  ]

rules :: [Rule]
rules =
  [ ruleAboutTimeofday
  , ruleAbsorptionOfAfterNamedDay
  , ruleAcum
  , ruleAfterTimeofday
  , ruleAzi
  , ruleBetweenTimeofdayAndTimeofdayInterval
  , ruleBlackFriday
  , ruleByTheEndOfTime
  , ruleByTime
  , ruleChristmasEve
  , ruleCraciun
  , ruleCycleAcesta
  , ruleCycleAcesta2
  , ruleCycleTrecut
  , ruleCycleUrmatoare
  , ruleDayofmonthNonOrdinalNamedmonth
  , ruleDayofmonthNonOrdinalOfNamedmonth
  , ruleDayofmonthNumeral
  , ruleDayofmonthNumeralOfNamedmonth
  , ruleDayofmonthnumberNamedmonth
  , ruleDayofmonthnumberNamedmonthYear
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleDimineata
  , ruleDiseara
  , ruleDupaDuration
  , ruleDupamiaza
  , ruleDurationDeAcum
  , ruleDurationInUrma
  , ruleDurationInainteDeTime
  , ruleEomendOfMonth
  , ruleEoyendOfYear
  , ruleFromTimeofdayTimeofdayInterval
  , ruleHalloweenDay
  , ruleHhmm
  , ruleHhmmss
  , ruleHourofdayIntegerAsRelativeMinutes
  , ruleHourofdayJumatate
  , ruleHourofdaySfert
  , ruleIeri
  , ruleInDuration
  , ruleInNamedmonth
  , ruleIntersect
  , ruleIntersectBy
  , ruleIntreDatetimeSiDatetimeInterval
  , ruleIntroNamedday
  , ruleLaTimeofday
  , ruleLastCycleOfTime
  , ruleLastDayofweekOfTime
  , ruleMaine
  , ruleMmdd
  , ruleMmddyyyy
  , ruleMonthDdddInterval
  , ruleMothersDay
  , ruleNameddayDayofmonthNumeral
  , ruleNameddayPeDayofmonthNumeral
  , ruleNamedmonthDayofmonthNonOrdinal
  , ruleNewYearsDay
  , ruleNewYearsEve
  , ruleNoon
  , ruleNthTimeAfterTime
  , ruleNthTimeOfTime
  , ruleOrdinalTrimestru
  , ruleOrdinalTrimestruYear
  , rulePartofdayAsta
  , rulePeDate
  , rulePeDayofmonthNonOrdinal
  , rulePentruDuration
  , ruleSearaNoapte
  , ruleSeason
  , ruleSeason2
  , ruleSeason3
  , ruleSezonAnotimp
  , ruleSinceTimeofday
  , ruleTheDayofmonthNumeral
  , ruleThisnextDayofweek
  , ruleTimeAceastaacestaasta
  , ruleTimePartofday
  , ruleTimezone
  , ruleTimeTrecuta
  , ruleTimeUrmatoarer
  , ruleTimeofdayFix
  , ruleTimeofdayLatent
  , ruleTrimestruNumeralYear
  , ruleUltimaCycle
  , ruleUltimeleNCycle
  , ruleUntilTimeofday
  , ruleUrmatoareaCycle
  , ruleUrmatoareleNCycle
  , ruleValentinesDay
  , ruleWeekend
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYyyymmdd
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
