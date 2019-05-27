-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.FR.Rules
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
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleAujourdhui :: Rule
ruleAujourdhui = Rule
  { name = "aujourd'hui"
  , pattern =
    [ regex "(aujourd'? ?hui)|(ce jour)|(dans la journ(é|e)e?)|(en ce moment)"
    ]
  , prod = \_ -> tt today
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

ruleHier :: Rule
ruleHier = Rule
  { name = "hier"
  , pattern =
    [ regex "hier|la veille"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleDbutDeSoire :: Rule
ruleDbutDeSoire = Rule
  { name = "début de soirée"
  , pattern =
    [ regex "(en |au )?d(é|e)but de (la )?soir(é|e)e"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 18) (hour False 21)
  }

ruleFinDeSoire :: Rule
ruleFinDeSoire = Rule
  { name = "fin de soirée"
  , pattern =
    [ regex "(en |(à|a) la )?fin de (la )?soir(é|e)e"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 21) (hour False 0)
  }

ruleDbutDeMatine :: Rule
ruleDbutDeMatine = Rule
  { name = "début de matinée"
  , pattern =
    [ regex "le matin (tr(e|è)s )?t(ô|o)t|(tr(e|è)s )?t(ô|o)t le matin|(en |au )?d(é|e)but de (la )?matin(é|e)e"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 7) (hour False 9)
  }

ruleMilieuDeMatine :: Rule
ruleMilieuDeMatine = Rule
  { name = "milieu de matinée"
  , pattern =
    [ regex "(en |au )?milieu de (la )?matin(é|e)e"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 9) (hour False 11)
  }

ruleFinDeMatine :: Rule
ruleFinDeMatine = Rule
  { name = "fin de matinée"
  , pattern =
    [ regex "(en |(à|a) la )?fin de (la )?matin(é|e)e"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 10) (hour False 12)
  }

ruleDbutDaprsmidi :: Rule
ruleDbutDaprsmidi = Rule
  { name = "début d'après-midi"
  , pattern =
    [ regex "(au |en )?d(é|e)but (d'|de l')apr(e|é|è)s( |\\-)midi"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 12) (hour False 14)
  }

ruleMilieuDaprsmidi :: Rule
ruleMilieuDaprsmidi = Rule
  { name = "milieu d'après-midi"
  , pattern =
    [ regex "(au |en )?milieu (d'|de l')apr(e|é|è)s( |\\-)midi"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 14) (hour False 17)
  }

ruleFinDaprsmidi :: Rule
ruleFinDaprsmidi = Rule
  { name = "fin d'après-midi"
  , pattern =
    [ regex "((à|a) la |en )?fin (d'|de l')apr(e|é|è)s( |\\-)midi"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 17) (hour False 19)
  }

ruleDbutDeJourne :: Rule
ruleDbutDeJourne = Rule
  { name = "début de journée"
  , pattern =
    [ regex "(en |au )?d(é|e)but de (la )?journ(é|e)e"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
        interval TTime.Open (hour False 6) (hour False 10)
  }

ruleMilieuDeJourne :: Rule
ruleMilieuDeJourne = Rule
  { name = "milieu de journée"
  , pattern =
    [ regex "(en |au )?milieu de (la )?journ(é|e)e"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 11) (hour False 16)
  }

ruleFinDeJourne :: Rule
ruleFinDeJourne = Rule
  { name = "fin de journée"
  , pattern =
    [ regex "(en |(à|a) la )?fin de (la )?journ(é|e)e"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 17) (hour False 21)
  }

ruleDbutDeSemaine :: Rule
ruleDbutDeSemaine = Rule
  { name = "début de semaine"
  , pattern =
    [ regex "(en |au )?d(é|e)but de (cette |la )?semaine"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (dayOfWeek 1) (dayOfWeek 2)
  }

ruleMilieuDeSemaine :: Rule
ruleMilieuDeSemaine = Rule
  { name = "milieu de semaine"
  , pattern =
    [ regex "(en |au )?milieu de (cette |la )?semaine"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (dayOfWeek 3) (dayOfWeek 4)
  }

ruleFinDeSemaine :: Rule
ruleFinDeSemaine = Rule
  { name = "fin de semaine"
  , pattern =
    [ regex "(en |(à|a) la )?fin de (cette |la )?semaine"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (dayOfWeek 4) (dayOfWeek 7)
  }

ruleLeLendemainDuTime :: Rule
ruleLeLendemainDuTime = Rule
  { name = "le lendemain du <time>"
  , pattern =
    [ regex "(le|au)? ?lendemain du"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ cycleNthAfter True TG.Day 1 td
      _ -> Nothing
  }

ruleCePartofday :: Rule
ruleCePartofday = Rule
  { name = "ce <part-of-day>"
  , pattern =
    [ regex "cet?t?e?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay . notLatent <$>
        intersect today td
      _ -> Nothing
  }

ruleCeTime :: Rule
ruleCeTime = Rule
  { name = "ce <time>"
  , pattern =
    [ regex "ce"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleDurationAvantTime :: Rule
ruleDurationAvantTime = Rule
  { name = "<duration> avant <time>"
  , pattern =
    [ dimension Duration
    , regex "avant"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleMidi :: Rule
ruleMidi = Rule
  { name = "midi"
  , pattern =
    [ regex "midi"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|au|jusqu'(au?|à)"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDeTimeofdayTimeofdayInterval :: Rule
ruleDeTimeofdayTimeofdayInterval = Rule
  { name = "de <time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ regex "(midi )?de"
    , Predicate isATimeOfDay
    , regex "\\-|(jusqu')?(à|au?)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleNProchainsCycle :: Rule
ruleNProchainsCycle = Rule
  { name = "n prochains <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "prochaine?s?|suivante?s?|apr(e|è|é)s"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain n
      _ -> Nothing
  }

ruleNDerniersCycle :: Rule
ruleNDerniersCycle = Rule
  { name = "n derniers <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "derni(e|è|é)re?s?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleN True grain $ - n
      _ -> Nothing
  }

ruleAvantTimeofday :: Rule
ruleAvantTimeofday = Rule
  { name = "avant <time-of-day>"
  , pattern =
    [ regex "(n[ ']importe quand )?(avant|jusqu'(a|à))"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleEntreDatetimeEtDatetimeInterval :: Rule
ruleEntreDatetimeEtDatetimeInterval = Rule
  { name = "entre <datetime> et <datetime> (interval)"
  , pattern =
    [ regex "entre"
    , dimension Time
    , regex "et"
    , dimension Time
    ]
  , prod = \case
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDuDatetimedayofweekDdMonthinterval :: Rule
ruleDuDatetimedayofweekDdMonthinterval = Rule
  { name = "du <datetime>-<day-of-week> dd <month>(interval)"
  , pattern =
    [ regex "du"
    , dimension Time
    , regex "\\-|au|jusqu'au"
    , Predicate isADayOfWeek
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \case
      (_:Token Time td1:_:_:token:Token Time td2:_) -> do
        from <- intersect td2 td1
        to <- intersectDOM td2 token
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleDuDdAuDdLatentMonthInterval :: Rule
ruleDuDdAuDdLatentMonthInterval = Rule
  { name = "du dd au dd (interval) (latent month)"
  , pattern =
    [ regex "du"
    , Predicate isDOMValue
    , regex "au|jusqu'au"
    , Predicate isDOMValue
    ]
  , prod = \case
      (_:token1:_:token2:_) -> do
        n1 <- getIntValue token1
        n2 <- getIntValue token2
        Token Time <$> interval TTime.Closed (dayOfMonth n1) (dayOfMonth n2)
      _ -> Nothing
  }

ruleDatetimedayofweekDdMonthinterval :: Rule
ruleDatetimedayofweekDdMonthinterval = Rule
  { name = "<datetime>-<day-of-week> dd <month>(interval)"
  , pattern =
    [ dimension Time
    , regex "\\-|(jusqu')?au"
    , Predicate isADayOfWeek
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \case
      (Token Time td1:_:_:token:Token Time td2:_) -> do
        from <- intersect td2 td1
        to <- intersectDOM td2 token
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleEntreDdEtDdMonthinterval :: Rule
ruleEntreDdEtDdMonthinterval = Rule
  { name = "entre dd et dd <month>(interval)"
  , pattern =
    [ regex "entre( le)?"
    , Predicate isDOMValue
    , regex "et( le)?"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \case
      (_:token1:_:token2:Token Time td:_) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleDatetimeddMonthinterval :: Rule
ruleDatetimeddMonthinterval = Rule
  { name = "<datetime>-dd <month>(interval)"
  , pattern =
    [ dimension Time
    , regex "\\-|au|jusqu'au"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \case
      (Token Time td1:_:token:Token Time td2:_) -> do
        from <- intersect td2 td1
        to <- intersectDOM td2 token
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleDuDdAuDdMonthInterval :: Rule
ruleDuDdAuDdMonthInterval = Rule
  { name = "du dd/nth au dd/nth month (interval)"
  , pattern =
    [ regex "du"
    , Predicate isDOMValue
    , regex "\\-|au|jusqu'(au?|à)"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \case
      (_:token1:_:token2:Token Time td:_) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
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
        tt $ hourMinute (h < 12) h m
      _ -> Nothing
  }

ruleDdMm :: Rule
ruleDdMm = Rule
  { name = "dd mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9]) (1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleDdMmYyyy :: Rule
ruleDdMmYyyy = Rule
  { name = "dd mm yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9]) (1[0-2]|0?[1-9]) (\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleNamedmonthProchain :: Rule
ruleNamedmonthProchain = Rule
  { name = "<named-month> prochain"
  , pattern =
    [ dimension Time
    , regex "prochain"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 False td
      _ -> Nothing
  }

ruleDiciDuration :: Rule
ruleDiciDuration = Rule
  { name = "d'ici <duration>"
  , pattern =
    [ regex "d'ici|dans l('|es?)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }

ruleToussaint :: Rule
ruleToussaint = Rule
  { name = "toussaint"
  , pattern =
    [ regex "((la |la journ(é|e)e de la |jour de la )?toussaint|jour des morts)"
    ]
  , prod = \_ -> tt $ monthDay 11 1
  }

ruleDernierCycleDeTimeLatent :: Rule
ruleDernierCycleDeTimeLatent = Rule
  { name = "dernier <cycle> de <time> (latent)"
  , pattern =
    [ regex "derni(e|é|è)re?"
    , dimension TimeGrain
    , regex "d['e]"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleDurationApresTime :: Rule
ruleDurationApresTime = Rule
  { name = "<duration> apres <time>"
  , pattern =
    [ dimension Duration
    , regex "apr(e|è)s"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationAfter dd td
      _ -> Nothing
  }

ruleNCycleAprs :: Rule
ruleNCycleAprs = Rule
  { name = "n <cycle> après"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(d')? ?apr(e|è|é)s|qui sui(t|ves?)|plus tard"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleNth grain n
      _ -> Nothing
  }

ruleSeason4 :: Rule
ruleSeason4 = Rule
  { name = "season"
  , pattern =
    [ regex "(ce )?printemps"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (monthDay 3 20) (monthDay 6 21)
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

ruleNCycleAvant :: Rule
ruleNCycleAvant = Rule
  { name = "n <cycle> avant"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(d')? ?avant|plus t(o|ô)t"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleNth grain $ - n
      _ -> Nothing
  }

ruleDimTimeDuMatin :: Rule
ruleDimTimeDuMatin = Rule
  { name = "<dim time> du matin"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "((du|dans|de) )?((au|le|la) )?mat(in(é|e)?e?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        morning <- partOfDay . mkLatent <$>
          interval TTime.Open (hour False 0) (hour False 12)
        Token Time <$> intersect td morning
      _ -> Nothing
  }

ruleOrdinalWeekendDeTime :: Rule
ruleOrdinalWeekendDeTime = Rule
  { name = "<ordinal> week-end de <time>"
  , pattern =
    [ dimension Ordinal
    , regex "week(\\s|-)?end (d['eu]|en|du mois de)"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        td2 <- intersect td weekend
        tt $ predNth (n - 1) False td2
      _ -> Nothing
  }

ruleHourofdayEtQuart :: Rule
ruleHourofdayEtQuart = Rule
  { name = "<hour-of-day> et quart"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(et )?quart"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHourofdayEtDemi :: Rule
ruleHourofdayEtDemi = Rule
  { name = "<hour-of-day> et demi"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "et demie?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHourofdayEtTroisQuart :: Rule
ruleHourofdayEtTroisQuart = Rule
  { name = "<hour-of-day> et trois quarts"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(et )?(3|trois) quarts?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 45
      _ -> Nothing
  }

ruleHourofdayEtpassDeNumeral :: Rule
ruleHourofdayEtpassDeNumeral = Rule
  { name = "<hour-of-day> et|passé de <number>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "et|(pass(é|e)e? de)"
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

ruleHourofdayEtpassDeNumeralMinutes :: Rule
ruleHourofdayEtpassDeNumeralMinutes = Rule
  { name = "<hour-of-day> et|passé de <number> minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "et|(pass(é|e)e? de)"
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(ute)?s?"
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

ruleHourofdayInteger :: Rule
ruleHourofdayInteger = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay, hasNoDirection]
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

ruleHourofdayIntegerMinutes :: Rule
ruleHourofdayIntegerMinutes = Rule
  { name = "<hour-of-day> <integer> minutes"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(ute)?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHourofdayMoinsIntegerAsRelativeMinutes :: Rule
ruleHourofdayMoinsIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> moins <integer> (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "moins( le)?"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }

ruleHourofdayMoinsQuart :: Rule
ruleHourofdayMoinsQuart = Rule
  { name = "<hour-of-day> moins quart"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "moins( le)? quart"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleAprsLeDayofmonth :: Rule
ruleAprsLeDayofmonth = Rule
  { name = "après le <day-of-month>"
  , pattern =
    [ regex "(apr(e|è)s le|(a|à) partir du)"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt . withDirection TTime.After $ dayOfMonth n
      _ -> Nothing
  }

ruleCycleDernier :: Rule
ruleCycleDernier = Rule
  { name = "<cycle> dernier"
  , pattern =
    [ dimension TimeGrain
    , regex "derni(è|e)re?|pass(é|e)e?|pr(e|é)c(e|é)dente?|(d')? ?avant"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleLeOrdinalCycleDeTime :: Rule
ruleLeOrdinalCycleDeTime = Rule
  { name = "le <ordinal> <cycle> de <time>"
  , pattern =
    [ regex "l[ea]"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "d['eu]|en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleEnSemaine :: Rule
ruleEnSemaine = Rule
  { name = "en semaine"
  , pattern =
    [ regex "(pendant la |en )?semaine"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (dayOfWeek 1) (dayOfWeek 5)
  }

ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd/-mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/-](1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleNamedmonthnameddayDernierpass :: Rule
ruleNamedmonthnameddayDernierpass = Rule
  { name = "<named-month|named-day> dernier|passé"
  , pattern =
    [ dimension Time
    , regex "derni(e|é|è)re?|pass(é|e)e?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleLeCycleDernier :: Rule
ruleLeCycleDernier = Rule
  { name = "le <cycle> dernier"
  , pattern =
    [ regex "l[ae']? ?"
    , dimension TimeGrain
    , regex "derni(è|e)re?|pass(é|e)e?"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleNCyclePassesprecedents :: Rule
ruleNCyclePassesprecedents = Rule
  { name = "n <cycle> passes|precedents"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "pass(e|è|é)(e|è|é)?s?|pr(e|é)c(e|é)dente?s?|(d')? ?avant|plus t(o|ô)t"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleN True grain $ - n
      _ -> Nothing
  }

ruleSoir :: Rule
ruleSoir = Rule
  { name = "soir"
  , pattern =
    [ regex "soir(é|e)?e?"
    ]
  , prod = \_ -> Token Time . mkLatent . partOfDay <$>
      interval TTime.Open (hour False 18) (hour False 0)
  }

ruleDdddMonthinterval :: Rule
ruleDdddMonthinterval = Rule
  { name = "dd-dd <month>(interval)"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-|au|jusqu'au"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):_:Token RegexMatch (GroupMatch (m2:_)):Token Time td:_) -> do
        d1 <- parseInt m1
        d2 <- parseInt m2
        from <- intersect td (dayOfMonth d1)
        to <- intersect td (dayOfMonth d2)
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
        n <- getIntValue token
        tt . mkLatent $ hour (n < 12) n
      _ -> Nothing
  }

ruleDuDddayofweekDdMonthinterval :: Rule
ruleDuDddayofweekDdMonthinterval = Rule
  { name = "du dd-<day-of-week> dd <month>(interval)"
  , pattern =
    [ regex "du"
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , regex "\\-|au|jusqu'au"
    , Predicate isADayOfWeek
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token RegexMatch (GroupMatch (m1:_)):_:_:Token RegexMatch (GroupMatch (m2:_)):Token Time td:_) -> do
        d1 <- parseInt m1
        d2 <- parseInt m2
        from <- intersect (dayOfMonth d1) td
        to <- intersect (dayOfMonth d2) td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleDbutNamedmonthinterval :: Rule
ruleDbutNamedmonthinterval = Rule
  { name = "début <named-month>(interval)"
  , pattern =
    [ regex "d(é|e)but( du mois d[e'] ?)?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> do
        from <- intersect (dayOfMonth 1) td
        to <- intersect (dayOfMonth 5) td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleLeCycleDeTime :: Rule
ruleLeCycleDeTime = Rule
  { name = "le <cycle> de <time>"
  , pattern =
    [ regex "l[ea]"
    , dimension TimeGrain
    , regex "d['eu]|en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain 0 td
      _ -> Nothing
  }

ruleSeason3 :: Rule
ruleSeason3 = Rule
  { name = "season"
  , pattern =
    [ regex "(cet )?hiver"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (monthDay 12 21) (monthDay 3 20)
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "season"
  , pattern =
    [ regex "(cet )?(é|e)t(é|e)"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (monthDay 6 21) (monthDay 9 23)
  }

ruleAprsmidi :: Rule
ruleAprsmidi = Rule
  { name = "après-midi"
  , pattern =
    [ regex "apr(e|é|è)s( |\\-)midi"
    ]
  , prod = \_ -> Token Time . mkLatent . partOfDay <$>
      interval TTime.Open (hour False 12) (hour False 19)
  }

ruleNoel :: Rule
ruleNoel = Rule
  { name = "noel"
  , pattern =
    [ regex "(jour de )?no(e|ë)l"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleDayofweekProchain :: Rule
ruleDayofweekProchain = Rule
  { name = "<day-of-week> prochain"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "prochain"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleDemain :: Rule
ruleDemain = Rule
  { name = "demain"
  , pattern =
    [ regex "(demain)|(le lendemain)"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleNamedmonthnameddaySuivantdaprs :: Rule
ruleNamedmonthnameddaySuivantdaprs = Rule
  { name = "<named-month|named-day> suivant|d'après"
  , pattern =
    [ dimension Time
    , regex "suivante?s?|d'apr(e|é|è)s"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 False td
      _ -> Nothing
  }

ruleNameddayEnHuit :: Rule
ruleNameddayEnHuit = Rule
  { name = "<named-day> en huit"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "en (huit|8)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 False td
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

ruleDimTimeDuSoir :: Rule
ruleDimTimeDuSoir = Rule
  { name = "<dim time> du soir"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "((du|dans|de) )?((au|le|la) )?soir(é|e)?e?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        td2 <- mkLatent . partOfDay <$>
          interval TTime.Open (hour False 16) (hour False 0)
        Token Time <$> intersect td td2
      _ -> Nothing
  }

ruleAprsTimeofday :: Rule
ruleAprsTimeofday = Rule
  { name = "après <time-of-day>"
  , pattern =
    [ regex "(apr(e|è)s|(a|à) partir de|(un peu )?plus tard que)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleAprsLeDjeuner :: Rule
ruleAprsLeDjeuner = Rule
  { name = "après le déjeuner"
  , pattern =
    [ regex "apr(e|è)s (le )?d(e|é|è)jeuner"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 13) (hour False 17)
      Token Time . partOfDay <$> intersect today td2
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

ruleErMai :: Rule
ruleErMai = Rule
  { name = "1er mai"
  , pattern =
    [ regex "f(e|ê)te du travail"
    ]
  , prod = \_ -> tt $ monthDay 5 1
  }

rulePremireQuinzaineDeNamedmonthinterval :: Rule
rulePremireQuinzaineDeNamedmonthinterval = Rule
  { name = "première quinzaine de <named-month>(interval)"
  , pattern =
    [ regex "(premi(è|e)re|1 ?(è|e)re) (quinzaine|15 ?aine) d[e']"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> do
        from <- intersect (dayOfMonth 1) td
        to <- intersect (dayOfMonth 14) td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleDeDatetimeDatetimeInterval :: Rule
ruleDeDatetimeDatetimeInterval = Rule
  { name = "de <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "de|depuis|du"
    , dimension Time
    , regex "\\-|au|jusqu'(au?|à)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleAvanthier :: Rule
ruleAvanthier = Rule
  { name = "avant-hier"
  , pattern =
    [ regex "avant[- ]?hier"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
  }

ruleCycleProchainsuivantdaprs :: Rule
ruleCycleProchainsuivantdaprs = Rule
  { name = "<cycle> prochain|suivant|d'après"
  , pattern =
    [ dimension TimeGrain
    , regex "prochaine?|suivante?|qui suit|(d')? ?apr(e|è|é)s"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleAprsLeTravail :: Rule
ruleAprsLeTravail = Rule
  { name = "après le travail"
  , pattern =
    [ regex "apr(e|è)s (le )?travail"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 17) (hour False 21)
      Token Time . partOfDay <$> intersect today td2
  }

ruleLeDayofmonthDatetime :: Rule
ruleLeDayofmonthDatetime = Rule
  { name = "le <day-of-month> à <datetime>"
  , pattern =
    [ regex "le"
    , Predicate isDOMInteger
    , regex "(a|à)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> intersect (dayOfMonth n) td
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "week(\\s|-)?end"
    ]
  , prod = \_ -> tt weekend
  }

ruleCedansLeCycle :: Rule
ruleCedansLeCycle = Rule
  { name = "ce|dans le <cycle>"
  , pattern =
    [ regex "(cet?t?e?s?)|(dans l[ae']? ?)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleDansDuration :: Rule
ruleDansDuration = Rule
  { name = "dans <duration>"
  , pattern =
    [ regex "dans"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleOrdinalCycleDeTime :: Rule
ruleOrdinalCycleDeTime = Rule
  { name = "<ordinal> <cycle> de <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "d['eu]|en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleAvantLeDjeuner :: Rule
ruleAvantLeDjeuner = Rule
  { name = "avant le déjeuner"
  , pattern =
    [ regex "avant (le )?d(e|é|è)jeuner"
    ]
  , prod = \_ -> do
      td2 <- interval TTime.Open (hour False 10) (hour False 12)
      Token Time . partOfDay <$> intersect today td2
  }

ruleDernierWeekendDeTime :: Rule
ruleDernierWeekendDeTime = Rule
  { name = "dernier week-end de <time>"
  , pattern =
    [ regex "dernier week(\\s|-)?end (d['eu]|en|du mois de)"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> do
        tt $ predLastOf weekend td
      _ -> Nothing
  }

ruleLeCycleProchainsuivantdaprs :: Rule
ruleLeCycleProchainsuivantdaprs = Rule
  { name = "le <cycle> prochain|suivant|d'après"
  , pattern =
    [ regex "l[ae']? ?|une? ?"
    , dimension TimeGrain
    , regex "prochaine?|suivante?|qui suit|(d'? ?)?apr(e|è|é)s"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
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

ruleEnNamedmonth :: Rule
ruleEnNamedmonth = Rule
  { name = "en <named-month>"
  , pattern =
    [ regex "en|au mois de?'?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleEntreTimeofdayEtTimeofdayInterval :: Rule
ruleEntreTimeofdayEtTimeofdayInterval = Rule
  { name = "entre <time-of-day> et <time-of-day> (interval)"
  , pattern =
    [ regex "entre"
    , Predicate isATimeOfDay
    , regex "et"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntersectByDeOr :: Rule
ruleIntersectByDeOr = Rule
  { name = "intersect by 'de' or ','"
  , pattern =
    [ Predicate isNotLatent
    , regex "de|,"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleDeuximeQuinzaineDeNamedmonthinterval :: Rule
ruleDeuximeQuinzaineDeNamedmonthinterval = Rule
  { name = "deuxième quinzaine de <named-month>(interval)"
  , pattern =
    [ regex "(deuxi(è|e)me|2 ?(è|e)me) (quinzaine|15 ?aine) d[e']"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> do
        from <- intersect (dayOfMonth 15) td
        let to = cycleLastOf TG.Day td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

rulePartofdayDuDimTime :: Rule
rulePartofdayDuDimTime = Rule
  { name = "<part-of-day> du <dim time>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "du"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleYyyymmdd :: Rule
ruleYyyymmdd = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})-(1[0-2]|0?[1-9])-(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m1
        m <- parseInt m2
        d <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTimeofdayHeures :: Rule
ruleTimeofdayHeures = Rule
  { name = "<time-of-day> heures"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "h\\.?(eure)?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleDernierDayofweekDeTimeLatent :: Rule
ruleDernierDayofweekDeTimeLatent = Rule
  { name = "dernier <day-of-week> de <time> (latent)"
  , pattern =
    [ regex "derni(e|é|è)re?"
    , Predicate isADayOfWeek
    , regex "d['e]"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleCeDayofweek :: Rule
ruleCeDayofweek = Rule
  { name = "ce <day-of-week>"
  , pattern =
    [ regex "ce"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleMatin :: Rule
ruleMatin = Rule
  { name = "matin"
  , pattern =
    [ regex "mat(in(é|e)?e?)?"
    ]
  , prod = \_ -> Token Time . mkLatent . partOfDay <$>
      interval TTime.Open (hour False 4) (hour False 12)
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

ruleSeason2 :: Rule
ruleSeason2 = Rule
  { name = "season"
  , pattern =
    [ regex "(cet )?automne"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (monthDay 9 23) (monthDay 12 21)
  }

ruleVersTimeofday :: Rule
ruleVersTimeofday = Rule
  { name = "à|vers <time-of-day>"
  , pattern =
    [ regex "(vers|autour de|(a|à) environ|aux alentours de|(a|à))"
    , Predicate $ and . sequence [isATimeOfDay, isNotLatent]
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleFinNamedmonthinterval :: Rule
ruleFinNamedmonthinterval = Rule
  { name = "fin <named-month>(interval)"
  , pattern =
    [ regex "fin( du mois d[e']? ?)?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> do
        from <- intersect (dayOfMonth 21) td
        let to = cycleLastOf TG.Day td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
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

ruleNameddayEnQuinze :: Rule
ruleNameddayEnQuinze = Rule
  { name = "<named-day> en quinze"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "en (quinze|15)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 2 False td
      _ -> Nothing
  }

ruleLeDayofmonthNonOrdinal :: Rule
ruleLeDayofmonthNonOrdinal = Rule
  { name = "le <day-of-month> (non ordinal)"
  , pattern =
    [ regex "le"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt $ dayOfMonth n
      _ -> Nothing
  }

ruleIntersectByMaisparExempleplutt :: Rule
ruleIntersectByMaisparExempleplutt = Rule
  { name = "intersect by 'mais/par exemple/plutôt'"
  , pattern =
    [ Predicate isNotLatent
    , regex "mais|par exemple|plut(ô|o)t"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleLeCycleAprssuivantTime :: Rule
ruleLeCycleAprssuivantTime = Rule
  { name = "le <cycle> après|suivant <time>"
  , pattern =
    [ regex "l[ea']?"
    , dimension TimeGrain
    , regex "suivante?|apr(e|è|é)s"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleAprsdemain :: Rule
ruleAprsdemain = Rule
  { name = "après-demain"
  , pattern =
    [ regex "apr(e|è)s[- ]?demain"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleIlYADuration :: Rule
ruleIlYADuration = Rule
  { name = "il y a <duration>"
  , pattern =
    [ regex "il y a"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleDudansLePartofday :: Rule
ruleDudansLePartofday = Rule
  { name = "du|dans le <part-of-day>"
  , pattern =
    [ regex "du|dans l[ae']? ?|au|en|l[ae' ]|d(è|e)s l?[ae']?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleAuDjeuner :: Rule
ruleAuDjeuner = Rule
  { name = "au déjeuner"
  , pattern =
    [ regex "((à|a) l'heure du|au moment de|pendant( le)?|au)? d(e|é|è)jeuner"
    ]
  , prod = \_ -> Token Time . partOfDay <$>
      interval TTime.Open (hour False 12) (hour False 14)
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

ruleMaintenant :: Rule
ruleMaintenant = Rule
  { name = "maintenant"
  , pattern =
    [ regex "maintenant|tout de suite"
    ]
  , prod = \_ -> tt now
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd/-mm/-yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/-](1[0-2]|0?[1-9])[-/](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m3
        m <- parseInt m2
        d <- parseInt m1
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleLeCycleAvantprcdentTime :: Rule
ruleLeCycleAvantprcdentTime = Rule
  { name = "le <cycle> avant|précédent <time>"
  , pattern =
    [ regex "l[ea']?"
    , dimension TimeGrain
    , regex "avant|pr(é|e)c(é|e)dent"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleDayOfMonthPremier :: Rule
ruleDayOfMonthPremier = Rule
  { name = "day of month (premier)"
  , pattern =
    [ regex "premier|prem\\.?|1 ?er"
    ]
  , prod = \_ -> tt $ dayOfMonth 1
  }

ruleTimeofdayTimeofdayInterval :: Rule
ruleTimeofdayTimeofdayInterval = Rule
  { name = "<time-of-day> - <time-of-day> (interval)"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "\\-|(jusqu')?(au?|à)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleLeVeilleDuTime :: Rule
ruleLeVeilleDuTime = Rule
  { name = "le veille du <time>"
  , pattern =
    [ regex "(la )?veille du"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ cycleNthAfter True TG.Day (-1) td
      _ -> Nothing
  }

ruleLeTime :: Rule
ruleLeTime = Rule
  { name = "le <time>"
  , pattern =
    [ regex "l[ea]"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleSoirDeNol :: Rule
ruleSoirDeNol = Rule
  { name = "soir de noël"
  , pattern =
    [ regex "soir(ée)? de no(e|ë)l"
    ]
  , prod = \_ -> do
      from <- intersect (monthDay 12 24) (hour False 18)
      to <- intersect (monthDay 12 25) (hour False 0)
      Token Time <$> interval TTime.Open from to
  }

ruleDayofweekDayofmonthTimeofday :: Rule
ruleDayofweekDayofmonthTimeofday = Rule
  { name = "<day-of-week> <day-of-month> à <time-of-day>)"
  , pattern =
    [ Predicate isADayOfWeek
    , Predicate isDOMInteger
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:token:Token Time td2:_) -> do
        dom <- intersectDOM td1 token
        Token Time <$> intersect dom td2
      _ -> Nothing
  }

ruleJourDeLan :: Rule
ruleJourDeLan = Rule
  { name = "jour de l'an"
  , pattern =
    [ regex "(jour de l'|nouvel )an"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleMinuit :: Rule
ruleMinuit = Rule
  { name = "minuit"
  , pattern =
    [ regex "minuit"
    ]
  , prod = \_ -> tt $ hour False 0
  }

ruleNCycleSuivants :: Rule
ruleNCycleSuivants = Rule
  { name = "n <cycle> suivants"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "prochaine?s?|suivante?s?|apr(e|è|é)s|qui sui(t|ves?)|plus tard"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain n
      _ -> Nothing
  }

ruleMidMonth :: Rule
ruleMidMonth = Rule
  { name = "<named-month>"
  , pattern =
    [ regex "mi[- ]"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> do
        from <- intersect (dayOfMonth 10) td
        to <- intersect (dayOfMonth 19) td
        Token Time <$> interval TTime.Open from to
      _ -> Nothing
  }

ruleDayofweekErdayofweekDdMonthinterval :: Rule
ruleDayofweekErdayofweekDdMonthinterval = Rule
  { name = "<day-of-week> 1er-<day-of-week> dd <month>(interval)"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "premier|prem\\.?|1er|1 er"
    , regex "\\-|au|jusqu'au"
    , Predicate isADayOfWeek
    , regex "(3[01]|[12]\\d|0?[1-9])"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:_:_:_:Token RegexMatch (GroupMatch (m:_)):Token Time td:_) -> do
        n <- parseInt m
        from <- intersect (dayOfMonth 1) td
        to <- intersect (dayOfMonth n) td
        Token Time <$> interval TTime.Closed from to
      _ -> Nothing
  }

ruleFinDuMois :: Rule
ruleFinDuMois = Rule
  { name = "fin du mois"
  , pattern =
    [ regex "(en |((à|a) la ))?fin (du|de) (ce )?mois"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (dayOfMonth 21) (dayOfMonth 0)
  }

ruleDbutDuMois :: Rule
ruleDbutDuMois = Rule
  { name = "début du mois"
  , pattern =
    [ regex "(en |au )?d(é|e)but (du|de) (ce )?mois"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (dayOfMonth 1) (dayOfMonth 10)
  }

ruleFinDAnnee :: Rule
ruleFinDAnnee = Rule
  { name = "fin d'année"
  , pattern =
    [ regex "(en |(à|a) la )?fin (d'|de l'|de cette )ann(é|e)e"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (month 11) (month 1)
  }

ruleDbutDAnnee :: Rule
ruleDbutDAnnee = Rule
  { name = "début d'année"
  , pattern =
    [ regex "(en |au )?d(é|e)but (d'|de l'|de cette )ann(é|e)e"
    ]
  , prod = \_ -> Token Time <$>
      interval TTime.Open (month 1) (month 3)
  }

rulePlusTard :: Rule
rulePlusTard = Rule
  { name = "plus tard"
  , pattern =
    [ regex "(un peu )?plus tard"
    ]
  , prod = \_ -> tt $ withDirection TTime.After $ cycleNth TG.Minute 10

  }

rulePlusTardPartofday :: Rule
rulePlusTardPartofday = Rule
  { name = "plus tard <part-of-day>"
  , pattern =
    [ regex "(un peu )?plus tard"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
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

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Janvier"   , "janvier|janv\\.?"                    )
  , ( "Fevrier"   , "f(é|e)vrier|f(é|e)v\\.?"   )
  , ( "Mars"      , "mars|mar\\.?"                        )
  , ( "Avril"     , "avril|avr\\.?"                       )
  , ( "Mai"       , "mai"                                 )
  , ( "Juin"      , "juin|jun\\.?"                        )
  , ( "Juillet"   , "juillet|juil?\\."                    )
  , ( "Aout"      , "ao(û|u)t|aou\\.?"               )
  , ( "Septembre" , "septembre|sept?\\.?"                 )
  , ( "Octobre"   , "octobre|oct\\.?"                     )
  , ( "Novembre"  , "novembre|nov\\.?"                    )
  , ( "Decembre"  ,  "d(é|e)cembre|d(é|e)c\\.?" )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Lundi"    , "lun\\.?(di)?"    )
  , ( "Mardi"    , "mar\\.?(di)?"    )
  , ( "Mercredi" , "mer\\.?(credi)?" )
  , ( "Jeudi"    , "jeu\\.?(di)?"    )
  , ( "Vendredi" , "ven\\.?(dredi)?" )
  , ( "Samedi"   , "sam\\.?(edi)?"   )
  , ( "Dimanche" , "dim\\.?(anche)?" )
  ]

rules :: [Rule]
rules =
  [ ruleAprsLeDayofmonth
  , ruleAprsLeDjeuner
  , ruleAprsLeTravail
  , ruleAprsTimeofday
  , ruleAprsdemain
  , ruleAprsmidi
  , ruleAuDjeuner
  , ruleAujourdhui
  , ruleAvantLeDjeuner
  , ruleAvantTimeofday
  , ruleAvanthier
  , ruleCeDayofweek
  , ruleCePartofday
  , ruleCeTime
  , ruleCedansLeCycle
  , ruleCycleDernier
  , ruleCycleProchainsuivantdaprs
  , ruleDansDuration
  , ruleDatetimeDatetimeInterval
  , ruleDatetimedayofweekDdMonthinterval
  , ruleDatetimeddMonthinterval
  , ruleDayOfMonthPremier
  , ruleDayofmonthNamedmonth
  , ruleDayofweekDayofmonth
  , ruleDayofweekDayofmonthTimeofday
  , ruleMidMonth
  , ruleDayofweekErdayofweekDdMonthinterval
  , ruleDayofweekProchain
  , ruleDbutDaprsmidi
  , ruleDbutDeJourne
  , ruleDbutDeMatine
  , ruleDbutDeSemaine
  , ruleDbutNamedmonthinterval
  , ruleDdMm
  , ruleDdMmYyyy
  , ruleDdddMonthinterval
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleDeDatetimeDatetimeInterval
  , ruleDeTimeofdayTimeofdayInterval
  , ruleDemain
  , ruleDernierCycleDeTimeLatent
  , ruleDernierDayofweekDeTimeLatent
  , ruleDernierWeekendDeTime
  , ruleDeuximeQuinzaineDeNamedmonthinterval
  , ruleDiciDuration
  , ruleDimTimeDuMatin
  , ruleDimTimeDuSoir
  , ruleDimTimePartofday
  , ruleDuDatetimedayofweekDdMonthinterval
  , ruleDuDdAuDdLatentMonthInterval
  , ruleDuDdAuDdMonthInterval
  , ruleDuDddayofweekDdMonthinterval
  , ruleDudansLePartofday
  , ruleDurationApresTime
  , ruleDurationAvantTime
  , ruleEnNamedmonth
  , ruleEnSemaine
  , ruleEntreDatetimeEtDatetimeInterval
  , ruleEntreDdEtDdMonthinterval
  , ruleEntreTimeofdayEtTimeofdayInterval
  , ruleErMai
  , ruleMilieuDaprsmidi
  , ruleFinDaprsmidi
  , ruleMilieuDeJourne
  , ruleFinDeJourne
  , ruleMilieuDeMatine
  , ruleFinDeMatine
  , ruleFinDeSemaine
  , ruleFinNamedmonthinterval
  , ruleHhhmmTimeofday
  , ruleHhmmMilitaryTimeofday
  , ruleHier
  , ruleIlYADuration
  , ruleIntersect
  , ruleIntersectByDeOr
  , ruleIntersectByMaisparExempleplutt
  , ruleJourDeLan
  , ruleLeCycleAprssuivantTime
  , ruleLeCycleAvantprcdentTime
  , ruleLeCycleDeTime
  , ruleLeCycleDernier
  , ruleLeCycleProchainsuivantdaprs
  , ruleLeDayofmonthDatetime
  , ruleLeDayofmonthNonOrdinal
  , ruleLeLendemainDuTime
  , ruleLeOrdinalCycleDeTime
  , ruleLeTime
  , ruleLeVeilleDuTime
  , ruleMaintenant
  , ruleMatin
  , ruleMidi
  , ruleMilieuDeSemaine
  , ruleMinuit
  , ruleNCycleAprs
  , ruleNCycleAvant
  , ruleNCyclePassesprecedents
  , ruleNCycleSuivants
  , ruleNDerniersCycle
  , ruleNProchainsCycle
  , ruleNameddayEnHuit
  , ruleNameddayEnQuinze
  , ruleNamedmonthProchain
  , ruleNamedmonthnameddayDernierpass
  , ruleNamedmonthnameddaySuivantdaprs
  , ruleNoel
  , ruleOrdinalCycleDeTime
  , ruleOrdinalWeekendDeTime
  , rulePartofdayDuDimTime
  , rulePremireQuinzaineDeNamedmonthinterval
  , ruleSeason
  , ruleSeason2
  , ruleSeason3
  , ruleSeason4
  , ruleSoir
  , ruleDbutDeSoire
  , ruleFinDeSoire
  , ruleSoirDeNol
  , ruleTimeofdayHeures
  , ruleTimeofdayLatent
  , ruleTimeofdayTimeofdayInterval
  , ruleToussaint
  , ruleVersTimeofday
  , ruleWeekend
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYyyymmdd
  , ruleHourofdayMoinsQuart
  , ruleHourofdayMoinsIntegerAsRelativeMinutes
  , ruleHourofdayIntegerMinutes
  , ruleHourofdayInteger
  , ruleHourofdayEtpassDeNumeral
  , ruleHourofdayEtpassDeNumeralMinutes
  , ruleHourofdayEtTroisQuart
  , ruleHourofdayEtQuart
  , ruleHourofdayEtDemi
  , ruleFinDuMois
  , ruleTimezone
  , ruleDbutDuMois
  , ruleFinDAnnee
  , ruleDbutDAnnee
  , rulePlusTard
  , rulePlusTardPartofday
  ]
  ++ ruleMonths
  ++ ruleDaysOfWeek
