-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.GA.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import qualified Duckling.Ordinal.Types as TOrdinal
import Duckling.Regex.Types
import Duckling.Time.Helpers
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

ruleArInn :: Rule
ruleArInn = Rule
  { name = "arú inné"
  , pattern =
    [ regex "ar(ú|u) inn(é|e)"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
  }

ruleNollaigNaMban :: Rule
ruleNollaigNaMban = Rule
  { name = "Nollaig na mBan"
  , pattern =
    [ regex "(l(á|a) |an )?nollaig (bheag|na mban)"
    ]
  , prod = \_ -> tt $ monthDay 1 6
  }

ruleInniu :: Rule
ruleInniu = Rule
  { name = "inniu"
  , pattern =
    [ regex "inniu"
    ]
  , prod = \_ -> tt today
  }

ruleAnOrdinalCycleINdiaidhTime :: Rule
ruleAnOrdinalCycleINdiaidhTime = Rule
  { name = "an <ordinal> <cycle> i ndiaidh <time>"
  , pattern =
    [ regex "an"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "(i ndiaidh|tar (é|e)is)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleInn :: Rule
ruleInn = Rule
  { name = "inné"
  , pattern =
    [ regex "inn(é|e)"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleLFhileBrde :: Rule
ruleLFhileBrde = Rule
  { name = "Lá Fhéile Bríde"
  , pattern =
    [ regex "(l(á|a) )?(fh(e|é)ile|'?le) bh?r(í|i)de"
    ]
  , prod = \_ -> tt $ monthDay 2 1
  }

ruleLFhileVailintn :: Rule
ruleLFhileVailintn = Rule
  { name = "Lá Fhéile Vailintín"
  , pattern =
    [ regex "(l(á|a) )?(fh(e|é)ile|'?le) vailint(í|i)n"
    ]
  , prod = \_ -> tt $ monthDay 2 14
  }

ruleTimeSeo :: Rule
ruleTimeSeo = Rule
  { name = "<time> seo"
  , pattern =
    [ dimension Time
    , regex "seo"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleTimeSeoChaite :: Rule
ruleTimeSeoChaite = Rule
  { name = "<time> seo chaite"
  , pattern =
    [ dimension Time
    , regex "seo ch?aite"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleTimeSeoChugainn :: Rule
ruleTimeSeoChugainn = Rule
  { name = "<time> seo chugainn"
  , pattern =
    [ Predicate isNotLatent
    , regex "seo (chugainn|at(a|á) ag teacht)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleAmrach :: Rule
ruleAmrach = Rule
  { name = "amárach"
  , pattern =
    [ regex "am(á|a)rach"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
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

ruleOrdinalRithe :: Rule
ruleOrdinalRithe = Rule
  { name = "<ordinal> ráithe"
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

ruleAnnaCycleRoimhTime :: Rule
ruleAnnaCycleRoimhTime = Rule
  { name = "(an|na) <cycle> roimh <time>"
  , pattern =
    [ regex "the"
    , dimension TimeGrain
    , regex "roimh"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleCycleShin :: Rule
ruleCycleShin = Rule
  { name = "<cycle> ó shin"
  , pattern =
    [ dimension TimeGrain
    , regex "(ó|o) shin"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) ->
        tt . cycleNth grain $ 1
      _ -> Nothing
  }

ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd/mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])/(0?[1-9]|1[0-2])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        m <- parseInt m2
        d <- parseInt m1
        tt $ monthDay m d
      _ -> Nothing
  }

ruleIGceannCycle :: Rule
ruleIGceannCycle = Rule
  { name = "i gceann <cycle>"
  , pattern =
    [ regex "(i|faoi) g?ch?eann"
    , Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token TimeGrain grain:_) ->
        tt $ cycleN True grain (TOrdinal.value od)
      _ -> Nothing
  }

ruleAnCycleDeTime :: Rule
ruleAnCycleDeTime = Rule
  { name = "an <cycle> de <time>"
  , pattern =
    [ regex "an"
    , dimension TimeGrain
    , regex "d[e']"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain 0 td
      _ -> Nothing
  }

ruleDayofmonthordinalNamedmonth :: Rule
ruleDayofmonthordinalNamedmonth = Rule
  { name = "<day-of-month>(ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
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

ruleOrdinalRitheYear :: Rule
ruleOrdinalRitheYear = Rule
  { name = "<ordinal> ráithe <year>"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:_:Token Time td:_) ->
        tt $ cycleNthAfter False TG.Quarter (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleCycleInniu :: Rule
ruleCycleInniu = Rule
  { name = "<cycle> ó inniu"
  , pattern =
    [ Predicate $ isIntegerBetween 1 9999
    , dimension TimeGrain
    , regex "(ó|o)(n l(á|a) (at(á|a) )?)?inniu"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        v <- getIntValue token
        tt $ cycleN True grain v
      _ -> Nothing
  }

ruleOrdinalCycleINdiaidhTime :: Rule
ruleOrdinalCycleINdiaidhTime = Rule
  { name = "<ordinal> <cycle> i ndiaidh <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "(i ndiaidh|tar (é|e)is)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleAnDayofmonthOrdinal :: Rule
ruleAnDayofmonthOrdinal = Rule
  { name = "an <day-of-month> (ordinal)"
  , pattern =
    [ regex "an|na"
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt $ dayOfMonth n
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

ruleAnOrdinalCycleDeTime :: Rule
ruleAnOrdinalCycleDeTime = Rule
  { name = "an <ordinal> <cycle> de <time>"
  , pattern =
    [ regex "an"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "d[e']"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
      _ -> Nothing
  }

ruleLNaNaithreacha :: Rule
ruleLNaNaithreacha = Rule
  { name = "Lá na nAithreacha"
  , pattern =
    [ regex "l(á|a) na naithreacha"
    ]
  , prod = \_ -> tt $ nthDOWOfMonth 2 7 6
  }

ruleArAmrach :: Rule
ruleArAmrach = Rule
  { name = "arú amárach"
  , pattern =
    [ regex "ar(ú|u) am(á|a)rach"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleOrdinalCycleDeTime :: Rule
ruleOrdinalCycleDeTime = Rule
  { name = "<ordinal> <cycle> de <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "d[e']"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain (TOrdinal.value od - 1) td
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

ruleArDate :: Rule
ruleArDate = Rule
  { name = "ar <date>"
  , pattern =
    [ regex "ar"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleAnNollaig :: Rule
ruleAnNollaig = Rule
  { name = "An Nollaig"
  , pattern =
    [ regex "(l(á|a) |an )?(nollai?g)"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleOnANamedday :: Rule
ruleOnANamedday = Rule
  { name = "on a named-day"
  , pattern =
    [ regex "ar an"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
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

ruleAnois :: Rule
ruleAnois = Rule
  { name = "anois"
  , pattern =
    [ regex "anois|(ag an (t-?)?am seo)"
    ]
  , prod = \_ -> tt now
  }

ruleLFhilePdraig :: Rule
ruleLFhilePdraig = Rule
  { name = "Lá Fhéile Pádraig"
  , pattern =
    [ regex "(l(á|a) )?(fh(e|é)ile|'?le) ph?(á|a)draig"
    ]
  , prod = \_ -> tt $ monthDay 3 17
  }

ruleAnCycleINdiaidhTime :: Rule
ruleAnCycleINdiaidhTime = Rule
  { name = "an <cycle> i ndiaidh <time>"
  , pattern =
    [ regex "the"
    , dimension TimeGrain
    , regex "(i ndiaidh|tar (é|e)is)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleDayofmonthOrdinal :: Rule
ruleDayofmonthOrdinal = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern =
    [ Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ dayOfMonth n
      _ -> Nothing
  }

ruleAnCycleSeo :: Rule
ruleAnCycleSeo = Rule
  { name = "an <cycle> seo"
  , pattern =
    [ regex "an"
    , dimension TimeGrain
    , regex "seo"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleAnDayofmonthNonOrdinal :: Rule
ruleAnDayofmonthNonOrdinal = Rule
  { name = "an <day-of-month> (non ordinal)"
  , pattern =
    [ regex "an|na"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ dayOfMonth v
      _ -> Nothing
  }

ruleDNamedday :: Rule
ruleDNamedday = Rule
  { name = "dé named-day"
  , pattern =
    [ regex "d(é|e)"
    , Predicate isADayOfWeek
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

ruleCycleRoimhTime :: Rule
ruleCycleRoimhTime = Rule
  { name = "<cycle> roimh <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "roimhe?"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
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

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd/mm/yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[-/](0?[1-9]|1[0-2])[/-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m3
        m <- parseInt m2
        d <- parseInt m1
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleAnNamedday :: Rule
ruleAnNamedday = Rule
  { name = "an named-day"
  , pattern =
    [ regex "an"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "luai?n|lu\\.?"             )
  , ( "Tuesday"  , "mh?(á|a)irt|m(á|a)?\\.?"   )
  , ( "Wednesday", "ch?(é|e)adaoin|c(é|e)\\.?" )
  , ( "Thursday" , "d(é|e)ardaoin|d(é|e)?\\.?" )
  , ( "Friday"   , "h?aoine|ao\\.?"            )
  , ( "Saturday" , "sathai?rn|sa\\.?"          )
  , ( "Sunday"   , "domhnai?[cg]h|do\\.?"      )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"  , "(mh?(í|i) )?(an )?t?ean(á|a)ir|ean\\.?"     )
  , ( "February" , "(mh?(í|i) )?(na )?feabhra|fea\\.?"          )
  , ( "March"    , "(mh?(í|i) )?(an )?mh?(á|a)rta|m(á|a)r\\.?"  )
  , ( "April"    , "(mh?(í|i) )?(an )?t?aibre(á|a)i?n|abr\\.?"  )
  , ( "May"      , "(mh?(í|i) )?(na )?bh?ealtaine|bea\\.?"      )
  , ( "June"     , "(mh?(í|i) )?(an )?mh?eith(ea|i)mh|mei\\.?"  )
  , ( "July"     , "(mh?(í|i) )?i(ú|u)il|i(ú|u)i\\.?"           )
  , ( "August"   , "(mh?(í|i) )?(na )?l(ú|u)nasa|l(ú|u)n\\.?"   )
  , ( "September", "(mh?(í|i) )?mh?e(á|a)n f(ó|o)mhair|mef?\\.?")
  , ( "October"  , "(mh?(í|i) )?(na )?nollai?g|nol\\.?"         )
  , ( "November" , "(mh?(í|i) )?(na )?samh(ain|na)|sam\\.?"     )
  , ( "December" , "(mh?(í|i) )?(na )?nollai?g|nol\\.?"         )
  ]

ruleCycleINdiaidhTime :: Rule
ruleCycleINdiaidhTime = Rule
  { name = "<cycle> i ndiaidh <time>"
  , pattern =
    [ dimension TimeGrain
    , regex "(i ndiaidh|tar (é|e)is)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleDayofmonthordinalNamedmonthYear :: Rule
ruleDayofmonthordinalNamedmonthYear = Rule
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

rules :: [Rule]
rules =
  [ ruleAbsorptionOfAfterNamedDay
  , ruleAmrach
  , ruleAnCycleDeTime
  , ruleAnCycleINdiaidhTime
  , ruleAnCycleSeo
  , ruleAnDayofmonthNonOrdinal
  , ruleAnDayofmonthOrdinal
  , ruleAnNamedday
  , ruleAnNollaig
  , ruleAnOrdinalCycleDeTime
  , ruleAnOrdinalCycleINdiaidhTime
  , ruleAnnaCycleRoimhTime
  , ruleAnois
  , ruleArAmrach
  , ruleArDate
  , ruleArInn
  , ruleCycleINdiaidhTime
  , ruleCycleInniu
  , ruleCycleRoimhTime
  , ruleCycleShin
  , ruleDNamedday
  , ruleDayofmonthOrdinal
  , ruleDayofmonthordinalNamedmonth
  , ruleDayofmonthordinalNamedmonthYear
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleIGceannCycle
  , ruleInn
  , ruleInniu
  , ruleIntersect
  , ruleIntersectBy
  , ruleLFhileBrde
  , ruleLFhilePdraig
  , ruleLFhileVailintn
  , ruleLNaNaithreacha
  , ruleNollaigNaMban
  , ruleOnANamedday
  , ruleOrdinalCycleDeTime
  , ruleOrdinalCycleINdiaidhTime
  , ruleOrdinalRithe
  , ruleOrdinalRitheYear
  , ruleTimeSeo
  , ruleTimeSeoChaite
  , ruleTimeSeoChugainn
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYyyymmdd
  ]
  ++ ruleDaysOfWeek
  ++ ruleMonths
