-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.CA.Corpus
  ( corpus
    , latentCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

context :: Context
context = testContext {locale = makeLocale CA Nothing}

latentCorpus :: Corpus
latentCorpus = (context, testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (datetime (2013, 2, 12, 13, 0, 0) Hour)
                 [ "una hora"
                 ]
      ]

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "ara"
             , "ja"
             , "en aquest moment"
             , "en aquests moments"
             , "ara mateix"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "avui"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "ahir"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "abans d'ahir"
             ]
  {--
    This is intentional
    The purpose is to steer the classifier towards "tomorrow" rule
    instead of "morning" rule.
  --}
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "demà"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "demà passat"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "dilluns"
             , "dl"
             , "dl."
             , "aquest dilluns"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "dilluns, Febrer 18"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "dimarts"
             , "dm"
             , "dm."
             , "dimarts 19"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "dimecres"
             , "dc"
             , "dc."
             , "dimecres 13"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "dijous"
             , "dj"
             , "dj."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "divendres"
             , "dv"
             , "dv."
             , "dv. 15"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "dissabte"
             , "ds"
             , "ds."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "diumenge"
             , "dg"
             , "dg."
             ]
  , examples (datetime (2013, 5, 5, 0, 0, 0) Day)
             [ "el 5 de maig"
             , "el cinc de maig"
             ]
  , examples (datetime (2013, 5, 5, 0, 0, 0) Day)
             [ "el cinc de maig de 2013"
             , "5-5-2013"
             ]
  , examples (datetime (2013, 7, 4, 0, 0, 0) Day)
             [ "el 4 de juliol"
             , "el 4/7"
             ]
   , examples (datetime (2013, 8, 4, 0, 0, 0) Day)
             [ "el 4 d'agost"
             , "el 4/8"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "el 3 de març"
             , "3 de març"
             , "el 3-3"
             ]
   , examples (datetime (2013, 10, 24, 0, 0, 0) Day)
             [ "el 24 d'octubre"
             , "el 24/10"
             ]
  , examples (datetime (2013, 4, 5, 0, 0, 0) Day)
             [ "el 5 d'abril"
             , "5 d'abril"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "el 1 de març"
             , "l'1 de març"
             , "1 de març"
             , "el primer de març"
             , "l'u de març"
             -- , "el primer dia de març" 2 tokens found
             , "u de març"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1-3-2013"
             , "1.3.2013"
             , "1/3/2013"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "el 16"
             , "16 de febrer"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "el 17"
             , "17 de febrer"
             , "17-2"
             , "el 17/2"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "el 20"
             , "20 de febrer"
             , "20/2"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31/10/1974"
             , "31/10/74"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "el dimarts vinent"
             , "dimarts vinent"
             -- , "el proper dimarts" -- Ejemplo no corresponde a la regla
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "dimecres vinent"
             , "el dimecres de la setmana vinent"
             -- , "dimecres de la propera setmana" -- 2 tokens found
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "el dilluns d'aquesta setmana"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "dimarts d'aquesta setmana"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "el dimecres d'aquesta setmana"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "aquesta setmana"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "la setmana passada"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "la setmana vinent"
             , "la propera setmana"
             , "setmana vinent"
             , "propera setmana"
             , "properes setmanes"
             , "següent setmana"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "el passat mes"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "el mes vinent"
             , "el proper mes"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "l'any passat"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "aquest any"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year) 
             [ "l'any vinent"
             , "el proper any"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "el diumenge passat"  -- proppassat?
             , "el diumenge de la setmana passada" -- don't fully match
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "el dimarts passat"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "les 15 hores" -- empty results
             , "les tres"  -- empty results
             , "les 3 pm"  -- empty results
             , "a les tres de la tarda" -- ruleDimTimeDeLaTarde está comentada en Rules.hs
             ]
  -- No encuentro regla asociada a este ejemplo:
  -- , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
  --           [ "a les vuit del vespre"
  --           ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Minute)
             [ "15:00"
             , "15.00"
             , "les tres"
             , "tres"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "mitja nit"
             , "mitjanit"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "migdia"
             , "mig dia"
             -- , "les dotze"
             ]
  -- , examples (datetime (2013, 2, 12, 12, 15, 0) Minute)
  --            [ "un quart d'una" -- dont' fully match
  --            ]
  -- , examples (datetime (2013, 2, 12, 11, 55, 0) Minute) --dubte
  --            [ -- "tres quarts i deu minuts de dotze" -- empty results
  --              -- "tres quarts i deu de dotze" -- empty results
  --              -- "les dotze menys cinc minuts" -- don't fully match
  --              -- "les dotze menys cinc" -- don't fully match
  --              -- "tres quarts i mig passats de dotze" -- empty results
  --            ]
  -- , examples (datetime (2013, 2, 12, 12, 30, 0) Minute)
  --            [ -- "dos quarts d'una" -- empty results
  --             -- "2/4 d'una" -- don't fully match
  --            ]
  -- , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
  --            [ "les tres de la matinada"  -- empty results
  --            ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ -- "un quart de quatre"  -- don't fully match
               -- "a un quart de quatre" -- no prosigue con el test (?)
               -- "1/4 de quatre" -- don't fully match
               -- "un quart de 4" -- don't fully match
               -- "un quart de quatre de la tarda" -- don't fully match
               -- "1 quart de quatre de la tarda" -- don't fully match
                "tres i quart"
              , "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ -- "dos quarts de quatre" -- empty results
               -- "a dos quarts de quatre" -- no prosigue con el test (?)
               -- "a dos quarts de 4 de la tarda" -- no prosigue con el test (?)
               -- "a 2/4 de quatre de la tarda" -- no prosigue con el test (?)
              "15:30"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ -- "tres quarts de 12" -- empty results
               "11:45"
             -- , "tres quarts de dotze" -- empty results
             -- , "avui a tres quarts de 12" -- don't fully match
             -- , "avui a tres quarts de dotze" -- don't fully match
             ]
  -- , examples (datetime (2013, 2, 12, 5, 15, 0) Minute)
  --            [ "un quart de 6" -- don't fully match
  --            ]
  -- , examples (datetime (2013, 2, 12, 6, 0, 0) Hour)
  --            [ "6 del matí" -- empty results
  --            ]
  -- , examples (datetime (2013, 2, 13, 11, 0, 0) Hour)
  --            [ "dimecres a les onze del matí" -- don't fully match
  --            ]
  -- , examples (datetime (2013, 2, 13, 11, 0, 0) Hour)
  --            [ -- "demà a les onze" -- dont' fully match
  --              "demà a les 11" -- dont' fully match
  --            ]
  -- , examples (datetime (2014, 9, 12, 0, 0, 0) Day)
  --           [ "divedres, 12 de setembre de 2014" -- don't fully match
  --           ]
  -- , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
  --           [ "en un segon"  -- empty results
  --           ]
  -- , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
  --            [ -- "en un minut" -- empty results
  --              -- "en 1 min" -- empty results
  --            ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "en 2 minuts"
             , "en dos minuts"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "en 60 minuts"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "en una hora"
             ]
  , examples (datetime (2013, 2, 12, 2, 30, 0) Minute)
             [ "fa dues hores"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "en 24 hores"
             , "en vint-i-quatre hores"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "en un dia"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "en 7 dies"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "en una setmana"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "fa tres setmanas"
             ]
  , examples (datetime (2013, 4, 12, 0, 0, 0) Day)
             [ "en dos mesos"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "fa tres mesos"
             ]
  , examples (datetime (2014, 2, 0, 0, 0, 0) Month)
             [ "en un any"
             , "en 1 any"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "fa dos anys"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "aquest estiu"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "aquest hivern"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "Nadal"
             , "el Nadal"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "nit de cap d'any"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "dia de cap d'any"
             , "cap d'any"
             ]
  , examples (datetime (2013, 2, 12, 21, 0, 0) Hour)
             [ "nou del vespre"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 0, 0), (2013, 2, 12, 19, 59, 0)) Hour)
             [ "aquesta tarda"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 15, 0, 0), (2013, 2, 13, 19, 59, 0)) Hour)
             [ "demà a la tarda"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 15, 0, 0), (2013, 2, 11, 19, 59, 0)) Hour)
             [ "ahir per la tarda"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 20, 0, 0), (2013, 2, 12, 22, 59, 0)) Hour)
             [ "aquest vespre"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 20, 0, 0), (2013, 2, 11, 22, 59, 0)) Hour)
             [ "demà al vespre"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 20, 0, 0), (2013, 2, 13, 22, 59, 0)) Hour)
             [ "ahir al vespre"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 23, 0, 0), (2013, 2, 13, 0, 59, 0)) Hour)
             [ "aquesta nit"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 23, 0, 0), (2013, 2, 14, 0, 59, 0)) Hour)
             [ "demà a la nit"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 23, 0, 0), (2013, 2, 12, 0, 59, 0)) Hour)
             [ "ahir per la nit"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "aquest weekend"
             , "aquest cap de setmana"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 6, 0, 0), (2013, 2, 18, 11, 59, 0)) Hour)
             [ "dilluns pel matí"
             , "el matí de dilluns"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 1, 0, 0), (2013, 2, 18, 5, 59, 0)) Hour)
             [ "dilluns a la matínada"
             , "la matinada de dilluns"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 6, 0, 0), (2013, 2, 15, 11, 59, 0)) Hour)
             [ "el 15 de febrer pel matí"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "a les 8 del vespre"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "darrers 2 segons" -- texte original pasados 2 segundos
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "propers 3 segons"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "darrers 2 minuts"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "propers 3 minuts"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "properes 3 hores"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "darrers 2 dies"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "propers 3 dies"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "darreres dues setmanes"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "3 properes setmanes"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "darrers 2 mesos"
             , "dos últims mesos"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "3 propers mesos"
             , "propers tres mesos"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "darrers 2 anys"
             , "dos últims anys"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "3 propers anys"
             , "propers tres amys"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13 a 15 de juliol"
             , "13 - 15 de juliol de 2013"
             , "del 13 al 15 de juliol"
             , "des del 13 fins al 15 de juliol"
             ]
  , examples (datetimeInterval ((2013, 7, 1, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "1 a 15 de juliol"
             , "1 - 15 de juliol de 2013"
             , "de l'1 al 15 de juliol"
             , "des de l'1 fins al 15 de juliol"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 0, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 12, 21, 0, 0, 0), (2014, 1, 7, 0, 0, 0)) Day)
             [ "21 de des. al 6 de gen"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 12, 7, 30, 0)) Second)
             [ "dintre de tres hores"
             ]
  , examples (datetime (2013, 2, 12, 16, 0, 0) Hour)
             [ "a les quatre de la tarda"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "a les quatre CET"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "dj 15"
             , "dj. 15"
             , "dijous 15"
             ]
  , examples (datetimeHoliday (2013, 12, 18, 0, 0, 0) Day "Día Mundial de la Lengua Árabe")
             [ "dia mundial de la llegua àrab"
             ]
  , examples (datetimeHoliday (2013, 3, 1, 0, 0, 0) Day "Día de la Cero Discriminación")
             [ "dia de la zero discriminació"
             ]
  , examples (datetimeHoliday (2019, 7, 6, 0, 0, 0) Day "Día Internacional de las Cooperativas")
             [ "dia internacional de les cooperatives del 2019"
             ]
  , examples (datetimeHoliday (2013, 11, 17, 0, 0, 0) Day "Día de la Prematuridad Mundial")
             [ "dia de la prematuritat mundial"
             , "dia mundial del prematur"
             , "dia mundial del nen prematur"
             ]
  , examples (datetimeHoliday (2013, 4, 1, 0, 0, 0) Day "Día de los Inocentes de Abril")
             [ "dia dels innocents d'abril"
             ]
  , examples (datetime (2013, 3, 9, 0, 0, 0) Day)
             [ "dia nou"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "dia quinze"
             ]
  , examples (datetime (2013, 3, 11, 0, 0, 0) Day)
             [ "dia onze"
             ]
  , examples (datetime (2013, 2, 12, 18, 2, 0) Minute)
             [
               "les sis cero dos pm"
             , "a les sis zero dos pm"
             , "a les 6 0 2 pm"
             , "sis zero dos pm"
             ]
  , examples (datetime (2013, 2, 12, 18, 2, 0) Minute)
             [ "les sis i dos de la tarde"
             ]
  , examples (datetime (1990, 0, 0, 0, 0, 0) Year)
             [
               "mil nou-cents noranta"
             ]
  , examples (datetime (1990, 5, 4, 0, 0, 0) Day)
             [
               "quatre de maig de mil nou-cents noranta"
             ]
  , examples (datetimeInterval ((2013, 4, 1, 0, 0, 0), (2013, 7, 1, 0, 0, 0)) Second)
             [ "segon trimestre de 2013"
             ]
  ]
