-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.CA.Corpus
  ( corpus
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
             , "dema"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "demà passat"
             , "dema passat"
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
   , examples (datetime (2013, 09, 24, 0, 0, 0) Day)
             [ "el 24 de setembre"
             , "el 24 de set"
             , "el 24/09"
             ]
   , examples (datetime (2013, 09, 0, 0, 0, 0) Month)
             [ "setembre"
             , "set"
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
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "dimecres vinent"
             , "el dimecres de la setmana vinent"
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
             [ "el diumenge passat"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "el dimarts passat"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Minute)
             [ "15:00"
             , "15.00"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "mitjanit"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "migdia"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "tres i quart"
             , "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "15:30"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "11:45"
             ]
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
             [ "fa tres setmanes"
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
  , examples (datetimeHoliday (2013, 12, 25, 0, 0, 0) Day "Navidad")
             [ "Nadal"
             , "el Nadal"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "nit de cap d'any"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "cap d'any"
             ]
  , examples (datetime (2013, 2, 12, 21, 0, 0) Hour)
             [ "nou del vespre"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 14, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "aquesta tarda"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 14, 0, 0), (2013, 2, 13, 19, 0, 0)) Hour)
             [ "demà a la tarda"
             , "dema a la tarda"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 14, 0, 0), (2013, 2, 11, 19, 0, 0)) Hour)
             [ "ahir per la tarda"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 19, 0, 0), (2013, 2, 12, 22, 0, 0)) Hour)
             [ "aquest vespre"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 19, 0, 0), (2013, 2, 13, 22, 0, 0)) Hour)
             [ "demà al vespre"
             , "dema al vespre"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 19, 0, 0), (2013, 2, 11, 22, 0, 0)) Hour)
             [ "ahir al vespre"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 0, 0, 0), (2013, 2, 12, 1, 0, 0)) Hour)
             [ "aquesta nit"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 13, 1, 0, 0)) Hour)
             [ "demà a la nit"
             , "dema a la nit"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 11, 1, 0, 0)) Hour)
             [ "ahir per la nit"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "aquest cap de setmana"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 6, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "dilluns pel matí"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 6, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "el 15 de febrer pel matí"
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
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "3 propers mesos"
             , "propers tres mesos"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "darrers 2 anys"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "3 propers anys"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13 - 15 de juliol de 2013"
             ]
  , examples (datetimeInterval ((2013, 7, 1, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "1 - 15 de juliol de 2013"
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
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "dj 15"
             , "dj. 15"
             , "dijous 15"
             ]
  , examples (datetimeHoliday (2013, 12, 18, 0, 0, 0) Day "Dia Mundial de la Lengua Àrab")
             [ "dia mundial de la lengua árabe"
             ]
  , examples (datetimeHoliday (2013, 3, 1, 0, 0, 0) Day "Dia de la Zero Discriminació")
             [ "dia de la zero discriminació"
             ]
  , examples (datetimeHoliday (2019, 7, 6, 0, 0, 0) Day "Dia Internacional de les Cooperatives")
             [ "dia internacional de les cooperatives del 2019"
             ]
  , examples (datetimeHoliday (2013, 11, 17, 0, 0, 0) Day "Dia de la Prematuridad Mundial")
             [ "dia de la prematuritat mundial"
             , "dia mundial de l'infant prematur"
             ]
  , examples (datetimeHoliday (2013, 4, 1, 0, 0, 0) Day "Dia dels Innocents d'Abril")
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
  , examples (datetime (1990, 0, 0, 0, 0, 0) Year)
             [ "mil nou-cents noranta"
             ]
  , examples (datetime (1990, 5, 4, 0, 0, 0) Day)
             [ "quatre de maig de mil nou-cents noranta"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "segon trimestre de 2013"
             ]
  ]
