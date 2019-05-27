-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ES.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {locale = makeLocale ES Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "ahora"
             , "ya"
             , "ahorita"
             , "cuanto antes"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "hoy"
             , "en este momento"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "ayer"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "anteayer"
             , "antier"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "mañana"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "pasado mañana"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "lunes"
             , "lu"
             , "lun."
             , "este lunes"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "lunes, 18 de febrero"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "martes"
             , "ma"
             , "ma."
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "miercoles"
             , "miércoles"
             , "mx"
             , "mié."
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "jueves"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "viernes"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "sabado"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "domingo"
             ]
  , examples (datetime (2013, 5, 5, 0, 0, 0) Day)
             [ "el 5 de mayo"
             , "el cinco de mayo"
             ]
  , examples (datetime (2013, 5, 5, 0, 0, 0) Day)
             [ "el cinco de mayo de 2013"
             , "mayo 5 del 2013"
             , "5-5-2013"
             ]
  , examples (datetime (2013, 7, 4, 0, 0, 0) Day)
             [ "el 4 de julio"
             , "el 4/7"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "el 3 de marzo"
             , "3 de marzo"
             , "el 3-3"
             ]
  , examples (datetime (2013, 4, 5, 0, 0, 0) Day)
             [ "el 5 de abril"
             , "5 de abril"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "el 1 de marzo"
             , "1 de marzo"
             , "el primero de marzo"
             , "el uno de marzo"
             , "primero de marzo"
             , "uno de marzo"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1-3-2013"
             , "1.3.2013"
             , "1/3/2013"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "el 16"
             , "16 de febrero"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "el 17"
             , "17 de febrero"
             , "17-2"
             , "el 17/2"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "el 20"
             , "20 de febrero"
             , "20/2"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31/10/1974"
             , "31/10/74"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "el martes que viene"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "miércoles que viene"
             , "el miércoles de la semana que viene"
             , "miercoles de la próxima semana"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "el lunes de esta semana"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "martes de esta semana"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "el miércoles de esta semana"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "esta semana"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "la semana pasada"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "la semana que viene"
             , "la proxima semana"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "el pasado mes"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "el mes que viene"
             , "el proximo mes"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "el año pasado"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "este ano"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "el año que viene"
             , "el proximo ano"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "el domingo pasado"
             , "el domingo de la semana pasada"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "el martes pasado"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "a las tres de la tarde"
             , "a las tres"
             , "a las 3 pm"
             , "a las 15 horas"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "a las ocho de la tarde"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Minute)
             [ "15:00"
             , "15.00"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "medianoche"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "mediodía"
             , "las doce"
             ]
  , examples (datetime (2013, 2, 12, 12, 15, 0) Minute)
             [ "las doce y cuarto"
             ]
  , examples (datetime (2013, 2, 12, 11, 55, 0) Minute)
             [ "las doce menos cinco"
             ]
  , examples (datetime (2013, 2, 12, 12, 30, 0) Minute)
             [ "las doce y media"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "las tres de la manana"
             , "las tres en la manana"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "a las tres y quince"
             , "a las 3 y cuarto"
             , "a las tres y cuarto de la tarde"
             , "a las tres y cuarto en la tarde"
             , "15:15"
             , "15.15"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "a las tres y media"
             , "a las 3 y treinta"
             , "a las tres y media de la tarde"
             , "a las 3 y treinta del mediodía"
             , "15:30"
             , "15.30"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "las doce menos cuarto"
             , "11:45"
             , "las once y cuarenta y cinco"
             , "hoy a las doce menos cuarto"
             , "hoy a las once y cuarenta y cinco"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Minute)
             [ "5 y cuarto"
             ]
  , examples (datetime (2013, 2, 12, 6, 0, 0) Hour)
             [ "6 de la mañana"
             ]
  , examples (datetime (2013, 2, 13, 11, 0, 0) Hour)
             [ "miércoles a las once de la mañana"
             ]
  , examples (datetime (2013, 2, 13, 11, 0, 0) Hour)
             [ "mañana a las once"
             , "mañana a 11"
             ]
  , examples (datetime (2014, 9, 12, 0, 0, 0) Day)
             [ "viernes, el 12 de septiembre de 2014"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "en un segundo"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "en un minuto"
             , "en 1 min"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "en 2 minutos"
             , "en dos minutos"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "en 60 minutos"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "en una hora"
             ]
  , examples (datetime (2013, 2, 12, 2, 30, 0) Minute)
             [ "hace dos horas"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "en 24 horas"
             , "en veinticuatro horas"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "en un dia"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "en 7 dias"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "en una semana"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "hace tres semanas"
             ]
  , examples (datetime (2013, 4, 12, 0, 0, 0) Day)
             [ "en dos meses"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "hace tres meses"
             ]
  , examples (datetime (2014, 2, 0, 0, 0, 0) Month)
             [ "en un ano"
             , "en 1 año"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "hace dos años"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "este verano"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "este invierno"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "Navidad"
             , "la Navidad"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "Nochevieja"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "ano nuevo"
             , "año nuevo"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "esta noche"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "mañana por la noche"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "ayer por la noche"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "este weekend"
             , "este fin de semana"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "lunes por la mañana"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "el 15 de febrero por la mañana"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "a las 8 de la tarde"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "pasados 2 segundos"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "proximos 3 segundos"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "pasados 2 minutos"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "proximos 3 minutos"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "proximas 3 horas"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "pasados 2 dias"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "proximos 3 dias"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "pasadas dos semanas"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "3 proximas semanas"
             , "3 semanas que vienen"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "pasados 2 meses"
             , "dos pasados meses"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "3 próximos meses"
             , "proximos tres meses"
             , "tres meses que vienen"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "pasados 2 anos"
             , "dos pasados años"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "3 próximos años"
             , "proximo tres años"
             , "3 años que vienen"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13 a 15 de julio"
             , "13 - 15 de julio de 2013"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 0, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 12, 21, 0, 0, 0), (2014, 1, 7, 0, 0, 0)) Day)
             [ "21 de Dic. a 6 de Ene"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 12, 7, 30, 0)) Second)
             [ "dentro de tres horas"
             ]
  , examples (datetime (2013, 2, 12, 16, 0, 0) Hour)
             [ "a las cuatro de la tarde"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "a las cuatro CET"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "jue 15"
             ]
  ]
