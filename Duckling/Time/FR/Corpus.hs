-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.FR.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {locale = makeLocale FR Nothing}, testOptions, examples)
  where
    examples =
      [ "Ana a un court de tennis"
      , "deux trois"
      , "deux trois minutes"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "maintenant"
             , "tout de suite"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "aujourd'hui"
             , "ce jour"
             , "dans la journée"
             , "en ce moment"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "hier"
             , "le jour d'avant"
             , "le jour précédent"
             , "la veille"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "avant-hier"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "demain"
             , "jour suivant"
             , "le jour d'après"
             , "le lendemain"
             , "un jour après"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "après-demain"
             , "le lendemain du 13 février"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "lundi"
             , "lun."
             , "ce lundi"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "lundi 18 février"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "mardi"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "mercredi 13 février"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "jeudi"
             , "deux jours plus tard"
             , "deux jours après"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "vendredi"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "samedi"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "dimanche"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "le 1er mars"
             , "premier mars"
             , "le 1 mars"
             , "vendredi 1er mars"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "le premier mars 2013"
             , "1/3/2013"
             , "2013-03-01"
             ]
  , examples (datetime (2013, 3, 2, 0, 0, 0) Day)
             [ "le 2 mars"
             , "2 mars"
             , "le 2/3"
             ]
  , examples (datetime (2013, 3, 2, 5, 0, 0) Hour)
             [ "le 2 mars à 5h"
             , "2 mars à 5h"
             , "le 2/3 à 5h"
             , "le 2 mars à 5h du matin"
             , "le 2 mars vers 5h"
             , "2 mars vers 5h"
             , "2 mars à environ 5h"
             , "2 mars aux alentours de 5h"
             , "2 mars autour de 5h"
             , "le 2/3 vers 5h"
             ]
  , examples (datetime (2013, 3, 2, 0, 0, 0) Day)
             [ "le 2"
             ]
  , examples (datetime (2013, 3, 2, 5, 0, 0) Hour)
             [ "le 2 à 5h"
             , "le 2 vers 5h"
             , "le 2 à 5h du mat"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "le 3 mars"
             , "3 mars"
             , "le 3/3"
             ]
  , examples (datetime (2013, 4, 5, 0, 0, 0) Day)
             [ "le 5 avril"
             , "5 avril"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "le 3 mars 2015"
             , "3 mars 2015"
             , "3/3/2015"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "le 15 février"
             , "15 février"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15/02/2013"
             , "15 fev 2013"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "le 16"
             ]
  , examples (datetime (2013, 2, 16, 18, 0, 0) Hour)
             [ "le 16 à 18h"
             , "le 16 vers 18h"
             , "le 16 plutôt vers 18h"
             , "le 16 à 6h du soir"
             , "le 16 vers 6h du soir"
             , "le 16 vers 6h dans la soirée"
             , "samedi 16 à 18h"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "17 février"
             , "le 17 février"
             , "17/2"
             , "17/02"
             , "le 17/02"
             , "17 02"
             , "17 2"
             , "le 17 02"
             , "le 17 2"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "mercredi 13"
             ]
  , examples (datetime (2014, 2, 20, 0, 0, 0) Day)
             [ "20/02/2014"
             , "20/2/2014"
             , "20/02/14"
             , "le 20/02/14"
             , "le 20/2/14"
             , "20 02 2014"
             , "20 02 14"
             , "20 2 2014"
             , "20 2 14"
             , "le 20 02 2014"
             , "le 20 02 14"
             , "le 20 2 2014"
             , "le 20 2 14"
             ]
  , examples (datetime (2013, 10, 31, 0, 0, 0) Day)
             [ "31 octobre"
             , "le 31 octobre"
             , "31/10"
             , "le 31/10"
             , "31 10"
             , "le 31 10"
             ]
  , examples (datetime (2014, 12, 24, 0, 0, 0) Day)
             [ "24/12/2014"
             , "24/12/14"
             , "le 24/12/14"
             , "24 12 2014"
             , "24 12 14"
             , "le 24 12 2014"
             , "le 24 12 14"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31/10/1974"
             , "31/10/74"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "lundi prochain"
             , "lundi la semaine prochaine"
             , "lundi de la semaine prochaine"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "mardi prochain"
             , "mardi suivant"
             , "mardi d'après"
             , "mardi la semaine prochaine"
             , "mardi de la semaine prochaine"
             , "mardi la semaine suivante"
             , "mardi de la semaine suivante"
             , "mardi la semaine d'après"
             , "mardi de la semaine d'après"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "mercredi prochain"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "mercredi suivant"
             , "mercredi d'après"
             , "mercredi la semaine prochaine"
             , "mercredi de la semaine prochaine"
             , "mercredi la semaine suivante"
             , "mercredi de la semaine suivante"
             , "mercredi la semaine d'après"
             , "mercredi de la semaine d'après"
             ]
  , examples (datetime (2013, 2, 25, 0, 0, 0) Day)
             [ "lundi en huit"
             , "lundi en 8"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "mardi en huit"
             , "mardi en 8"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "mercredi en huit"
             , "mercredi en 8"
             ]
  , examples (datetime (2013, 3, 4, 0, 0, 0) Day)
             [ "lundi en quinze"
             , "lundi en 15"
             ]
  , examples (datetime (2013, 2, 26, 0, 0, 0) Day)
             [ "mardi en quinze"
             , "mardi en 15"
             ]
  , examples (datetime (2013, 2, 27, 0, 0, 0) Day)
             [ "mercredi en quinze"
             , "mercredi en 15"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "lundi cette semaine"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "mardi cette semaine"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "mercredi cette semaine"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "cette semaine"
             , "dans la semaine"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "la semaine dernière"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "la semaine prochaine"
             , "la semaine suivante"
             , "la semaine qui suit"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "le mois dernier"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "le mois prochain"
             , "le mois suivant"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "l'année dernière"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "cette année"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "l'année prochaine"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "dimanche dernier"
             , "dimanche de la semaine dernière"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "3eme jour d'octobre"
             , "le 3eme jour d'octobre"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "premiere semaine d'octobre 2014"
             , "la premiere semaine d'octobre 2014"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ "la semaine du 6 octobre"
             , "la semaine du 7 octobre"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "dernier jour d'octobre 2015"
             , "le dernier jour d'octobre 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "dernière semaine de septembre 2014"
             , "la dernière semaine de septembre 2014"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "à quinze heures"
             , "à 15 heures"
             , "à 3 heures cet après-midi"
             , "15h"
             , "15H"
             , "vers 15 heures"
             , "à environ 15 heures"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Minute)
             [ "15:00"
             , "15h00"
             , "15H00"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "minuit"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "midi"
             , "aujourd'hui à midi"
             ]
  , examples (datetime (2013, 2, 12, 12, 15, 0) Minute)
             [ "midi et quart"
             , "midi quinze"
             ]
  , examples (datetime (2013, 2, 12, 11, 55, 0) Minute)
             [ "midi moins cinq"
             ]
  , examples (datetime (2013, 2, 12, 12, 30, 0) Minute)
             [ "midi et demi"
             , "midi trente"
             ]
  , examples (datetime (2013, 2, 13, 0, 3, 0) Minute)
             [ "minuit trois"
             ]
  , examples (datetime (2013, 2, 12, 0, 3, 0) Minute)
             [ "aujourd'hui à minuit trois"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "à quinze heures quinze"
             , "à quinze heures et quinze minutes"
             , "15h passé de 15 minutes"
             , "à trois heures et quart cet après-midi"
             , "15:15"
             , "15h15"
             ]
  , examples (datetime (2013, 2, 13, 15, 15, 0) Minute)
             [ "à trois heures et quart demain après-midi"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "à quinze heures trente"
             , "à quinze heures passé de trente minutes"
             , "à trois heures et demi cet après-midi"
             , "15:30"
             , "15h30"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "midi moins le quart"
             , "11h45"
             , "onze heures trois quarts"
             , "aujourd'hui à 11h45"
             ]
  , examples (datetime (2013, 2, 13, 11, 0, 0) Hour)
             [ "mercredi à 11h"
             ]
  , examples (datetime (2013, 2, 13, 11, 0, 0) Hour)
             [ "demain à 11 heures"
             , "demain à 11H"
             ]
  , examples (datetime (2013, 2, 14, 11, 0, 0) Hour)
             [ "jeudi à 11h"
             , "après-demain à 11 heures"
             , "après-demain à 11H"
             ]
  , examples (datetime (2013, 2, 15, 12, 0, 0) Hour)
             [ "vendredi à midi"
             , "vendredi à 12h"
             ]
  , examples (datetime (2013, 2, 15, 16, 0, 0) Hour)
             [ "vendredi quinze à seize heures"
             , "vendredi 15 à 16h"
             , "vendredi quinze à 16h"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "dans une seconde"
             , "dans 1\""
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "dans une minute"
             , "dans 1 min"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "dans 2 minutes"
             , "dans deux min"
             , "dans 2'"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "dans 60 minutes"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "dans une heure"
             ]
  , examples (datetime (2013, 2, 12, 2, 30, 0) Minute)
             [ "il y a deux heures"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "dans 24 heures"
             , "dans vingt quatre heures"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "dans un jour"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "dans 7 jours"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "dans 1 semaine"
             , "dans une semaine"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "il y a trois semaines"
             ]
  , examples (datetime (2013, 4, 12, 0, 0, 0) Day)
             [ "dans deux mois"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "il y a trois mois"
             ]
  , examples (datetime (2014, 2, 0, 0, 0, 0) Month)
             [ "dans une année"
             , "dans 1 an"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "il y a deux ans"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "cet été"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "cet hiver"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "Noel"
             , "noël"
             , "jour de noel"
             ]
  , examples (datetimeInterval ((2013, 12, 24, 18, 0, 0), (2013, 12, 25, 0, 0, 0)) Hour)
             [ "le soir de noël"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "jour de l'an"
             , "nouvel an"
             , "premier janvier"
             ]
  , examples (datetime (2013, 11, 1, 0, 0, 0) Day)
             [ "la toussaint"
             , "le jour de la toussaint"
             , "la journée de la toussaint"
             , "toussaint"
             , "le jour des morts"
             ]
  , examples (datetime (2013, 5, 1, 0, 0, 0) Day)
             [ "fête du travail"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "cet après-midi"
             , "l'après-midi"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 7, 0, 0), (2013, 2, 12, 9, 0, 0)) Hour)
             [ "aujourd'hui en début de matinée"
             , "en début de matinée"
             , "le 12 février en début de matinée"
             , "aujourd'hui très tôt le matin"
             , "aujourd'hui tôt le matin"
             , "aujourd'hui le matin tôt"
             , "aujourd'hui le matin très tôt"
             , "le matin très tôt"
             , "le matin tôt"
             , "tôt le matin"
             , "très tôt le matin"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 0, 0), (2013, 2, 12, 11, 0, 0)) Hour)
             [ "aujourd'hui en milieu de matinée"
             , "le 12 février en milieu de matinée"
             , "en milieu de matinée"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 10, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "aujourd'hui en fin de matinée"
             , "en fin de matinée"
             , "le 12 février en fin de matinée"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "après déjeuner"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 10, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "avant déjeuner"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 14, 0, 0)) Hour)
             [ "aujourd'hui pendant le déjeuner"
             , "à l'heure du déjeuner"
             , "au moment de déjeuner"
             , "pendant le déjeuner"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 17, 0, 0), (2013, 2, 12, 21, 0, 0)) Hour)
             [ "après le travail"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "dès le matin"
             , "dès la matinée"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 14, 0, 0)) Hour)
             [ "aujourd'hui en début d'après-midi"
             , "en début d'après-midi"
             , "le 12 février en début d'après-midi"
             , "au début de l'après-midi"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 14, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "aujourd'hui en milieu d'après-midi"
             , "en milieu d'après-midi"
             , "le 12 février en milieu d'après-midi"
             , "au milieu de l'après-midi"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 17, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "aujourd'hui en fin d'après-midi"
             , "en fin d'après-midi"
             , "le 12 février en fin d'après-midi"
             , "à la fin de l'après-midi"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 6, 0, 0), (2013, 2, 12, 10, 0, 0)) Hour)
             [ "aujourd'hui en début de journée"
             , "le 12 février en début de journée"
             , "en début de journée"
             , "au début de la journée"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 0, 0), (2013, 2, 12, 16, 0, 0)) Hour)
             [ "aujourd'hui en milieu de journée"
             , "en milieu de journée"
             , "le 12 février en milieu de journée"
             , "au milieu de la journée"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 17, 0, 0), (2013, 2, 12, 21, 0, 0)) Hour)
             [ "aujourd'hui en fin de journée"
             , "en fin de journée"
             , "le 12 février en fin de journée"
             , "à la fin de la journée"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "ce soir"
             , "le soir"
             , "dans la soirée"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 12, 21, 0, 0)) Hour)
             [ "aujourd'hui en début de soirée"
             , "en début de soirée"
             , "le 12 février en début de soirée"
             , "au début de la soirée"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 21, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "aujourd'hui en fin de soirée"
             , "en fin de soirée"
             , "le 12 février en fin de soirée"
             , "à la fin de la soirée"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "demain soir"
             , "mercredi soir"
             , "mercredi en soirée"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "hier soir"
             , "la veille au soir"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "ce week-end"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 13, 0, 0, 0)) Day)
             [ "en début de semaine"
             , "au début de la semaine"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 15, 0, 0, 0)) Day)
             [ "en milieu de semaine"
             , "au milieu de la semaine"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 0, 0, 0), (2013, 2, 18, 0, 0, 0)) Day)
             [ "en fin de semaine"
             , "à la fin de la semaine"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "en semaine"
             ]
  , examples (datetimeInterval ((2013, 9, 6, 18, 0, 0), (2013, 9, 9, 0, 0, 0)) Hour)
             [ "le premier week-end de septembre"
             ]
  , examples (datetimeInterval ((2013, 9, 13, 18, 0, 0), (2013, 9, 16, 0, 0, 0)) Hour)
             [ "le deuxième week-end de septembre"
             ]
  , examples (datetimeInterval ((2013, 9, 27, 18, 0, 0), (2013, 9, 30, 0, 0, 0)) Hour)
             [ "le dernier week-end de septembre"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "lundi matin"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 12, 0, 0), (2013, 2, 18, 19, 0, 0)) Hour)
             [ "lundi après-midi"
             , "lundi dans l'après-midi"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 17, 0, 0), (2013, 2, 18, 19, 0, 0)) Hour)
             [ "lundi fin d'après-midi"
             , "lundi en fin d'après-midi"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "le 15 février dans la matinée"
             , "matinée du 15 février"
             , "le 15 février le matin"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 heures ce soir"
             , "8h du soir"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "3 heures du matin"
             , "3h du mat"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "2 dernières secondes"
             , "deux dernieres secondes"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "3 prochaines secondes"
             , "trois prochaines secondes"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "2 dernieres minutes"
             , "deux dernières minutes"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "3 prochaines minutes"
             , "trois prochaines minutes"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "3 prochaines heures"
             , "3 heures suivantes"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "2 dernier jours"
             , "deux derniers jour"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "3 prochains jours"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "2 dernieres semaines"
             , "2 semaines passées"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "3 prochaines semaines"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "2 derniers mois"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "3 prochains mois"
             , "3 mois suivant"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "2 dernieres annees"
             , "2 années passées"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "3 prochaines années"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13-15 juillet"
             , "13 au 15 juillet"
             , "13 jusqu'au 15 juillet"
             , "13 juillet au 15 juillet"
             , "13 juillet - 15 juillet"
             , "entre le 13 et le 15 juillet"
             , "samedi 13 au dimanche 15e juillet"
             , "du samedi 13 au dimanche 15 juillet"
             , "du 13 au dimanche 15 juillet"
             , "entre le 13 et le quinze juillet"
             , "du treize au 15 juillet"
             , "du 13e au 15 juillet"
             ]
  , examples (datetimeInterval ((2013, 7, 1, 0, 0, 0), (2013, 7, 11, 0, 0, 0)) Day)
             [ "1er au 10 juillet"
             , "lundi 1er au mercredi 10 juillet"
             , "lundi 1 au mercredi 10e juillet"
             , "du lundi 1er au mercredi 10 juillet"
             , "du 1er au mercredi 10 juillet"
             , "du 1er au dix juillet"
             , "1er au dix juillet"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 19, 0, 0, 0)) Day)
             [ "du 13 au 18"
             , "entre le 13 et le dix-huit"
             , "du 13e au dix-huit"
             ]
  , examples (datetimeInterval ((2013, 6, 10, 0, 0, 0), (2013, 7, 2, 0, 0, 0)) Day)
             [ "10 juin au 1er juillet"
             , "entre le 10 juin et le 1er juillet"
             , "du 10 juin au 1er juillet"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "de 9h30 jusqu'à 11h jeudi"
             , "de 9 heures 30 à 11h jeudi"
             , "de 9 heures 30 a 11h jeudi"
             , "entre 9h30 et 11h jeudi"
             , "jeudi mais entre 9h30 et 11h"
             , "jeudi par exemple entre 9h30 et 11h"
             , "9h30 - 11h00 Jeudi"
             ]
  , examples (datetimeOpenInterval After (2013, 3, 8, 0, 0, 0) Day)
             [ "à partir du 8"
             , "à partir du 8 mars"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 14, 9, 30, 0) Minute)
             [ "à partir de 9h30 jeudi"
             , "jeudi après 9h30"
             , "jeudi plus tard que 9h30"
             , "jeudi matin à partir de 9 heures 30"
             ]
  , examples (datetimeOpenInterval After (2013, 11, 1, 16, 0, 0) Hour)
             [ "après 16h le 1er novembre"
             , "plus tard que 16h le 1er novembre"
             ]
  , examples (datetimeOpenInterval After (2013, 11, 1, 0, 0, 0) Day)
             [ "après le 1er novembre"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 16, 0, 0) Hour)
             [ "avant 16h"
             , "n'importe quand avant 16h"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 13, 17, 0, 0)) Hour)
             [ "demain jusqu'à 16h"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 20, 10, 0, 0) Hour)
             [ "le 20 à partir de 10h"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 15, 12, 0, 0) Hour)
             [ "vendredi à partir de midi"
             ]
  , examples (datetimeInterval ((2013, 2, 20, 0, 0, 0), (2013, 2, 20, 19, 0, 0)) Hour)
             [ "le 20 jusqu'à 18h"
             ]
  , examples (datetimeInterval ((2014, 9, 14, 0, 0, 0), (2014, 9, 21, 0, 0, 0)) Day)
             [ "14 - 20 sept. 2014"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "d'ici 2 semaines"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 27, 4, 0, 0)) Second)
             [ "dans les 15 jours"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "de 5 à 7"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "jeudi de 9h à 11h"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 15, 0, 0)) Hour)
             [ "entre midi et 2"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11h30-1h30"
             , "de 11h30 à 1h30"
             , "de 11h30 jusqu'à 1h30"
             ]
  , examples (datetime (2013, 9, 21, 13, 30, 0) Minute)
             [ "13h30 samedi 21 septembre"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "à seize heures CET"
             ]
  , examples (datetimeInterval ((2013, 3, 21, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Day)
             [ "fin mars"
             , "fin du mois de mars"
             ]
  , examples (datetimeInterval ((2013, 4, 1, 0, 0, 0), (2013, 4, 6, 0, 0, 0)) Day)
             [ "début avril"
             , "début du mois d'avril"
             ]
  , examples (datetimeInterval ((2013, 4, 1, 0, 0, 0), (2013, 4, 15, 0, 0, 0)) Day)
             [ "la première quinzaine d'avril"
             ]
  , examples (datetimeInterval ((2013, 4, 15, 0, 0, 0), (2013, 5, 1, 0, 0, 0)) Day)
             [ "la deuxième quinzaine d'avril"
             ]
  , examples (datetimeInterval ((2013, 4, 1, 0, 0, 0), (2013, 4, 6, 0, 0, 0)) Day)
             [ "début avril"
             , "début du mois d'avril"
             ]
  , examples (datetimeInterval ((2013, 12, 10, 0, 0, 0), (2013, 12, 20, 0, 0, 0)) Day)
             [ "mi-décembre"
             ]
  , examples (datetimeInterval ((2013, 2, 21, 0, 0, 0), (2013, 3, 1, 0, 0, 0)) Day)
             [ "en fin de mois"
             , "à la fin du mois"
             ]
  , examples (datetimeInterval ((2013, 11, 1, 0, 0, 0), (2014, 1, 1, 0, 0, 0)) Month)
             [ "en fin d'année"
             , "à la fin de l'année"
             ]
  , examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 3, 1, 0, 0, 0)) Month)
             [ "en début d'année"
             , "au début de l'année"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Day)
             [ "au début du mois"
             , "en début de mois"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "mars"
             , "en mars"
             , "au mois de mars"
             , "le mois de mars"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "jeudi 15"
             ]
  , examples (datetime (2013, 8, 15, 8, 0, 0) Hour)
             [ "jeudi 15 à 8h"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 4, 40, 0) Minute)
             [ "plus tard"
             , "un peu plus tard"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "plus tard dans l'après-midi"
             , "un peu plus tard dans l'après-midi"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 00, 0, 0)) Hour)
             [ "plus tard dans la soirée"
             , "un peu plus tard dans la soirée"
             ]
  ]
