-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.NL.Corpus
  ( corpus
  , defaultCorpus
  , latentCorpus
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

context :: Context
context = testContext {locale = makeLocale NL Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

defaultCorpus :: Corpus
defaultCorpus = (context, testOptions, allExamples ++ custom)
  where
    custom = concat
      [ examples (datetimeHoliday (2013, 12, 5, 0, 0, 0) Day "Sinterklaas")
                 [ "Sinterklaas"
                 , "Pakjesavond"
                 , "Sinterklaasavond"
                 ]
      ]

latentCorpus :: Corpus
latentCorpus = (context, testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (datetimeOpenInterval Before (2013, 2, 12, 8, 0, 0) Hour)
                 [ "voor 8"
                 ]
      ]

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "een hotel"
      , "twee aanbiedingen"
      , "komende 5 agendapunten"
      , "d"
      , "do"
      , "di"
      , "woe"
      , "vr"
      , "zat"
      , "zo"
      , "zon"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "nu"
             , "direct"
             , "zojuist"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "vandaag"
             , "op deze dag"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "gisteren"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "morgen"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "maandag"
             , "ma."
             , "volgende week maandag"
             , "komende maandag"
             , "aanstaande maandag"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Maandag, 18 Feb"
             , "maandag, 18 februari"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "dinsdag"
             , "di."
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "donderdag"
             , "do."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "vrijdag"
             , "vr."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "zaterdag"
             , "za."
             , "zat."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "zondag"
             , "zo."
             , "zon."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1 maart"
             , "op de eerste"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "3 maart"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 maart 2015"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "op de 15de"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15 februari"
             , "februari 15"
             , "op de 15de februari"
             , "15-2"
             , "op 15-2"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "Aug 8"
             , "8 augustus"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "Oktober 2014"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31.10.1974"
             , "31.10.74"
             , "31-10-1974"
             , "31-10-74"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14 april 2015"
             , "April 14, 2015"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "volgende week dinsdag"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "volgende week vrijdag"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "maart"
             ]
  , examples (datetime (2014, 3, 0, 0, 0, 0) Month)
             [ "volgend jaar maart"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "zondag, feb 10"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "woensdag, 13 februari"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Maandag feb 18"
             , "Maandag 18 februari"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "Deze week"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "vorige week"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "volgende week"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "vorige maand"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "volgende maand"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "dit kwartaal"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "volgende kwartaal"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "derde kwartaal"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4de kwartaal 2018"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "vorig jaar"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "dit jaar"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "volgend jaar"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "afgelopen zondag"
             , "vorige week zondag"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "vorige dinsdag"
             , "afgelopen dinsdag"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "dinsdag over een week"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "komende woensdag"
             , "woensdag"
             , "woe."
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "volgende week woensdag"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "deze week maandag"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "dinsdag deze week"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "deze week woensdag"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "overmorgen"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "eergisteren"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "laatste maandag in maart"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "laatste zondag van maart 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "derde dag in oktober"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "eerste week in oktober 2014"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "laatste dag van oktober 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "laatste week van september 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "eerste dinsdag in oktober"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "derde dinsdag in september 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "eerste woensdag in oktober 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "tweede woensdag in oktober 2014"
             ]
  , examples (datetime (2015, 1, 13, 0, 0, 0) Day)
             [ "derde dinsdag na kerst 2014"
             ]
  , examples (datetime (2013, 2, 12, 4, 0, 0) Hour)
             [ "4 uur in de morgen"
             , "4 uur in de nacht"
             , "4 uur in de ochtend"
             , "4 uur 's ochtends"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "3 uur"
             , "om drie uur"
             ]
  , examples (datetime (2013, 2, 12, 3, 18, 0) Minute)
             [ "3:18 in de ochtend"
             , "3:18 's morgens"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "drie uur 's middags"
             , "15 uur"
             , "om drie uur"
             , "rond drie uur"
             , "rond 3 uur 's middags"
             , "om ongeveer drie uur"
             , "ca. 15h"
             , "om circa 15u"
             ]
  , examples (datetime (2013, 4, 1, 18, 0, 0) Hour)
             [ "op 01-04 tegen 18 uur"
             , "op 1 april tegen zes uur 's avonds"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "stipt om 17 uur morgen"
             ]
  , examples (datetime (2013, 2, 12, 14, 15, 0) Minute)
             [ "rond kwart over 2"
             , "kwart over twee uur"
             , "14 uur 15"
             , "15 over 14"
             , "2 uur 15 's middags"
             , "14:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "15:20"
             , "10 voor half 4"
             , "10 minuten voor half 4"
             , "3 uur 20 's middags"
             , "twintig over 3"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "om half vier"
             , "15:30"
             , "half vier 's middags"
             , "half 4"
             ]
  , examples (datetime (2013, 2, 13, 3, 30, 0) Minute)
             [ "3:30"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "kwart voor 12"
             , "11:45"
             , "15 minuten voor 12"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 uur 's avonds"
             , "vanavond om 8 uur"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Minute)
             [ "vandaag om 20:00"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "om 19:30 op vr. 20 sept."
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "op zaterdag om 9 uur 's morgens"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Hour)
             [ "Vrij. 18 juli 2014 7 uur 's avonds"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Vr., 18 Juli 2014"
             , "Vrijdag, 18-07-14"
             , "Vrijdag, 18/07/2014"
             , "18de juli 2014"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "over een seconde"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "in een minuut"
             , "over een minuut"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "in 2 minuten"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "in 60 minuten"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "in 30 minuten"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "in 2,5 uur"
             , "in twee en een half uur"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "over een uur"
             , "in een uur"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "over twee uur"
             , "in een paar uur"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "over 24 uur"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "morgen"
             ]
  , examples (datetime (2016, 2, 0, 0, 0, 0) Month)
             [ "in 3 jaar"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "in 7 dagen"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "in 1 week"
             , "over een week"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 dagen geleden"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 dagen geleden"
             ]
  , examples (datetime (2013, 1, 29, 0, 0, 0) Day)
             [ "twee weken geleden"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "een week geleden"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "drie weken geleden"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "drie maanden geleden"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "twee jaar geleden"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "over 7 dagen"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "een jaar na kerst"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "deze zomer"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "deze winter"
             ]
  , examples (datetimeHoliday (2013, 12, 25, 0, 0, 0) Day "Kerstmis")
             [ "kerst"
             , "kerstmis"
             ]
  , examples (datetimeHoliday (2013, 12, 31, 0, 0, 0) Day "Oudjaar")
             [ "oudjaar"
             , "oudejaarsavond"
             ]
  , examples (datetimeHoliday (2014, 1, 1, 0, 0, 0) Day "Nieuwjaarsdag")
             [ "nieuwjaarsdag"
             , "nieuwjaar"
             ]
  , examples (datetimeHoliday (2013, 2, 14, 0, 0, 0) Day "Valentijnsdag")
             [ "Valentijnsdag"
             ]
  , examples (datetimeHoliday (2013, 5, 12, 0, 0, 0) Day "Moederdag")
             [ "moederdag"
             ]
  , examples (datetimeHoliday (2013, 6, 16, 0, 0, 0) Day "Vaderdag")
             [ "vaderdag"
             ]
  , examples (datetimeHoliday (2013, 10, 31, 0, 0, 0) Day "Halloween")
             [ "Halloween"
             ]
  , examples (datetimeHoliday (2013, 11, 1, 0, 0, 0) Day "Allerheiligen")
             [ "Allerheiligen"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "vanavond"
             , "deze avond"
             , "vandaag avond"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "morgenavond"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "gisteravond"
             , "gisterenavond"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 0, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "vanmorgen"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 13, 12, 0, 0)) Hour)
             [ "morgenochtend"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 11, 12, 0, 0)) Hour)
             [ "gisterenochtend"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "morgennacht"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "gisterennacht"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 18, 0, 0)) Hour)
             [ "vanmiddag"
             , "deze namiddag"
             , "vandaag namiddag"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 18, 0, 0)) Hour)
             [ "morgenmiddag"
             , "morgen 's middags"
             , "morgen namiddag"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 12, 0, 0), (2013, 2, 11, 18, 0, 0)) Hour)
             [ "gisterenmiddag"
             , "gistermiddag"
             , "gisterennamiddag"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "dit weekend"
             , "komend weekend"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 0, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "15 februari 's morgens"
             , "15 februari in de morgen"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "afgelopen 2 seconden"
             , "vorige 2 secondes"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "komende drie secondes"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "afgelopen 2 minuten"
             , "afgelopen twee minuten"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "komende 3 minuten"
             , "volgende drie minuten"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "komende 3 uur"
             , "volgende 3 uur"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "afgelopen 2 dagen"
             , "vorige 2 dagen"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "komende 3 dagen"
             , "komende drie dagen"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 15, 0, 0, 0)) Day)
             [ "komende paar dagen"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "vorige 2 weken"
             , "afgelopen twee weken"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "komende 3 weken"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "vorige 2 maanden"
             , "afgelopen twee maanden"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "komende 3 maanden"
             , "volgende drie maanden"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "vorige 2 jaren"
             , "afgelopen twee jaar"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "komende 3 jaren"
             , "volgende drie jaar"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13 - 15 juli"
             , "13 t/m 15 juli"
             , "13 - 15 juli"
             , "juli 13 - juli 15"
             , "13 tot en met 15 juli"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "aug 8 - aug 12"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "op donderdag om 9:30 - 11:00"
             , "op donderdag tussen 9:30 en 11:00"
             , "donderdag 9:30 - 11:00"
             , "op donderdag na 9:30 maar voor 11:00"
             , "donderdag van 9:30 tot 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "donderdag ochtend van 9 tot 11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-13:30"
             ]
  , examples (datetime (2013, 9, 21, 1, 30, 0) Minute)
             [ "1:30 am Za. 21 sept"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "binnen 2 weken"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Hour)
             [ "tot 2 uur 's middags"
             ]
  , examples (datetimeOpenInterval Before (2013, 3, 1, 0, 0, 0) Month)
             [ "tot het einde van de maand"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "16 uur CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "donderdag 8:00 GMT"
             , "donderdag 8:00 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "vandaag om 14 uur"
             , "om 2 uur"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "morgen om 15 uur"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "vanaf 14u"
             , "op zijn vroegst om 14 uur"
             , "twee uur op zijn vroegst"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "tot 11 uur"
             , "tot 11h 's morgens"
             , "tot 11 am in de ochtend"
             , "op zijn laatst om 11 uur"
             , "11 uur op zijn laatst"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "om 13:30 in de middag"
             , "vanmiddag om 1 uur 30"
             , "13:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "in 15 minuten"
             , "over een kwartier"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "na de lunch"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "komende maandag"
             , "aanstaande maandag"
             ]
  , examples (datetime (2013, 12, 10, 0, 0, 0) Day)
             [ "10-12"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 30, 0), (2013, 2, 12, 19, 1, 0)) Minute)
             [ "18:30u - 19:00u"
             , "18:30h tot 19:00h"
             ]
  , examples (datetimeInterval ((2013, 10, 14, 0, 0, 0), (2013, 10, 16, 0, 0, 0)) Day)
             [ "14/10 - 15/10"
             , "van 14/10/2013 tot 15/10/2013"
             ]
  , examples (datetimeInterval ((2018, 10, 14, 0, 0, 0), (2018, 10, 16, 0, 0, 0)) Day)
             [ "14 t/m 15 oktober 2018"
             ]
  , examples (datetime (2013, 10, 10, 0, 0, 0) Day)
             [ "op 10-10"
             , "op 10.10"
             , "10/10"
             ]
  , examples (datetime (2013, 2, 12, 10, 10, 0) Minute)
             [ "om 10:10"
             , "op 10:10"
             ]
  , examples (datetime (2013, 2, 12, 17, 10, 0) Minute)
             [ "17h10"
             , "17u10"
             ]
  , examples (datetimeHoliday (2018, 4, 27, 0, 0, 0) Day "Koningsdag")
           [ "Koningsdag 2018"
           , "koningsdag 2018"
           , "king's day 2018"
           , "King's Day 2018"
           ]
  , examples (datetimeHoliday (2014, 4, 26, 0, 0, 0) Day "Koningsdag")
           [ "Koningsdag 2014"
           , "koningsdag 2014"
           , "King's Day 2014"
           , "king's day 2014"
           ]
  ]
