-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.DE.Corpus
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

context :: Context
context = testContext {locale = makeLocale DE Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "ein Hotel"
      , "ein Angebot"
      , "nächsten 5"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "jetzt"
             , "genau jetzt"
             , "gerade eben"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "heute"
             , "zu dieser zeit"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "gestern"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "morgen"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "montag"
             , "mo."
             , "diesen montag"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Montag, Feb 18"
             , "Montag, Februar 18"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "dienstag"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "donnerstag"
             , "do"
             , "do."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "freitag"
             , "fr."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "samstag"
             , "sa."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "sonntag"
             , "so."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1 märz"
             , "erster märz"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "märz 3"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "märz 3 2015"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "am 15ten"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15. februar"
             , "februar 15"
             , "15te februar"
             , "15.2."
             , "am 15.2."
             , "februar 15"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "Aug 8"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "Oktober 2014"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31.10.1974"
             , "31.10.74"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14 april 2015"
             , "April 14, 2015"
             , "14te April 15"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "nächsten dienstag"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "übernächsten freitag"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "nächsten märz"
             ]
  , examples (datetime (2014, 3, 0, 0, 0, 0) Month)
             [ "übernächsten märz"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Sonntag, Feb 10"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Mittwoch, Feb 13"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Montag, Feb 18"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "diese woche"
             , "kommende woche"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "letzte woche"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "nächste woche"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "letzten monat"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "nächsten monat"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "dieses quartal"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "nächstes quartal"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "drittes quartal"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4tes quartal 2018"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "letztes jahr"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "dieses jahr"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "nächstes jahr"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "letzten sonntag"
             , "sonntag der letzten woche"
             , "sonntag letzte woche"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "letzten dienstag"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "nächsten dienstag"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "nächsten mittwoch"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "mittwoch der nächsten woche"
             , "mittwoch nächste woche"
             , "mittwoch nach dem nächsten"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "freitag nach dem nächsten"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "montag dieser woche"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "dienstag dieser woche"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "mittwoch dieser woche"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "übermorgen"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "vorgestern"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "letzter montag im märz"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "letzter sonntag im märz 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "dritter tag im oktober"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "erste woche im oktober 2014"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "letzter tag im oktober 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "letzte woche im september 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "erster dienstag im oktober"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "dritter dienstag im september 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "erster mittwoch im oktober 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "zweiter mittwoch im oktober 2014"
             ]
  , examples (datetime (2015, 1, 13, 0, 0, 0) Day)
             [ "dritter dienstag nach weihnachten 2014"
             ]
  , examples (datetime (2013, 2, 12, 4, 0, 0) Hour)
             [ "um 4 in der früh"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "um 3"
             , "3 uhr"
             , "um drei"
             ]
  , examples (datetime (2013, 2, 12, 3, 18, 0) Minute)
             [ "3:18 früh"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "um 3 am nachmittag"
             , "um 15"
             , "um 15 uhr"
             , "15 uhr"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "zirka 15 uhr"
             , "zirka 3 uhr am nachmittag"
             , "um ungefähr 15 uhr"
             , "gegen 15 uhr"
             , "ca. 15h"
             , "um ca 15h"
             ]
  , examples (datetime (2013, 4, 1, 18, 0, 0) Hour)
             [ "01.04. gegen 18Uhr"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "pünktlich um 17 uhr morgen"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "um viertel nach 3"
             , "viertel nach drei Uhr"
             , "3 uhr 15 am nachmittag"
             , "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "um 20 nach 3"
             , "15:20 am nachmittag"
             , "15 uhr 20 nachmittags"
             , "zwanzig nach 3"
             , "15:20"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "um halb 4"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "halb vier uhr nachmittags"
             , "halb vier am nachmittag"
             , "15:30"
             ]
  , examples (datetime (2013, 2, 13, 3, 30, 0) Minute)
             [ "3:30"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "viertel vor 12"
             , "11:45"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "15 minuten vor 12"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 uhr am abend"
             , "heute abend um 20 Uhr"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Minute)
             [ "heute um 20:00"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "um 19:30 am fr, 20. Sept."
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "am samstag um 9 Uhr"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Hour)
             [ "Fr, 18. Juli 2014 7 uhr abends"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Fr, 18. Juli 2014"
             , "Freitag, 18.07.14"
             , "Freitag, den 18.07.2014"
             , "Freitag, der 18. Juli 14"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "in einer sekunde"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "in einer minute"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "in 2 minuten"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "in 60 minuten"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "in einer halben stunde"
             , "in 30 minuten"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "in 2.5 stunden"
             , "in zwei ein halb stunden"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "in einer stunde"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "in zwei stunden"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "in ein paar stunden"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "in 24 stunden"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "morgen"
             ]
  , examples (datetime (2016, 2, 0, 0, 0, 0) Month)
             [ "in 3 Jahren"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "in 7 tagen"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "in 1 woche"
             , "in einer woche"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "in zirka einer halben stunde"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "vor 7 tagen"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "vor 14 tagen"
             ]
  , examples (datetime (2013, 1, 29, 0, 0, 0) Day)
             [ "vor zwei wochen"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "vor einer woche"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "vor drei wochen"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "vor drei monaten"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "vor zwei jahren"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "in 7 tagen"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "ein jahr nach weihnachten"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "diesen sommer"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "diesen winter"
             ]
  , examples (datetimeHoliday (2013, 12, 25, 0, 0, 0) Day "Weihnachten")
             [ "Weihnachten"
             , "Weihnachtstag"
             ]
  , examples (datetimeHoliday (2013, 12, 31, 0, 0, 0) Day "Silvester")
             [ "Silvester"
             ]
  , examples (datetimeHoliday (2014, 1, 1, 0, 0, 0) Day "Neujahr")
             [ "Neujahrstag"
             , "Neujahr"
             ]
  , examples (datetimeHoliday (2013, 2, 14, 0, 0, 0) Day "Valentinstag")
             [ "Valentinstag"
             ]
  , examples (datetimeHoliday (2013, 5, 12, 0, 0, 0) Day "Muttertag" )
             [ "Muttertag"
             ]
  , examples (datetimeHoliday (2013, 6, 16, 0, 0, 0) Day "Vatertag" )
             [ "Vatertag"
             ]
  , examples (datetimeHoliday (2013, 10, 3, 0, 0, 0) Day "Tag der Deutschen Einheit")
             [ "Tag der Deutschen Einheit"
             ]
  , examples (datetimeHoliday (2013, 10, 31, 0, 0, 0) Day "Halloween")
             [ "Halloween"
             ]
  , examples (datetimeHoliday (2013, 11, 1, 0, 0, 0) Day "Allerheiligen" )
             [ "Allerheiligen"
             ]
  , examples (datetimeHoliday (2013, 12, 6, 0, 0, 0) Day "Nikolaus")
             [ "Nikolaus"
             , "Nikolaustag"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "heute abend"
             , "am abend"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "morgen abend"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "morgen mittag"
             , "morgen zu mittag"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "gestern abend"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "dieses wochenende"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 3, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "montag morgens"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 3, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "morgens am 15. februar"
             , "15. februar morgens"
             , "am morgen des 15. februar"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "letzte 2 sekunden"
             , "letzten zwei sekunden"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "nächste 3 sekunden"
             , "nächsten drei sekunden"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "letzte 2 minuten"
             , "letzten zwei minuten"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "nächste 3 minuten"
             , "nächsten drei minuten"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "nächste 3 stunden"
             , "nächsten drei stunden"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "letzte 2 tage"
             , "letzten zwei tage"
             , "vergangenen zwei tage"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "nächsten 3 tagen"
             , "nächsten drei tage"
             , "kommenden drei tagen"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 15, 0, 0, 0)) Day)
             [ "nächsten paar tagen"
             , "kommenden paar tagen"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "letzten 2 wochen"
             , "letzte zwei wochen"
             , "vergangenen 2 wochen"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "nächsten 3 wochen"
             , "nächste drei wochen"
             , "kommenden drei wochen"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "letzten 2 monaten"
             , "letzte zwei monate"
             , "vergangenen zwei monaten"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "nächsten 3 monaten"
             , "nächste drei monate"
             , "kommenden drei monaten"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "letzten 2 jahren"
             , "letzten zwei jahre"
             , "vergangenen zwei jahren"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "nächsten 3 jahren"
             , "kommenden drei jahren"
             , "nächste drei jahre"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13. - 15. Juli"
             , "13ter bis 15ter Juli"
             , "13 bis 15 Juli"
             , "13 - 15 Juli"
             , "Juli 13 - Juli 15"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "Aug 8 - Aug 12"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "am Donnerstag von 9:30 - 11:00"
             , "am Donnerstag zwischen 9:30 und 11:00"
             , "Donnerstag 9:30 - 11:00"
             , "am Donnerstag nach 9:30 aber vor 11:00"
             , "Donnerstag von 9:30 bis 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "Donnerstag Vormittag von 9 bis 11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-13:30"
             ]
  , examples (datetime (2013, 9, 21, 1, 30, 0) Minute)
             [ "1:30 am Sa, 21. Sept"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "binnen 2 wochen"
             , "innerhalb von 2 wochen"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Hour)
             [ "bis 2 Uhr nachmittag"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 13, 0, 0, 0) Hour)
             [ "bis zum ende des tages"
             ]
  , examples (datetimeOpenInterval Before (2013, 3, 1, 0, 0, 0) Month)
             [ "bis zum ende des monats"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "16 Uhr CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "donnerstag 8:00 GMT"
             , "donnerstag 8:00 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "heute um 14 Uhr"
             , "um 2"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "morgen um 15 Uhr"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "nach 14 Uhr"
             , "nach 14h"
             , "ab 14Uhr"
             , "nach 2 Uhr"
             , "frühestens 14 Uhr"
             , "14 Uhr frühstens"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "bis 11 uhr"
             , "vor 11 uhr"
             , "bis 11h vormittags"
             , "bis 11 am vormittag"
             , "spätestens 11 uhr"
             , "11Uhr spätestens"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "am nachmittag"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "um 13:30 am nachmittag"
             , "nachmittags um 1 uhr 30"
             , "13:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "in 15 minuten"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "nach dem mittagessen"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "nächsten montag"
             , "kommenden montag"
             ]
  , examples (datetime (2013, 12, 10, 0, 0, 0) Day)
             [ "10.12."
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 30, 0), (2013, 2, 12, 19, 1, 0)) Minute)
             [ "18:30h - 19:00h"
             , "18:30h/19:00h"
             ]
  , examples (datetimeInterval ((2013, 10, 14, 0, 0, 0), (2013, 10, 16, 0, 0, 0)) Day)
             [ "14. - 15.10."
             , "14 - 15.10."
             , "14. - 15.10"
             , "14 - 15.10"
             , "14.10. - 15.10."
             , "14. - 15.10.2013"
             , "14.10. - 15.10.2013"
             , "14./15.10."
             ]
  , examples (datetimeInterval ((2018, 10, 14, 0, 0, 0), (2018, 10, 16, 0, 0, 0)) Day)
             [ "14. - 15.10.18"
             , "14 - 15.10.18"
             , "14.10. - 15.10.2018"
             , "14./15.10.2018"
             , "vom 14.10. - 15.10.2018"
             , "14.10. bis 15.10.2018"
             , "vom 14.10. auf den 15.10.2018"
             , "vom 14.10. bis zum 15.10.2018"
             ]
  , examples (datetime (2013, 10, 10, 0, 0, 0) Day)
             [ "am 10.10."
             , "am 10.10"
             , "10.10"
             ]
  , examples (datetime (2013, 2, 12, 10, 10, 0) Minute)
             [ "um 10.10"
             ]
  , examples (datetime (2013, 2, 12, 17, 10, 0) Minute)
             [ "17h10"
             ]
  , examples (datetime (2018, 8, 31, 0, 0, 0) Day)
             [ "2018-08-31"
             , "2018-8-31"
             ]
  , examples (datetime (1980, 5, 30, 0, 0, 0) Day)
             [ "30. Mai 1980"
             ]
  , examples (datetime (2013, 2, 9, 0, 0, 0) Day)
             [ "vorvorgestern"
             ]
  , examples (datetime (2013, 12, 5, 0, 0, 0) Day)
             [ "fünfter Dezember"
             ]
  , examples (datetime (2013, 12, 30, 0, 0, 0) Day)
             [ "dreißigster Dezember"
             , "dreissigster Dezember"
             ]
  , examples (datetime (2013, 12, 4, 0, 0, 0) Day)
             [ "am vierten Dezember"
             , "der vierte Dezember"
             ]
  ]
