-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.DA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {locale = makeLocale DA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "nu"
             , "lige nu"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "i dag"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "i går"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "i morgen"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "mandag"
             , "på mandag"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Mandag den 18. februar"
             , "Mandag, 18 februar"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "tirsdag"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "torsdag"
             , "tors"
             , "tors."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "fredag"
             , "fre"
             , "fre."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "lørdag"
             , "lør"
             , "lør."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "søndag"
             , "søn"
             , "søn."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "Den første marts"
             , "1. marts"
             , "Den 1. marts"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "3 marts"
             , "den tredje marts"
             , "den 3. marts"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 marts 2015"
             , "tredje marts 2015"
             , "3. marts 2015"
             , "3-3-2015"
             , "03-03-2015"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "På den 15."
             , "På den 15"
             , "Den 15."
             , "Den 15"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "den 15. februar"
             , "15. februar"
             , "februar 15"
             , "15-02"
             , "15/02"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "8 Aug"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "Oktober 2014"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31/10/1974"
             , "31/10/74"
             , "31-10-74"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14april 2015"
             , "April 14, 2015"
             , "fjortende April 15"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "næste tirsdag"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "næste fredag igen"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "næste marts"
             ]
  , examples (datetime (2014, 3, 0, 0, 0, 0) Month)
             [ "næste marts igen"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Søndag, 10 feb"
             , "Søndag 10 Feb"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Ons, Feb13"
             , "Ons feb13"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Mandag, Feb 18"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "denne uge"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "sidste uge"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "næste uge"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "sidste måned"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "næste måned"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "dette kvartal"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "næste kvartal"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "tredje kvartal"
             , "3. kvartal"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4. kvartal 2018"
             , "fjerde kvartal 2018"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "sidste år"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "i år"
             , "dette år"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "næste år"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "sidste søndag"
             , "søndag i sidste uge"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "sidste tirsdag"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "næste tirsdag"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "næste onsdag"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "onsdag i næste uge"
             , "onsdag næste uge"
             , "næste onsdag igen"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "næste fredag igen"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "mandag i denne uge"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "tirsdag i denne uge"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "onsdag i denne uge"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "i overmorgen"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "i forgårs"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "sidste mandag i marts"
             , "sidste mandag i Marts"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "sidste søndag i marts 2014"
             , "sidste søndag i Marts 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "tredje dag i oktober"
             , "tredje dag i Oktober"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "første uge i oktober 2014"
             , "første uge i Oktober 2014"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "sidste dag i oktober 2015"
             , "sidste dag i Oktober 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "sidste uge i september 2014"
             , "sidste uge i September 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "første tirsdag i oktober"
             , "første tirsdag i Oktober"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "tredje tirsdag i september 2014"
             , "tredje tirsdag i September 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "første onsdag i oktober 2014"
             , "første onsdag i Oktober 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "anden onsdag i oktober 2014"
             , "anden onsdag i Oktober 2014"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "klokken 3"
             , "kl. 3"
             , "kl 3"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "klokken 15"
             , "kl. 15"
             , "kl 15"
             , "15h"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "ca. kl. 15"
             , "cirka kl. 15"
             , "cirka kl 15"
             , "omkring klokken 15"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "imorgen klokken 17 sharp"
             , "imorgen kl. 17 præcis"
             , "imorgen kl 17 præcis"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "kvarter over 15"
             , "kvart over 15"
             , "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "kl. 20 over 15"
             , "klokken 20 over 15"
             , "kl. 15:20"
             , "kl 15:20"
             , "15:20"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "15:30"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "kvarter i 12"
             , "kvart i 12"
             , "11:45"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "klokken 9 på lørdag"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "Fre, Jul 18, 2014 19:00"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Fre, Jul 18"
             , "Jul 18, Fre"
             ]
  , examples (datetime (2014, 9, 20, 19, 30, 0) Minute)
             [ "kl. 19:30, Lør, 20 sep"
             , "kl 19:30, Lør, 20 sep"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "om 1 sekund"
             , "om ét sekund"
             , "om et sekund"
             , "ét sekund fra nu"
             , "et sekund fra nu"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "om 1 minut"
             , "om ét minut"
             , "om et minut"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "om 2 minutter"
             , "om to minutter"
             , "om 2 minutter mere"
             , "om to minutter mere"
             , "2 minutter fra nu"
             , "to minutter fra nu"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "om 60 minutter"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "om en halv time"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "om 2,5 time"
             , "om 2 og en halv time"
             , "om to og en halv time"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "om én time"
             , "om 1 time"
             , "om 1t"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "om et par timer"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "om 24 timer"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "om en dag"
             ]
  , examples (datetime (2016, 2, 0, 0, 0, 0) Month)
             [ "3 år fra i dag"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "om 7 dage"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "om en uge"
             , "om én uge"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "om ca. en halv time"
             , "om cirka en halv time"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 dage siden"
             , "syv dage siden"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 dage siden"
             , "fjorten dage siden"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "en uge siden"
             , "én uge siden"
             , "1 uge siden"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "3 uger siden"
             , "tre uger siden"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "3 måneder siden"
             , "tre måneder siden"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "to år siden"
             , "2 år siden"
             ]
  , examples (datetime (1954, 0, 0, 0, 0, 0) Year)
             [ "1954"
             ]
  , examples (datetime (2013, 12, 24, 0, 0, 0) Day)
             [ "et år efter juleaften"
             , "ét år efter juleaften"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "denne sommer"
             , "den her sommer"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "denne vinter"
             , "den her vinter"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "1 juledag"
             , "1. juledag"
             , "første juledag"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "nytårsaftensdag"
             , "nytårsaften"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "nytårsdag"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "i aften"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "sidste weekend"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "i morgen aften"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "i morgen middag"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "i går aftes"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "denne weekend"
             , "i weekenden"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "mandag morgen"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "sidste 2 sekunder"
             , "sidste to sekunder"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "næste 3 sekunder"
             , "næste tre sekunder"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "sidste 2 minutter"
             , "sidste to minutter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "næste 3 minutter"
             , "næste tre minutter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "sidste 1 time"
             , "seneste 1 time"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "næste 3 timer"
             , "næste tre timer"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "sidste 2 dage"
             , "sidste to dage"
             , "seneste 2 dage"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "næste 3 dage"
             , "næste tre dage"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "sidste 2 uger"
             , "sidste to uger"
             , "seneste to uger"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "næste 3 uger"
             , "næste tre uger"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "sidste 2 måneder"
             , "sidste to måneder"
             , "seneste to måneder"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "næste 3 måneder"
             , "næste tre måneder"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "sidste 2 år"
             , "sidste to år"
             , "seneste 2 år"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "næste 3 år"
             , "næste tre år"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13-15 juli"
             , "13-15 Juli"
             , "13 til 15 Juli"
             , "13 juli til 15 juli"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "8 Aug - 12 Aug"
             , "8 Aug - 12 aug"
             , "8 aug - 12 aug"
             , "8 august - 12 august"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             , "9:30 til 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "fra 9:30 - 11:00 på torsdag"
             , "fra 9:30 til 11:00 på torsdag"
             , "mellem 9:30 og 11:00 på torsdag"
             , "9:30 - 11:00 på torsdag"
             , "9:30 til 11:00 på torsdag"
             , "efter 9:30 men før 11:00 på torsdag"
             , "torsdag fra 9:30 til 11:00"
             , "torsdag mellem 9:30 og 11:00"
             , "fra 9:30 til 11:00 på torsdag"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "torsdag fra 9 til 11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-13:30"
             , "11:30-13:30"
             , "11:30-13:30"
             , "11:30-13:30"
             , "11:30-13:30"
             , "11:30-13:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "indenfor 2 uger"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Hour)
             [ "inden kl. 14"
             , "indtil kl. 14"
             , "indtil kl 14"
             , "inden klokken 14"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "@ 16 CET"
             , "kl. 16 CET"
             , "kl 16 CET"
             , "klokken 16 CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "torsdag kl. 8:00 GMT"
             , "torsdag kl. 8:00 gmt"
             , "torsdag kl 8:00 gmt"
             , "torsdag klokken 8:00 GMT"
             , "torsdag klokken 8:00 gmt"
             , "torsdag 08:00 GMT"
             , "torsdag 08:00 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "idag kl. 14"
             , "i dag kl. 14"
             , "i dag kl. 14"
             , "idag klokken 14"
             , "kl. 14"
             , "kl 14"
             , "klokken 14"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Minute)
             [ "25/4 kl. 16:00"
             , "25/4 kl 16:00"
             , "25/4 klokken 16:00"
             , "25-04 klokken 16:00"
             , "25-4 kl. 16:00"
             , "25-4 kl 16:00"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Minute)
             [ "15:00 i morgen"
             , "kl. 15:00 i morgen"
             , "kl 15:00 i morgen"
             , "klokken 15:00 i morgen"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "efter kl. 14"
             , "efter kl 14"
             , "efter klokken 14"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 17, 4, 0, 0) Hour)
             [ "efter 5 dage"
             , "efter fem dage"
             ]
  , examples (datetime (2013, 2, 17, 4, 0, 0) Hour)
             [ "om 5 dage"
             , "om fem dage"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 13, 14, 0, 0) Hour)
             [ "efter i morgen kl. 14"
             , "efter i morgen kl 14"
             , "efter i morgen klokken 14"
             , "i morgen efter kl. 14"
             , "i morgen efter kl 14"
             , "i morgen efter klokken 14"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "før kl. 11"
             , "før kl 11"
             , "før klokken 11"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 13, 11, 0, 0) Hour)
             [ "i morgen før kl. 11"
             , "i morgen før kl 11"
             , "i morgen før klokken 11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "om eftermiddagen"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "kl. 13:30"
             , "kl 13:30"
             , "klokken 13:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "om 15 minutter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "efter frokost"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "denne morgen"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "næste mandag"
             ]
  ]
