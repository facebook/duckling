-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.NB.Corpus
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
corpus = (testContext {locale = makeLocale NB Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "nå"
             , "akkurat nå"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "i dag"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "i går"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "i morgen"
             , "i morra"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "mandag"
             , "man."
             , "på mandag"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Mandag den 18. februar"
             , "Man, 18 februar"
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
             [ "Den første mars"
             , "1. mars"
             , "Den 1. mars"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "3 mars"
             , "den tredje mars"
             , "den 3. mars"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 mars 2015"
             , "tredje mars 2015"
             , "3. mars 2015"
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
             , "Den femtende"
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
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "neste fredag igjen"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "neste mars"
             ]
  , examples (datetime (2014, 3, 0, 0, 0, 0) Month)
             [ "neste mars igjen"
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
             , "Man, februar 18"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "denne uken"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "forrige uke"
             , "sist uke"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "neste uke"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "forrige måned"
             , "sist måned"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "neste måned"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "dette kvartalet"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "neste kvartal"
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
             [ "forrige år"
             , "sist år"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "i fjor"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "i år"
             , "dette år"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "neste år"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "forrige søndag"
             , "sist søndag"
             , "søndag i forrige uke"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "forrige tirsdag"
             , "sist tirsdag"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "neste tirsdag"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "neste onsdag"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "onsdag i neste uke"
             , "onsdag neste uke"
             , "neste onsdag igjen"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "neste fredag igjen"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "mandag denne uken"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "tirsdag denne uken"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "onsdag denne uken"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "i overimorgen"
             , "i overimorra"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "i forigårs"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "siste mandag i mars"
             , "siste mandag i mars"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "siste søndag i mars 2014"
             , "siste søndag i mars 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "tredje dag i oktober"
             , "tredje dag i Oktober"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "første uke i oktober 2014"
             , "første uke i Oktober 2014"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "siste dag i oktober 2015"
             , "siste dag i Oktober 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "siste uke i september 2014"
             , "siste uke i September 2014"
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
             [ "andre onsdag i oktober 2014"
             , "andre onsdag i Oktober 2014"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "klokken 3"
             , "klokka 3"
             , "kl. 3"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "klokken 15"
             , "klokka 15"
             , "kl. 15"
             , "15h"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "ca. kl. 15"
             , "cirka kl. 15"
             , "omkring klokken 15"
             , "omkring klokka 15"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "imorgen klokken 17 sharp"
             , "imorra klokken 17 sharp"
             , "imorgen klokka 17 presis"
             , "imorra klokka 17 presis"
             , "imorgen kl. 17 presis"
             , "imorra kl. 17 presis"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "kvarter over 15"
             , "kvart over 15"
             , "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "kl. 20 over 15"
             , "klokken 20 over 15"
             , "klokka 20 over 15"
             , "kl. 15:20"
             , "15:20"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "15:30"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "kvarter på 12"
             , "kvart på 12"
             , "11:45"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "klokken 9 på lørdag"
             , "klokka 9 på lørdag"
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
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "om 1 sekund"
             , "om ett sekund"
             , "om et sekund"
             , "ett sekund fra nå"
             , "et sekund fra nå"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "om 1 minutt"
             , "om et minutt"
             , "om ett minutt"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "om 2 minutter"
             , "om to minutter"
             , "om 2 minutter mer"
             , "om to minutter mer"
             , "2 minutter fra nå"
             , "to minutter fra nå"
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
             [ "om 7 dager"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "om en uke"
             , "om én uke"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "om ca. en halv time"
             , "om cirka en halv time"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 dager siden"
             , "syv dager siden"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 dager siden"
             , "fjorten dager siden"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "en uke siden"
             , "én uke siden"
             , "1 uke siden"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "3 uker siden"
             , "tre uker siden"
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
             [ "et år etter julaften"
             , "ett år etter julaften"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "denne sommeren"
             , "den her sommeren"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "denne vinteren"
             , "den her vinteren"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "1 juledag"
             , "1. juledag"
             , "første juledag"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "nyttårsaften"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "nyttårsdag"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "i kveld"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "forrige helg"
             , "sist helg"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "i morgen kveld"
             , "i morra kveld"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "i morgen middag"
             , "i morra middag"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "i går kveld"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "denne helgen"
             , "denne helga"
             , "i helga"
             , "i helgen"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "mandag morgen"
             , "mandag morran"
             ]
  , examples (datetimeInterval ((2013, 12, 24, 0, 0, 0), (2013, 12, 31, 0, 0, 0)) Day)
             [ "i romjulen"
             , "i romjula"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "siste 2 sekunder"
             , "siste to sekunder"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "neste 3 sekunder"
             , "neste tre sekunder"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "siste 2 minutter"
             , "siste to minutter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "neste 3 minutter"
             , "neste tre minutter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "siste 1 time"
             , "seneste 1 time"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "neste 3 timer"
             , "neste tre timer"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "siste 2 dager"
             , "siste to dager"
             , "seneste 2 dager"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "neste 3 dager"
             , "neste tre dager"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "siste 2 uker"
             , "siste to uker"
             , "seneste to uker"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "neste 3 uker"
             , "neste tre uker"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "siste 2 måneder"
             , "siste to måneder"
             , "seneste to måneder"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "neste 3 måneder"
             , "neste tre måneder"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "siste 2 år"
             , "siste to år"
             , "seneste 2 år"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "neste 3 år"
             , "neste tre år"
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
             , "mellom 9:30 og 11:00 på torsdag"
             , "9:30 - 11:00 på torsdag"
             , "9:30 til 11:00 på torsdag"
             , "etter 9:30 men før 11:00 på torsdag"
             , "torsdag fra 9:30 til 11:00"
             , "torsdag mellom 9:30 og 11:00"
             , "fra 9:30 til 11:00 på torsdag"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "torsdag fra 9 til 11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-13:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "innenfor 2 uker"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Hour)
             [ "innen kl. 14"
             , "innen klokken 14"
             , "innen klokka 14"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "16h CET"
             , "kl. 16 CET"
             , "klokken 16 CET"
             , "klokka 16 CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "torsdag kl. 8:00 GMT"
             , "torsdag kl. 8:00 gmt"
             , "torsdag klokken 8:00 GMT"
             , "torsdag klokka 8:00 GMT"
             , "torsdag klokken 8:00 gmt"
             , "torsdag klokka 8:00 gmt"
             , "torsdag 08:00 GMT"
             , "torsdag 08:00 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "idag kl. 14"
             , "idag klokken 14"
             , "idag klokka 14"
             , "kl. 14"
             , "klokken 14"
             , "klokka 14"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Minute)
             [ "25/4 kl. 16:00"
             , "25/4 klokken 16:00"
             , "25/4 klokka 16:00"
             , "25-04 klokken 16:00"
             , "25-04 klokka 16:00"
             , "25-4 kl. 16:00"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Minute)
             [ "15:00 i morgen"
             , "15:00 i morra"
             , "kl. 15:00 i morgen"
             , "kl. 15:00 i morra"
             , "klokken 15:00 i morgen"
             , "klokken 15:00 i morra"
             , "klokka 15:00 i morgen"
             , "klokka 15:00 i morra"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "etter kl. 14"
             , "etter klokken 14"
             , "etter klokka 14"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 17, 4, 0, 0) Hour)
             [ "etter 5 dager"
             , "etter fem dager"
             ]
  , examples (datetime (2013, 2, 17, 4, 0, 0) Hour)
             [ "om 5 dager"
             , "om fem dager"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 13, 14, 0, 0) Hour)
             [ "etter i morgen kl. 14"
             , "etter i morra kl. 14"
             , "etter i morgen klokken 14"
             , "etter i morra klokken 14"
             , "etter i morgen klokka 14"
             , "etter i morra klokka 14"
             , "i morgen etter kl. 14"
             , "i morra etter kl. 14"
             , "i morgen etter klokken 14"
             , "i morra etter klokken 14"
             , "i morgen etter klokka 14"
             , "i morra etter klokka 14"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "før kl. 11"
             , "før klokken 11"
             , "før klokka 11"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 13, 11, 0, 0) Hour)
             [ "i morgen før kl. 11"
             , "i morra før kl. 11"
             , "i morgen før klokken 11"
             , "i morra før klokken 11"
             , "i morgen før klokka 11"
             , "i morra før klokka 11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "om ettermiddagen"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "kl. 13:30"
             , "klokken 13:30"
             , "klokka 13:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "om 15 minutter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "etter frokost"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "denne morgen"
             , "denne morran"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "neste mandag"
             ]
  , examples (datetime (2014, 2, 9, 0, 0, 0) Day)
             [ "morsdag"
             ]
  , examples (datetime (2013, 11, 10, 0, 0, 0) Day)
             [ "farsdag"
             ]
  ]
