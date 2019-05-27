-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.SV.Corpus
  ( corpus
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
corpus = (testContext {locale = makeLocale SV Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "nu"
             , "just nu"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "idag"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "igår"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "imorgon"
             , "i morgon"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "måndag"
             , "mån"
             , "på måndag"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Måndag den 18 februari"
             , "Mån, 18 februari"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "tisdag"
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
             [ "lördag"
             , "lör"
             , "lör."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "söndag"
             , "sön"
             , "sön."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "Den förste mars"
             , "Den första mars"
             , "1:a mars"
             , "Den 1:a mars"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "3 mars"
             , "den tredje mars"
             , "den 3:e mars"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 mars 2015"
             , "tredje mars 2015"
             , "3:e mars 2015"
             , "3-3-2015"
             , "03-03-2015"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "På den 15:e"
             , "På den 15"
             , "Den 15:e"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "den 15:e februari"
             , "15:e februari"
             , "februari 15"
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
             , "fjortonde April 15"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "nästa fredag igen"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "nästa mars"
             ]
  , examples (datetime (2014, 3, 0, 0, 0, 0) Month)
             [ "nästa mars igen"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Söndag, 10 feb"
             , "Söndag 10 Feb"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Ons, Feb13"
             , "Ons feb13"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Måndag, Feb 18"
             , "Mån, februari 18"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "denna vecka"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "förra vecka"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "nästa vecka"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "förra månad"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "nästa månad"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "detta kvartal"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "nästa kvartal"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "tredje kvartalet"
             , "3:e kvartal"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4:e kvartal 2018"
             , "fjärde kvartalet 2018"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "förra år"
             , "förra året"
             , "föregående år"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "i fjol"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "i år"
             , "detta år"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "nästa år"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "förra söndag"
             , "söndag i förra veckan"
             , "söndag förra veckan"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "förra tisdag"
             , "i tisdags"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "nästa tisdag"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "nästa onsdag"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "onsdag i nästa vecka"
             , "onsdag nästa vecka"
             , "nästa onsdag igen"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "nästa fredag igen"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "måndag denna veckan"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "tisdag denna vecka"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "onsdag denna vecka"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "i överimorgon"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "i förrgår"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "sista måndag i mars"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "sista söndag i mars 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "tredje dagen i oktober"
             , "tredje dagen i Oktober"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "första veckan i oktober 2014"
             , "första veckan i Oktober 2014"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "sista dagen i oktober 2015"
             , "sista dagen i Oktober 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "sista veckan i september 2014"
             , "sista veckan i September 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "första tisdag i oktober"
             , "första tisdagen i Oktober"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "tredje tisdagen i september 2014"
             , "tredje tisdagen i September 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "första onsdagen i oktober 2014"
             , "första onsdagen i Oktober 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "andra onsdagen i oktober 2014"
             , "andra onsdagen i Oktober 2014"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "klockan 3"
             , "kl. 3"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "klockan 15"
             , "kl. 15"
             , "15h"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "ca. kl. 15"
             , "cirka kl. 15"
             , "omkring klockan 15"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "imorgon klockan 17 exakt"
             , "imorgon kl. 17 precis"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "kvart över 15"
             , "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "kl. 20 över 15"
             , "klockan 20 över 15"
             , "tjugo över 15"
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
             [ "kvart i 12"
             , "kvart i tolv"
             , "11:45"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "klockan 9 på lördag"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "Fre, Jul 18, 2014 19:00"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Fre, Jul 18"
             , "Jul 18, Fre"
             ]
  , examples (datetime (2014, 9, 20, 19, 30, 0) Minute)
             [ "kl. 19:30, Lör, 20 sep"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "om 1 sekund"
             , "om en sekund"
             , "en sekund från nu"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "om 1 minut"
             , "om en minut"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "om 2 minuter"
             , "om två minuter"
             , "om 2 minuter mer"
             , "om två minuter mer"
             --, "2 minuter från nu" t14892978
             , "två minuter från nu"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "om 60 minuter"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "om en halv timme"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "om 2,5 timme"
             , "om 2 och en halv timme"
             , "om två och en halv timme"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "om en timme"
             , "om 1 timme"
             , "om 1t"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "om ett par timmar"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "om 24 timmar"
             , "2013-02-13 kl. 4:30"
             , "2013-02-13 kl 04:30"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "om en dag"
             ]
  , examples (datetime (2016, 2, 0, 0, 0, 0) Month)
             [ -- "3 år från idag" t14892978
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "om 7 dagar"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "om en vecka"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "om ca. en halv timme"
             , "om cirka en halv timme"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 dagar sedan"
             , "sju dagar sedan"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 dagar sedan"
             , "fjorton dagar sedan"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "en vecka sedan"
             , "1 vecka sedan"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "3 veckor sedan"
             , "tre veckor sedan"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "3 månader sedan"
             , "tre månader sedan"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "två år sedan"
             , "2 år sedan"
             ]
  , examples (datetime (1954, 0, 0, 0, 0, 0) Year)
             [ "1954"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "denna sommaren"
             , "den här sommaren"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "denna vintern"
             , "den här vintern"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "juldagen"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "nyårsafton"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "nyårsdagen"
             , "nyårsdag"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "ikväll"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "förra helg"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "imorgon kväll"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "imorgon lunch"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "igår kväll"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "denna helgen"
             , "denna helg"
             , "i helgen"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "måndag morgon"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "senaste 2 sekunder"
             , "senaste två sekunderna"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "nästa 3 sekunder"
             , "nästa tre sekunder"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "senaste 2 minuter"
             , "senaste två minuter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "nästa 3 minuter"
             , "nästa tre minuter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "senaste 1 timme"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "nästa 3 timmar"
             , "nästa tre timmar"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "senaste 2 dagar"
             , "senaste två dagar"
             , "senaste 2 dagar"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "nästa 3 dagar"
             , "nästa tre dagar"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "senaste 2 veckor"
             , "senaste två veckorna"
             , "senaste två veckor"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "nästa 3 veckor"
             , "nästa tre veckorna"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "senaste 2 månader"
             , "senaste två månader"
             , "senaste två månader"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "nästa 3 månader"
             , "nästa tre månader"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "senaste 2 år"
             , "senaste två år"
             , "senaste 2 år"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "nästa 3 år"
             , "nästa tre år"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13-15 juli"
             , "13-15 Juli"
             , "13 till 15 Juli"
             , "13 juli till 15 juli"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "8 Aug - 12 Aug"
             , "8 Aug - 12 aug"
             , "8 aug - 12 aug"
             , "8 augusti - 12 augusti"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             , "9:30 till 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "från 9:30 - 11:00 på torsdag"
             , "från 9:30 till 11:00 på torsdag"
             , "mellan 9:30 och 11:00 på torsdag"
             , "9:30 - 11:00 på torsdag"
             , "9:30 till 11:00 på torsdag"
             , "efter 9:30 men före 11:00 på torsdag"
             , "torsdag från 9:30 till 11:00"
             , "torsdag mellan 9:30 och 11:00"
             , "från 9:30 till 11:00 på torsdag"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "torsdag från 9 till 11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-13:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "inom 2 veckor"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Hour)
             [ "innan kl. 14"
             , "innan klockan 14"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "@ 16 CET"
             , "kl. 16 CET"
             , "klockan 16 CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "torsdag kl. 8:00 GMT"
             , "torsdag kl. 8:00 gmt"
             , "torsdag klockan 8:00 GMT"
             , "torsdag klockan 8:00 gmt"
             , "torsdag 08:00 GMT"
             , "torsdag 08:00 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "idag kl. 14"
             , "idag klockan 14"
             , "kl. 14"
             , "klockan 14"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Minute)
             [ "25/4 kl. 16:00"
             , "25/4 klockan 16:00"
             , "25-04 klockan 16:00"
             , "25-4 kl. 16:00"
             , "2013-04-25 kl 16:00"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Minute)
             [ "15:00 imorgon"
             , "kl. 15:00 imorgon"
             , "klockan 15:00 imorgon"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "efter kl. 14"
             , "efter klockan 14"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 17, 4, 0, 0) Hour)
             [ "efter 5 dagar"
             , "efter fem dagar"
             ]
  , examples (datetime (2013, 2, 17, 4, 0, 0) Hour)
             [ "om 5 dagar"
             , "om fem dagar"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 13, 14, 0, 0) Hour)
             [ "efter imorgon kl. 14"
             , "efter imorgon klockan 14"
             , "imorgon efter kl. 14"
             , "imorgon efter klockan 14"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "före kl. 11"
             , "före klockan 11"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 13, 11, 0, 0) Hour)
             [ "imorgon före kl. 11"
             , "imorgon före klockan 11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "under eftermiddagen"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "kl. 13:30"
             , "klockan 13:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "om 15 minuter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "efter lunch"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "nästa måndag"
             ]
  ]
