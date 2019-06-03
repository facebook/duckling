-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.PL.Corpus
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
corpus = (testContext {locale = makeLocale PL Nothing}, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {locale = makeLocale PL Nothing}, testOptions, examples)
  where
    examples =
      [ "nie"
      , "niez"
      , "Za Herbatke"
      , "za herbatke"
      , "No nic"
      , "no nic"
      , "pierwszy"
      , "drugiej"
      , "trzecia piętnaście"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "teraz"
             , "w tej chwili"
             , "w tym momencie"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "dziś"
             , "dzis"
             , "dzisiaj"
             , "obecnego dnia"
             , "tego dnia"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "wczoraj"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "jutro"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "pojutrze"
             , "po jutrze"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "poniedziałek"
             , "pon."
             , "ten poniedziałek"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Poniedziałek, 18 Luty"
             , "Poniedziałek, Luty 18"
             , "Poniedziałek 18tego Lutego"
             , "Poniedziałek 18-tego Lutego"
             , "Poniedziałek, 18-tego Lutego"
             , "poniedzialek, 18go Lutego"
             , "Pon, 18 Luty"
             ]
  , examples (datetime (2013, 2, 2, 0, 0, 0) Day)
             [ "Sobota, 2ego Lutego"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "Wtorek"
             , "nastepny wtorek"
             , "wt."
             , "wtr."
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "czwartek"
             , "ten czwartek"
             , "czw"
             , "czw."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "piatek"
             , "ten piatek"
             , "pia"
             , "pia."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "sobota"
             , "ta sobota"
             , "sob"
             , "sob."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "niedziela"
             , "ta niedziela"
             , "niedz"
             , "niedz."
             , "nd"
             , "ndz"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "pierwszy marca"
             , "pierwszego marca"
             , "marzec pierwszy"
             , "1szy marca"
             , "1szy marzec"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "marzec 3"
             , "marzec 3ci"
             , "3go marca"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3ci marca 2015"
             , "marzec 3ci 2015"
             , "3 marzec 2015"
             , "marzec 3 2015"
             , "trzeci marca 2015"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15 Luty"
             , "15 Lutego"
             , "Luty 15"
             , "15-tego Lutego"
             , "2/15"
             , "Pietnastego Lutego"
             , "Piętnasty Luty"
             , "Luty Piętnastego"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "Sierpień 8"
             , "Sie 8"
             , "Sier 8"
             , "Sierp. 8"
             , "8 Sie."
             , "Ósmy Sie."
             , "Osmego Sie."
             ]
  , examples (datetime (2013, 11, 20, 0, 0, 0) Day)
             [ "20 listopada"
             , "20 listopadowi"
             , "20 listopadem"
             , "20 listopad"
             ]
  , examples (datetime (2013, 5, 20, 0, 0, 0) Day)
             [ "20 maja"
             , "20 maj"
             , "20 majem"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "Październik 2014"
             , "Pazdziernika 2014"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "10/31/1974"
             , "10/31/74"
             , "10-31-74"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14kwiecien 2015"
             , "Kwiecień 14, 2015"
             , "14tego Kwietnia 15"
             , "14-tego Kwietnia 15"
             , "14-ty Kwietnia 15"
             , "Czternasty Kwietnia 15"
             , "Czternastego Kwietnia 15"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "nastepny wtorek"
             , "kolejny wtorek"
             , "kolejnego wtorku"
             , "nastepnego wtorku"
             , "wtorek w przyszłym tygodniu"
             , "wtorek za tydzień"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "piatek po nastepnym"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "nastepny Marzec"
             ]
  , examples (datetime (2014, 3, 0, 0, 0, 0) Month)
             [ "Marzec po nastepnym"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Niedziela, 10 Luty"
             , "Niedziela, Luty 10"
             , "Niedziela, 10tego Luty"
             , "Niedziela, 10-tego Luty"
             , "Niedziela, 10-ty Lutego"
             , "Niedziela, 10tego Lutego"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Śr., Luty13"
             , "Śr., 13Luty"
             , "sr, 13Luty"
             , "sr, 13tego Lutego"
             , "Śro., 13Lutego"
             , "Środa trzynastego lutego"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Poniedziałek, Luty 18"
             , "Poniedziałek, 18 Lutego"
             , "Pon, Luty 18"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "ten tydzien"
             , "ten tydzień"
             , "ten tyg"
             , "tym tygodniu"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "ostatni tydzien"
             , "poprzedniego tygodnia"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "nastepny tydzien"
             , "nastepnego tygodnia"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "ostatni miesiac"
             , "poprzedni miesiac"
             , "poprzedniego miesiąca"
             , "po przedniego miesiąca"
             , "ostatniego miesiaca"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "nastepnego miesiaca"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "ten kwartał"
             , "tego kwartału"
             , "tym kwartale"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "nastepny kwartał"
             , "następny kwartal"
             , "kolejnym kwartale"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "trzeci kwartał"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4ty kwartał 2018"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "poprzedni rok"
             , "ostatni rok"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "ten rok"
             , "tym roku"
             , "obecny rok"
             , "w obecny rok"
             , "w obecnym roku"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "w kolejnym roku"
             , "kolejny rok"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "poprzednia niedziela"
             , "niedziela z ostatniego tygodnia"
             , "niedziela ostatniego tygodnia"
             , "niedziela poprzedniego tygodnia"
             , "ostatnia niedziela"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "ostatni wtorek"
             , "poprzedni wtorek"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "nastepny wtorek"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "nastepna środa"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "sroda nastepnego tygodnia"
             , "środa w przyszłym tygodniu"
             , "środa za tydzień"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "piatek nastepnego tygodnia"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "poniedzialek tego tygodnia"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "wtorek tego tygodnia"
             , "wtorek w tym tygodniu"
             , "ten wtorek"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "środa w tym tygodniu"
             , "ta środa"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "pojutrze"
             , "po jutrze"
             , "dzień po jutrze"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "dzień przed wczoraj"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "ostatni Poniedziałek Marca"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "ostatnia Niedziela w Marcu 2014"
             , "ostatnia Niedziela marca 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "trzeci dzień października"
             , "trzeci dzień w październiku"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "pierwszy tydzień października 2014"
             , "pierwszy tydzien w październiku 2014"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "ostatni dzień w październiku 2015"
             , "ostatni dzień października 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "ostatni tydzień we wrześniu 2014"
             , "ostatni tydzień września 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "pierwszy wtorek w październiku"
             , "pierwszy wtorek października"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "trzeci wtorek we wrześniu 2014"
             , "trzeci wtorek września 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "pierwsza środa w październiku 2014"
             , "pierwsza środa października 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "druga środa w październiku 2014"
             , "druga środa października 2014"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "o 3 rano"
             , "3 rano"
             , "3 z rana"
             , "o trzeciej rano"
             , "o trzeciej z rana"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Hour)
             [ "o pierwszy"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "o drugiej"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18 rano"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "o trzeciej"
             , "o 3 po południu"
             -- , "3 po południu" t14902576
             , "3 popołudniu"
             , "trzecia popoludniu"
             , "o trzeciej popoludniu"
             , "piętnasta godzina"
             , "15sta godzina"
             , "o piętnastej"
             , "o 15stej"
             ]
  , examples (datetime (2013, 2, 12, 18, 0, 0) Hour)
             [ -- "6 po południu" t14902576
               "6 popołudniu"
             , "szósta popoludniu"
             , "o szostej popoludniu"
             , "o 18stej"
             , "osiemnasta godzina"
             , "o osiemnastej"
             ]
  , examples (datetime (2013, 2, 12, 19, 0, 0) Hour)
             [ -- "7 po południu" t14902576
               "7 popołudniu"
             , "siódma popoludniu"
             , "o siodmej popoludniu"
             , "o dziewiętnastej"
             , "dziewietnasta godzina"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 wieczorem"
             , "8 popołudniu"
             , "osma w nocy"
             , "ósma wieczorem"
             , "dwudziesta godzina"
             ]
  , examples (datetime (2013, 2, 12, 21, 0, 0) Hour)
             [ "dziewiata wieczorem"
             , "dziewiąta popołudniu"
             , "dziewiata po południu"
             , "dziewiąta wieczorem"
             , "dziewiąta nocą"
             , "dziewiąta w nocy"
             , "9 wieczorem"
             , "9 popołudniu"
             -- , "9 po południu" t14902576
             , "9 wieczorem"
             , "9 nocą"
             , "9 w nocy"
             , "o dziewiatej w nocy"
             , "dwudziesta pierwsza godzina"
             , "dwudziestapierwsza godzina"
             ]
  , examples (datetime (2013, 2, 12, 22, 0, 0) Hour)
             [ "dziesiąta wieczorem"
             , "dziesiata popołudniu"
             , "dziesiata po południu"
             , "dziesiata wieczorem"
             , "dziesiata nocą"
             , "10 w nocy"
             , "o dziesiatej w nocy"
             , "o dwudziestej drugiej"
             ]
  , examples (datetime (2013, 2, 12, 23, 0, 0) Hour)
             [ "jedenasta wieczorem"
             , "jedenasta w nocy"
             , "11 w nocy"
             , "11 wieczorem"
             , "o jedenastej wieczorem"
             , "o dwudziestejtrzeciej"
             ]
  , examples (datetime (2013, 2, 13, 2, 0, 0) Hour)
             [ "jutro o drugiej"
             ]
  , examples (datetime (2013, 2, 14, 2, 0, 0) Hour)
             [ "po jutrze o drugiej"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "około 3 po południu"
             , "około trzeciej"
             , "koło trzeciej"
             , "o koło trzeciej"
             , "mniej wiecej o 3"
             , "tak o 15stej"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "jutro równo o piątej popołudniu"
             , "jutro równo o 17-stej"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "piętnaście po trzeciej"
             , "15 po trzeciej"
             , "kwadrans po 3"
             , "o trzecia piętnaście"
             , "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "20 po 3"
             , "3:20"
             , "3:20 w poludnie"
             , "o trzecia dwadzieścia"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "w pół do szesnastej"
             , "pol po trzeciej"
             , "15:30"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "3:30"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "kwadrans do południa"
             , "kwadrans przed południem"
             , "kwadrans do 12stej"
             , "11:45"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 wieczorem"
             , "8 tego wieczora"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "o 7:30 popołudniu Piatek, 20 Wrzesień"
             , "o 7:30 popołudniu Piatek, Wrzesień 20"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "o 9 rano w Sobote"
             , "w Sobote na 9 rano"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "Pia, Lip 18, 2014 19:00"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "w sekundę"
             , "za sekundę"
             , "sekunde od teraz"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ -- "za minutę" t14902624
               "za jedną minutę"
             , "przez minutę"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "w 2 minuty"
             , "za jeszcze 2 minuty"
             , "2 minuty od teraz"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "w 60 minut"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "w pół godziny"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "w 2.5 godziny"
             , "w 2 i pół godziny"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "w godzinę"
             , "w 1h"
             , "w przeciągu godziny"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "w kilka godzin"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "w 24 godziny"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "w jeden dzień"
             , "dzień od dziś"
             ]
  , examples (datetime (2016, 2, 0, 0, 0, 0) Month)
             [ "3 lata od dziś"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "w 7 dni"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "w jeden tydzień"
             , "w tydzień"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "za około pół godziny"
             , "za jakieś pół godziny"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 dni temu"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 dni temu"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "tydzien temu"
             , "jeden tydzień temu"
             , "1 tydzień temu"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "trzy tygodnie temu"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "trzy miesiące temu"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "dwa lata temu"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "7 dni potem"
             ]
  , examples (datetime (2013, 2, 26, 4, 0, 0) Hour)
             [ "14 dni później"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "tydzień później"
             , "jeden tydzień później"
             , "1 tydzień później"
             ]
  , examples (datetime (2013, 3, 5, 0, 0, 0) Day)
             [ "trzy tygodnie później"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "trzy miesiące później"
             ]
  , examples (datetime (2015, 2, 0, 0, 0, 0) Month)
             [ "dwa lata później"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "to lato"
             , "w to lato"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "ta zima"
             , "tej zimy"
             ]
  , examples (datetime (2013, 12, 24, 0, 0, 0) Day)
             [ "Wigilia Bożego Narodzenia"
             , "Wigilia"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "święta Bożego Narodzenia"
             , "boże narodzenie"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "sylwester"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "walentynki"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "Dzień Mamy"
             ]
  , examples (datetime (2013, 6, 16, 0, 0, 0) Day)
             [ "Dzień Taty"
             ]
  , examples (datetime (2013, 10, 31, 0, 0, 0) Day)
             [ "halloween"
             ]
  , examples (datetime (2013, 11, 28, 0, 0, 0) Day)
             [ "dzień dziękczynienia"
             , "dziękczynienie"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "ten wieczór"
             , "dzisiejszy wieczór"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "jutrzejszy wieczór"
             , "Środowy wieczór"
             , "jutrzejsza noc"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "wczorajszy wieczór"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "ten week-end"
             , "ten weekend"
             , "ten wekend"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "poniedziałkowy poranek"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "ostatnie 2 sekundy"
             , "ostatnie dwie sekundy"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "kolejne 3 sekundy"
             , "kolejne trzy sekundy"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "ostatnie 2 minuty"
             , "ostatnie dwie minuty"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "kolejne 3 minuty"
             , "nastepne trzy minuty"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "ostatnia 1 godzina"
             , "poprzednia jedna godzina"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "kolejne 3 godziny"
             , "kolejne trzy godziny"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "ostatnie 2 dni"
             , "ostatnie dwa dni"
             , "poprzednie 2 dni"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "nastepne 3 dni"
             , "nastepne trzy dni"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "nastepne kilka dni"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "ostatnie 2 tygodnie"
             , "ostatnie dwa tygodnie"
             , "poprzednie 2 tygodnie"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "nastepne 3 tygodnie"
             , "nastepne trzy tygodnie"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "ostatnie 2 miesiace"
             , "ostatnie dwa miesiące"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "nastepne trzy miesiące"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "ostatnie 2 lata"
             , "ostatnie dwa lata"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "nastepne 3 lata"
             , "kolejne trzy lata"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "Lipiec 13-15"
             , "Lipca 13 do 15"
             , "Lipiec 13 - Lipiec 15"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "Sie 8 - Sie 12"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "od 9:30 - 11:00 w Czwartek"
             -- , "miedzy 9:30 a 11:00 w czwartek" t14902649
             , "9:30 - 11:00 w czwartek"
             -- , "pozniej niż 9:30 ale przed 11:00 w Czwartek" t14902649
             , "Czwartek od 9:30 do 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "Czwartek od 9 rano do 11 rano"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-1:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "dziś wieczorem"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "w ciągu 2 tygodni"
             , "w ciągu dwóch tygodni"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Hour)
             [ "przed drugą po południu"
             , "przed drugą"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "dziś o drugiej w południe"
             , "o drugiej popołudniu"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Hour)
             [ "4/25 o 4 popołudniu"
             , "4/25 o 16"
             , "4/25 o szesnastej"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "3 popoludniu jutro"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "po drugiej po poludniu"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 17, 4, 0, 0) Hour)
             [ "po pięciu dniach"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "po drugiej po południu"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "przed 11 rano"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 13, 11, 0, 0) Hour)
             [ "jutro przed 11 rano"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "to w południe"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "w 15 minut"
             , "w piętnaście minut"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "nastepny pon"
             , "kolejny poniedziałek"
             ]
  ]
