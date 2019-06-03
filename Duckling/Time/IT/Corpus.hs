-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.IT.Corpus
  ( corpus
  , negativeCorpus
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
corpus = (testContext {locale = makeLocale IT Nothing}, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {locale = makeLocale IT Nothing}, testOptions, examples)
  where
    examples =
      [ "ma"
      , "3 20"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "subito"
             , "immediatamente"
             , "in questo momento"
             , "ora"
             , "adesso"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "di oggi"
             , "oggi"
             , "in giornata"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "ieri"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "domani"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "Il giorno dopo domani"
             , "dopodomani"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Lunedì 18 febbraio"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "martedì"
             , "Martedì 19"
             , "mar 19"
             , "il 19"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "l'altro ieri"
             , "altroieri"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "lunedi"
             , "lun"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "lunedi 18 febbraio"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "Martedì"
             , "Martedi"
             , "mar"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Mercoledì"
             , "mer"
             , "mer."
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "mercoledi 13 feb"
             , "il 13 febbraio"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "il 13 febbraio 2013"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "giovedi"
             , "giovedì"
             , "gio"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "venerdi"
             , "venerdì"
             , "ven"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "sabato"
             , "sab"
             , "sab."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "domenica"
             , "dom"
             , "dom."
             ]
   , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
              [ "gennaio 2013"
              , "genn 2013"
              , "genn. 2013"
              ]
  , examples (datetime (2013, 12, 1, 0, 0, 0) Month)
              [ "dicembre 2013"
              , "dic 2013"
              , "dic. 2013"
              ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "domenica 10 febbraio"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "il 1 marzo"
             , "primo marzo"
             , "primo di marzo"
             , "il 1º marzo"
             ]
  , examples (datetimeOpenInterval Before (2013, 3, 1, 0, 0, 0) Month)
             [ "prima di marzo"
             ]
  , examples (datetime (2013, 3, 15, 0, 0, 0) Day)
             [ "le idi di marzo"
             , "idi di marzo"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 marzo 2015"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "il 15 febbraio"
             , "15/2"
             , "il 15/02"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31/10/1974"
             , "31/10/74"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "martedì scorso"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "martedì prossimo"
             , "il martedì dopo"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "mercoledì prossimo"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "ottobre 2014"
             ]
  , examples (datetime (2013, 2, 12, 3, 0, 0) Hour)
             [ "l'ultima ora"
             , "nell'ultima ora"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "questa settimana"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "la settimana scorsa"
             , "la scorsa settimana"
             , "nella scorsa settimana"
             , "della settimana scorsa"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "la settimana prossima"
             , "la prossima settimana"
             , "nella prossima settimana"
             , "settimana prossima"
             , "prossima settimana"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "il mese scorso"
             , "nel mese scorso"
             , "nel mese passato"
             , "lo scorso mese"
             , "dello scorso mese"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "il mese prossimo"
             , "il prossimo mese"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "questo trimestre"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "il prossimo trimestre"
             , "nel prossimo trimestre"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "terzo trimestre"
             , "il terzo trimestre"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "quarto trimestre 2018"
             , "il quarto trimestre 2018"
             , "del quarto trimestre 2018"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "l'anno scorso"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "quest'anno"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "il prossimo anno"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "ultima domenica"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "lunedì di questa settimana"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "martedì di questa settimana"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "mercoledì di questa settimana"
             ]
  , examples (datetime (2013, 2, 14, 17, 0, 0) Hour)
             [ "dopo domani alle 17"
             , "dopodomani alle 5 del pomeriggio"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "ultimo lunedì di marzo"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "ultima domenica di marzo 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "il terzo giorno di ottobre"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "prima settimana di ottobre 2014"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ "la settimana del 6 ottobre"
             , "la settimana del 7 ott"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "il we del 15 febbraio"
             ]
  , examples (datetimeInterval ((2013, 4, 12, 18, 0, 0), (2013, 4, 15, 0, 0, 0)) Hour)
             [ "il week-end del 10 aprile"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "l'ultimo giorno di ottobre 2015"
             , "l'ultimo giorno dell'ottobre 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "l'ultima settimana di settembre 2014"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "tra un'ora"
             , "tra 1 ora"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "tra un quarto d'ora"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "tra mezz'ora"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Second)
             [ "tra tre quarti d'ora"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "primo martedì di ottobre"
             , "primo martedì in ottobre"
             , "1° martedì del mese di ottobre"
             , "1º martedì del mese di ottobre"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "terzo martedì di settembre 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "primo mercoledì di ottobre 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "secondo mercoledì di ottobre 2014"
             ]
  , examples (datetime (2015, 1, 13, 0, 0, 0) Day)
             [ "terzo martedì dopo natale 2014"
             ]
  , examples (datetime (2016, 1, 0, 0, 0, 0) Month)
             [ "il mese dopo natale 2015"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "alle 3 di pomeriggio"
             , "le tre di pomeriggio"
             , "alle 3 del pomeriggio"
             , "le tre del pomeriggio"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "circa alle 3 del pomeriggio"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "per le 15"
             , "verso le 15"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Minute)
             [ "3:00"
             , "03:00"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "15:15"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "3:15 di pomeriggio"
             , "3:15 del pomeriggio"
             , "3 e un quarto di pomeriggio"
             , "tre e un quarto di pomeriggio"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "alle tre e venti di pomeriggio"
             , "alle tre e venti del pomeriggio"
             , "3:20 di pomeriggio"
             , "3:20 del pomeriggio"
             , "15:20 del pomeriggio"
             ]
  , examples (datetime (2013, 2, 13, 3, 20, 0) Minute)
             [ "alle tre e venti"
             , "tre e 20"
             , "3 e 20"
             , "3:20"
             , "alle 3 20"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "15:30"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "a mezzogiorno meno un quarto"
             , "mezzogiorno meno un quarto"
             , "un quarto a mezzogiorno"
             , "11:45 del mattino"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "alle 3 del mattino"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "alle 19:30 di venerdì 20 settembre"
             , "alle 19:30 venerdì 20 settembre"
             , "venerdì 20 settembre alle 19:30"
             , "il 20 settembre alle 19:30"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "questo week-end"
             , "questo fine settimana"
             , "questo finesettimana"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "lunedi mattina"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "15 febbraio al mattino"
             , "mattino di 15 febbraio"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 di stasera"
             , "8 della sera"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "venerdì 20 settembre alle 7:30 del pomeriggio"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "alle 9 di sabato"
             , "sabato alle 9"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "quest'estate"
             , "questa estate"
             , "in estate"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "quest'inverno"
             , "questo inverno"
             , "in inverno"
             ]
  , examples (datetimeInterval ((2014, 9, 23, 0, 0, 0), (2014, 12, 22, 0, 0, 0)) Day)
             [ "il prossimo autunno"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "natale"
             , "il giorno di natale"
             ]
  , examples (datetime (2013, 12, 24, 0, 0, 0) Day)
             [ "vigilia di natale"
             , "alla vigilia"
             , "la vigilia"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "vigilia di capodanno"
             , "san silvestro"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2014, 1, 1, 4, 0, 0)) Hour)
             [ "notte di san silvestro"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "capodanno"
             , "primo dell'anno"
             ]
  , examples (datetime (2014, 1, 6, 0, 0, 0) Day)
             [ "epifania"
             , "befana"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "san valentino"
             , "festa degli innamorati"
             ]
  , examples (datetime (2013, 3, 19, 0, 0, 0) Day)
             [ "festa del papà"
             , "festa del papa"
             , "festa di san giuseppe"
             , "san giuseppe"
             ]
  , examples (datetime (2013, 4, 25, 0, 0, 0) Day)
             [ "anniversario della liberazione"
             , "la liberazione"
             , "alla liberazione"
             ]
  , examples (datetime (2013, 5, 1, 0, 0, 0) Day)
             [ "festa del lavoro"
             , "festa dei lavoratori"
             , "giorno dei lavoratori"
             , "primo maggio"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "festa della mamma"
             ]
  , examples (datetime (2013, 6, 2, 0, 0, 0) Day)
             [ "festa della repubblica"
             , "la repubblica"
             , "repubblica"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "ferragosto"
             , "assunzione"
             ]
  , examples (datetime (2013, 10, 31, 0, 0, 0) Day)
             [ "halloween"
             ]
  , examples (datetime (2013, 11, 1, 0, 0, 0) Day)
             [ "tutti i santi"
             , "ognissanti"
             , "festa dei santi"
             , "il giorno dei santi"
             ]
  , examples (datetime (2013, 11, 2, 0, 0, 0) Day)
             [ "giorno dei morti"
             , "commemorazione dei defunti"
             ]
  , examples (datetime (2013, 11, 2, 2, 0, 0) Hour)
             [ "ai morti alle 2"
             ]
  , examples (datetime (2013, 12, 8, 0, 0, 0) Day)
             [ "immacolata"
             , "immacolata concezione"
             , "all'immacolata"
             ]
  , examples (datetime (2013, 12, 8, 18, 0, 0) Hour)
             [ "all'immacolata alle 18"
             ]
  , examples (datetime (2013, 12, 26, 0, 0, 0) Day)
             [ "santo stefano"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "questa sera"
             , "sta sera"
             , "stasera"
             , "in serata"
             , "nella sera"
             , "verso sera"
             , "la sera"
             , "alla sera"
             , "la serata"
             , "nella serata"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 4, 0, 0), (2013, 2, 13, 12, 0, 0)) Hour)
             [ "domani mattina"
             , "domattina"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 18, 0, 0, 0)) Second)
             [ "in settimana"
             , "per la settimana"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 13, 4, 0, 0)) Hour)
             [ "stanotte"
             , "nella notte"
             , "in nottata"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "ultimo weekend"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "domani in serata"
             , "domani sera"
             , "nella serata di domani"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 0, 0, 0), (2013, 2, 14, 4, 0, 0)) Hour)
             [ "domani notte"
             , "domani in nottata"
             , "nella nottata di domani"
             , "nella notte di domani"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "domani a pranzo"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "ieri sera"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "questo weekend"
             , "questo week-end"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "lunedì mattina"
             , "nella mattinata di lunedì"
             , "lunedì in mattinata"
             , "lunedì nella mattina"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "il 15 febbraio in mattinata"
             , "mattina del 15 febbraio"
             , "15 febbraio mattina"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "gli ultimi 2 secondi"
             , "gli ultimi due secondi"
             , "i 2 secondi passati"
             , "i due secondi passati"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "i prossimi 3 secondi"
             , "i prossimi tre secondi"
             , "nei prossimi tre secondi"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "gli ultimi 2 minuti"
             , "gli ultimi due minuti"
             , "i 2 minuti passati"
             , "i due minuti passati"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "i prossimi 3 minuti"
             , "nei prossimi 3 minuti"
             , "i prossimi tre minuti"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 2, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "le ultime 2 ore"
             , "le ultime due ore"
             , "nelle ultime due ore"
             , "le scorse due ore"
             , "le due ore scorse"
             , "le scorse 2 ore"
             , "le 2 ore scorse"
             , "nelle 2 ore scorse"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 4, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "le ultime 24 ore"
             , "le ultime ventiquattro ore"
             , "le 24 ore passate"
             , "nelle 24 ore scorse"
             , "le ventiquattro ore passate"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "le prossime 3 ore"
             , "prossime tre ore"
             , "nelle prossime 3 ore"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "gli ultimi 2 giorni"
             , "gli ultimi due giorni"
             , "negli ultimi 2 giorni"
             , "i 2 giorni passati"
             , "i due giorni passati"
             , "nei due giorni passati"
             , "gli scorsi due giorni"
             , "i 2 giorni scorsi"
             , "i due giorni scorsi"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "prossimi 3 giorni"
             , "i prossimi tre giorni"
             , "nei prossimi 3 giorni"
             , "prossimi giorni"
             , "nei prossimi giorni"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "le ultime 2 settimane"
             , "le ultime due settimane"
             , "le 2 ultime settimane"
             , "le due ultime settimane"
             , "nelle 2 ultime settimane"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "prossime 3 settimane"
             , "le prossime tre settimane"
             , "le 3 prossime settimane"
             , "nelle prossime 3 settimane"
             , "le tre prossime settimane"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "gli ultimi 2 mesi"
             , "gli ultimi due mesi"
             , "i 2 mesi passati"
             , "nei 2 mesi passati"
             , "i due mesi passati"
             , "i due mesi scorsi"
             , "i 2 mesi scorsi"
             , "negli scorsi due mesi"
             , "gli scorsi due mesi"
             , "gli scorsi 2 mesi"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "i prossimi 3 mesi"
             , "i prossimi tre mesi"
             , "prossimi 3 mesi"
             , "i 3 prossimi mesi"
             , "i tre prossimi mesi"
             , "nei prossimi tre mesi"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "gli ultimi 2 anni"
             , "gli ultimi due anni"
             , "negli ultimi 2 anni"
             , "i 2 anni passati"
             , "i due anni passati"
             , "i 2 anni scorsi"
             , "i due anni scorsi"
             , "gli scorsi due anni"
             , "gli scorsi 2 anni"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "i prossimi 3 anni"
             , "i prossimi tre anni"
             , "nei tre prossimi anni"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13-15 luglio"
             , "dal 13 al 15 luglio"
             , "tra il 13 e il 15 luglio"
             , "tra 13 e 15 luglio"
             , "dal tredici al quindici luglio"
             , "13 luglio - 15 luglio"
             ]
  , examples (datetimeInterval ((2013, 3, 3, 0, 0, 0), (2013, 3, 6, 0, 0, 0)) Day)
             [ "dal 3 al 5"
             , "tra il 3 e il 5"
             , "dal tre al cinque"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "8 ago - 12 ago"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 15, 0, 0, 0)) Day)
             [ "da domani a giovedì"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "dalle 9:30 alle 11:00 di giovedì"
             , "tra le 9:30 e le 11:00 di giovedì"
             , "9:30 - 11:00 giovedì"
             , "giovedì dalle 9:30 alle 11:00"
             , "giovedì tra le 9:30 e le 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "dalle 9 alle 11 di giovedì"
             , "tra le 9 e le 11 di giovedì"
             , "9 - 11 giovedì"
             , "giovedì dalle nove alle undici"
             , "giovedì tra le nove e le undici"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 3, 0, 0), (2013, 2, 14, 14, 0, 0)) Hour)
             [ "dalle tre all'una di giovedì"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 13, 3, 0, 0)) Hour)
             [ "dalla mezzanotte alle 2"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 15, 0, 0), (2013, 2, 13, 17, 1, 0)) Minute)
             [ "domani dalle 15:00 alle 17:00"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-13:30"
             ]
  , examples (datetime (2013, 9, 21, 13, 30, 0) Minute)
             [ "13:30 di sabato 21 settembre"
             , "13:30 del 21 settembre"
             ]
  , examples (datetime (2013, 2, 26, 0, 0, 0) Day)
             [ "in due settimane"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 12, 14, 0, 0)) Second)
             [ "fino alle 14:00"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Minute)
             [ "entro le 14:00"
             ]
  , examples (datetimeOpenInterval Before (2013, 3, 1, 0, 0, 0) Month)
             [ "entro la fine del mese"
             ]
  , examples (datetimeOpenInterval Before (2014, 1, 1, 0, 0, 0) Year)
             [ "entro la fine dell'anno"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 3, 1, 0, 0, 0)) Second)
             [ "fino alla fine del mese"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2014, 1, 1, 0, 0, 0)) Second)
             [ "fino alla fine dell'anno"
             ]
  , examples (datetime (2013, 2, 13, 1, 0, 0) Minute)
             [ "alle 4 CET"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "alle 16 CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "giovedì alle 8:00 GMT"
             , "giovedì alle 8:00 gmt"
             ]
  , examples (datetime (2013, 2, 13, 14, 0, 0) Hour)
             [ "domani alle 14"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "alle 14"
             , "alle 2 del pomeriggio"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Minute)
             [ "25/4 alle 16:00"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "3 del pomeriggio di domani"
             , "15 del pomeriggio di domani"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "dopo le 14"
             , "dalle 14"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 13, 0, 0, 0) Hour)
             [ "dalla mezzanotte"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 13, 14, 0, 0) Hour)
             [ "domani dopo le 14"
             , "domani dalle 14"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "prima delle 11"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 14, 11, 0, 0) Hour)
             [ "dopodomani prima delle 11"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 14, 12, 0, 0) Hour)
             [ "giovedì entro mezzogiorno"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 14, 0, 0, 0) Day)
             [ "da dopodomani"
             , "da giovedì"
             ]
  , examples (datetimeOpenInterval After (2013, 3, 1, 0, 0, 0) Day)
             [ "dal primo"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 20, 0, 0, 0) Day)
             [ "dal 20"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 15, 0, 0, 0) Day)
             [ "entro il 15"
             ]
  , examples (datetimeOpenInterval Before (2013, 4, 20, 0, 0, 0) Day)
             [ "prima del 20 aprile"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "nel pomeriggio"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "alle 13:30"
             , "13:30"
             , "1:30 del pomeriggio"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "in 15 minuti"
             , "tra 15 minuti"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "questa mattina"
             , "questa mattinata"
             , "questo mattino"
             ]
  , examples (datetime (2013, 2, 25, 0, 0, 0) Day)
             [ "prossimo lunedì"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "alle 12"
             , "a mezzogiorno"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "alle 24"
             , "a mezzanotte"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "marzo"
             , "in marzo"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "gio 15"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 giorni fa"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "una settimana fa"
             , "1 settimana fa"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "tre settimane fa"
             , "3 settimane fa"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "tre mesi fa"
             , "3 mesi fa"
             ]
  , examples (datetime (2011, 2, 1, 0, 0, 0) Month)
             [ "due anni fa"
             , "2 anni fa"
             ]
  ]
