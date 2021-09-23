-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HU.Corpus
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
corpus = (testContext {locale = makeLocale HU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "most"
             , "épp most"
             , "azonnal"
             , "mostani"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "ma"
             , "máma"
             , "Mai nap"
             , "Mai napot"
             , "mai napon"
             , "mai"
             , "mait"
             , "mai napi"
             , "mai napit"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "holnap"
             , "holnapi"
             , "holnapit"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "holnapután"
             , "holnaputáni"
             , "holnaputánit"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "tegnap"
             , "tegnapi"
             , "tegnapit"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "tegnapelőtt"
             , "tegnapelőtti"
             , "tegnapelőttit"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "hónap vége"
             , "a hónap vége"
             , "hó vége"
             , "hó végi"
             , "hó végit"
             , "hó végén"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "év vége"
             , "az év vége"
             , "év végi"
             , "év végit"
             , "év végén"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "hétfő"
             , "hétfőn"
             , "hétfőt"
             , "hétfői"
             , "hétfőit"
             , "hét."
             , "hét"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "kedd"
             , "kedden"
             , "keddet"
             , "keddi"
             , "keddit"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "szerda"
             , "szerdán"
             , "szerdát"
             , "szerdai"
             , "szerdait"
             , "szer"
             , "szer."
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "csütörtök"
             , "csütörtökön"
             , "csütörtököt"
             , "csütörtöki"
             , "csütörtökit"
             , "csüt"
             , "csüt."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "péntek"
             , "pénteken"
             , "pénteket"
             , "pénteki"
             , "péntekit"
             , "pén"
             , "pén."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "szombat"
             , "szombaton"
             , "szombatot"
             , "szombati"
             , "szombatit"
             , "szom"
             , "szom."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "vasárnap"
             , "vasárnapot"
             , "vasárnapi"
             , "vasárnapit"
             , "vas"
             , "vas."
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Month)
             [ "január"
             , "januárban"
             , "januári"
             , "januárit"
             , "jan"
             , "jan."
             ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Month)
             [ "február"
             , "februárban"
             , "februári"
             , "februárit"
             , "feb"
             , "feb."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "március"
             , "márciusban"
             , "márciusi"
             , "márciusit"
             , "már"
             , "már."
             , "márc"
             , "márc."
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Month)
             [ "április"
             , "áprilisban"
             , "áprilisi"
             , "áprilisit"
             , "ápr"
             , "ápr."
             ]
  , examples (datetime (2013, 5, 1, 0, 0, 0) Month)
             [ "május"
             , "májusban"
             , "májusi"
             , "májusit"
             , "máj"
             , "máj."
             ]
  , examples (datetime (2013, 6, 1, 0, 0, 0) Month)
             [ "június"
             , "júniusban"
             , "júniusi"
             , "júniusit"
             , "jún"
             , "jún."
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Month)
             [ "július"
             , "júliusban"
             , "júliusi"
             , "júliusit"
             , "júl"
             , "júl."
             ]
  , examples (datetime (2013, 8, 1, 0, 0, 0) Month)
             [ "augusztus"
             , "augusztusban"
             , "augusztusi"
             , "augusztusit"
             , "aug"
             , "aug."
             ]
  , examples (datetime (2013, 9, 1, 0, 0, 0) Month)
             [ "szeptember"
             , "szeptemberben"
             , "szeptemberi"
             , "szeptemberit"
             , "szep"
             , "szep."
             , "szept"
             , "szept."
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Month)
             [ "október"
             , "októberben"
             , "októberi"
             , "októberit"
             , "okt"
             , "okt."
             ]
  , examples (datetime (2013, 11, 1, 0, 0, 0) Month)
             [ "november"
             , "novemberben"
             , "novemberi"
             , "novemberit"
             , "nov"
             , "nov."
             ]
  , examples (datetime (2013, 12, 1, 0, 0, 0) Month)
             [ "december"
             , "decemberben"
             , "decemberi"
             , "decemberit"
             , "dec"
             , "dec."
             ]
  , examples (datetime (2013, 3, 15, 0, 0, 0) Day)
             [ "március 15"
             , "március 15."
             , "már 15"
             , "már. 15"
             , "márc 15"
             , "márc. 15"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
            [ "március 1-én"
            , "március 1-jén"
            , "március 1-je"
            , "március 1-jei"
            , "március 1-i"
            , "március 1én"
            , "március 1je"
            , "március 1jei"
            , "március 1i"
            ]
  , examples (datetime (2013, 3, 2, 0, 0, 0) Day)
            [ "március 2-án"
            , "március 2-ai"
            , "március 2-a"
            , "március 2án"
            , "március 2ai"
            , "március 2a"
            ]
  , examples (datetime (2013, 3, 4, 0, 0, 0) Day)
            [ "március 4-e"
            , "március 4e"
            ]
  , examples (datetime (2013, 3, 7, 0, 0, 0) Day)
            [ "március 7-ei"
            , "március 7ei"
            ]
  , examples (datetime (2013, 3, 15, 0, 0, 0) Day)
            [ "március 15-én"
            , "március 15-ei"
            , "március 15-i"
            , "március 15én"
            , "március 15e"
            , "március 15ei"
            ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "következő hónap"
             , "jövő hónap"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "előző év"
             , "múlt év"
             ]
  , examples (datetime (2013, 2, 21, 0, 0, 0) Day)
             [ "jövő csütörtök"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "15:20"
             ]
  , examples (datetime (2013, 2, 12, 8, 20, 0) Minute)
             [ "08:20"
             , "8:20"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 44) Second)
             [ "15:20:44"
             ]
  , examples (datetime (2013, 2, 12, 8, 20, 44) Second)
             [ "08:20:44"
             , "8:20:44"
             ]
  , examples (datetime (2013, 2, 12, 11, 0, 0) Hour)
             [ "de 11"
             , "de. 11"
             , "délelőtt 11"
             ]
  , examples (datetime (2013, 2, 12, 23, 0, 0) Hour)
             [ "du 11"
             , "du. 11"
             , "délután 11"
             ]
  , examples (datetime (2013, 2, 13, 23, 0, 0) Hour)
             [ "szerda du 11"
             , "szerda du. 11"
             , "szerda délután 11"
             ]
  , examples (datetime (2013, 2, 20, 23, 0, 0) Hour)
             [ "jövő szerda du 11"
             , "jövő szerda du. 11"
             , "jövő szerda délután 11"
             ]
  , examples (datetime (2013, 8, 20, 0, 0, 0) Day)
             [ "2013.08.20"
             , "2013 . 08 . 20"
             , "2013-08-20"
             , "2013 - 08 - 20"
             ]
  , examples (datetime (2013, 8, 20, 11, 45, 0) Minute)
             [ "2013.08.20 11:45"
             ]
  , examples (datetime (2013, 8, 20, 17, 0, 0) Hour)
             [ "2013.08.20 délután 5"
             ]
  , examples (datetime (2013, 8, 20, 0, 0, 0) Day)
             [ "08.20"
             , "08 . 20"
             , "08-20"
             , "08 - 20"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 6, 0, 0), (2013, 2, 12, 10, 0, 0)) Hour)
             [ "ma reggel"
             , "reggel"
             , "ma reggeli"
             , "reggeli"
             , "ma reggelit"
             , "reggelit"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 8, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "délelőtt"
             , "délelőtti"
             , "délelőttit"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 13, 0, 0)) Hour)
             [ "délben"
             , "déli"
             , "délit"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 8, 0, 0), (2013, 2, 13, 12, 0, 0)) Hour)
             [ "holnap délelőtt"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 12, 0, 0), (2013, 2, 11, 13, 0, 0)) Hour)
             [ "tegnap délben"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 18, 0, 0)) Hour)
             [ "ma délután"
             , "délután"
             , "délutáni"
             , "délutánit"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 16, 0, 0), (2013, 2, 12, 20, 0, 0)) Hour)
             [ "ma este"
             , "este"
             , "esti"
             , "estit"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 20, 0, 0), (2013, 2, 12, 23, 0, 0)) Hour)
             [ "ma éjszaka"
             , "éjszaka"
             , "éjszakai"
             , "éjszakait"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "nyár"
             , "nyári"
             , "nyárit"
             , "nyáron"
             ]
   , examples (datetimeInterval ((2013, 9, 23, 0, 0, 0), (2013, 12, 22, 0, 0, 0)) Day)
             [ "ősz"
             , "őszi"
             , "őszit"
             , "ősszel"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "tél"
             , "téli"
             , "télit"
             , "télen"
             ]
    , examples (datetimeInterval ((2013, 3, 20, 0, 0, 0), (2013, 6, 22, 0, 0, 0)) Day)
             [ "tavasz"
             , "tavaszi"
             , "tavaszit"
             , "tavasszal"
             ]
  ]
