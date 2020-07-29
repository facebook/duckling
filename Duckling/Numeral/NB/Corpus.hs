-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.NB.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale NB Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "null"
             ]
  , examples (simple 1)
             [ "1"
             , "én"
             , "en"
             , "Ett"
             ]
  , examples (simple 2)
             [ "2"
             , "to"
             , "et par"
             ]
  , examples (simple 7)
             [ "7"
             , "syv"
             , "sju"
             ]
  , examples (simple 14)
             [ "14"
             , "fjorten"
             ]
  , examples (simple 16)
             [ "16"
             , "seksten"
             ]
  , examples (simple 17)
             [ "17"
             , "sytten"
             , "søtten"
             ]
  , examples (simple 18)
             [ "18"
             , "atten"
             ]
  , examples (simple 20)
             [ "20"
             , "tyve"
             , "Tjue"
             ]
  , examples (simple 30)
             [ "30"
             , "tretti"
             , "tredve"
             ]
  , examples (simple 70)
             [ "70"
             , "søtti"
             , "sytti"
             ]
  , examples (simple 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (simple 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (simple 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             , "100 000"
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             , "3 000 000"
             ]
  , examples (simple 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             , "1 200 000"
             ]
  , examples (simple (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             , "negativ 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             , "-1 200 000"
             ]
  , examples (simple 5000)
             [ "5 tusen"
             , "fem tusen"
             , "5 000"
             ]
  , examples (simple 100)
             [ "hundre"
             ]
  , examples (simple 5020)
             [ "fem tusen og tjue"
             ]
  , examples (simple 1e9)
             [ "en milliard"
             ]
  , examples (simple 1e12)
             [ "en billion"
             ]
  , examples (simple 1e15)
             [ "en billiard"
             ]
  , examples (simple 1e18)
             [ "en trillion"
             ]
  , examples (simple 1e21)
             [ "en trilliard"
             ]
  , examples (simple 1e24)
             [ "en kvadrillion"
             ]
  , examples (simple 1e27)
             [ "en kvadrilliard"
             ]
  , examples (simple 6e10)
             [ "seksti milliarder"
             , "60 milliarder"
             ]
  , examples (simple 3e12)
             [ "tre billioner"
             , "3 billioner"
             ]
  , examples (simple 4e17)
             [ "fire hundre billiarder"
             , "400 billiarder"
             ]
  , examples (simple 2e18)
             [ "to trillioner"
             , "2 trillioner"
             ]
  , examples (simple 1.8e22)
             [ "atten trilliarder"
             , "18 trilliarder"
             ]
  , examples (simple 1.8e22)
             [ "atten trilliarder"
             , "18 trilliarder"
             ]
  , examples (simple 4.11e26)
             [ "fire hundre og elleve kvadrillioner"
             , "411 kvadrillioner"
             ]
  , examples (simple 9e27)
             [ "ni kvadrilliarder"
             , "9 kvadrilliarder"
             ]
  , examples (simple 21)
             [ "tjueen"
             , "tjueén"
             ]
  , examples (simple 22)
             [ "tjueto"
             ]
  , examples (simple 23)
             [ "tjuetre"
             ]
  , examples (simple 24)
             [ "tjuefire"
             ]
  , examples (simple 25)
             [ "tjuefem"
             ]
  , examples (simple 26)
             [ "tjueseks"
             ]
  , examples (simple 27)
             [ "tjuesju"
             , "tjuesyv"
             ]
  , examples (simple 28)
             [ "tjueåtte"
             ]
  , examples (simple 29)
             [ "tjueni"
             ]
  , examples (simple 31)
             [ "trettien"
             , "trettién"
             ]
  , examples (simple 32)
             [ "trettito"
             ]
  , examples (simple 33)
             [ "trettitre"
             ]
  , examples (simple 34)
             [ "trettifire"
             ]
  , examples (simple 35)
             [ "trettifem"
             ]
  , examples (simple 36)
             [ "trettiseks"
             ]
  , examples (simple 37)
             [ "trettisju"
             , "trettisyv"
             ]
  , examples (simple 38)
             [ "trettiåtte"
             ]
  , examples (simple 39)
             [ "trettini"
             ]
  , examples (simple 41)
             [ "førtien"
             , "førtién"
             ]
  , examples (simple 42)
             [ "førtito"
             ]
  , examples (simple 43)
             [ "førtitre"
             ]
  , examples (simple 44)
             [ "førtifire"
             ]
  , examples (simple 45)
             [ "førtifem"
             ]
  , examples (simple 46)
             [ "førtiseks"
             ]
  , examples (simple 47)
             [ "førtisju"
             , "førtisyv"
             ]
  , examples (simple 48)
             [ "førtiåtte"
             ]
  , examples (simple 49)
             [ "førtini"
             ]
  , examples (simple 51)
             [ "femtien"
             , "femtién"
             ]
  , examples (simple 52)
             [ "femtito"
             ]
  , examples (simple 53)
             [ "femtitre"
             ]
  , examples (simple 54)
             [ "femtifire"
             ]
  , examples (simple 55)
             [ "femtifem"
             ]
  , examples (simple 56)
             [ "femtiseks"
             ]
  , examples (simple 57)
             [ "femtisju"
             , "femtisyv"
             ]
  , examples (simple 58)
             [ "femtiåtte"
             ]
  , examples (simple 59)
             [ "femtini"
             ]
  , examples (simple 61)
             [ "sekstien"
             , "sekstién"
             ]
  , examples (simple 62)
             [ "sekstito"
             ]
  , examples (simple 63)
             [ "sekstitre"
             ]
  , examples (simple 64)
             [ "sekstifire"
             ]
  , examples (simple 65)
             [ "sekstifem"
             ]
  , examples (simple 66)
             [ "sekstiseks"
             ]
  , examples (simple 67)
             [ "sekstisju"
             , "sekstisyv"
             ]
  , examples (simple 68)
             [ "sekstiåtte"
             ]
  , examples (simple 69)
             [ "sekstini"
             ]
  , examples (simple 71)
             [ "syttien"
             , "søttien"
             , "søttién"
             , "søttién"
             ]
  , examples (simple 72)
             [ "syttito"
             , "søttito"
             ]
  , examples (simple 73)
             [ "syttitre"
             , "søttitre"
             ]
  , examples (simple 74)
             [ "syttifire"
             , "søttifire"
             ]
  , examples (simple 75)
             [ "syttifem"
             , "søttifem"
             ]
  , examples (simple 76)
             [ "syttiseks"
             , "søttiseks"
             ]
  , examples (simple 77)
             [ "syttisju"
             , "syttisju"
             , "søttisyv"
             , "søttisyv"
             ]
  , examples (simple 78)
             [ "søttiåtte"
             , "søttiåtte"
             ]
  , examples (simple 79)
             [ "søttini"
             , "søttini"
             ]
  , examples (simple 81)
             [ "åttien"
             , "åttién"
             ]
  , examples (simple 82)
             [ "åttito"
             ]
  , examples (simple 83)
             [ "åttitre"
             ]
  , examples (simple 84)
             [ "åttifire"
             ]
  , examples (simple 85)
             [ "åttifem"
             ]
  , examples (simple 86)
             [ "åttiseks"
             ]
  , examples (simple 87)
             [ "åttisju"
             , "åttisyv"
             ]
  , examples (simple 88)
             [ "åttiåtte"
             ]
  , examples (simple 89)
             [ "åttini"
             ]
  , examples (simple 91)
             [ "nittien"
             , "nittién"
             ]
  , examples (simple 92)
             [ "nittito"
             ]
  , examples (simple 93)
             [ "nittitre"
             ]
  , examples (simple 94)
             [ "nittifire"
             ]
  , examples (simple 95)
             [ "nittifem"
             ]
  , examples (simple 96)
             [ "nittiseks"
             ]
  , examples (simple 97)
             [ "nittisju"
             , "nittisyv"
             ]
  , examples (simple 98)
             [ "nittiåtte"
             ]
  , examples (simple 99)
             [ "nittini"
             ]
  ]
