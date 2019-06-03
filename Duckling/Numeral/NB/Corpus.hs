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
  [ examples (NumeralValue 0)
             [ "0"
             , "null"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "én"
             , "en"
             , "Ett"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "to"
             , "et par"
             ]
  , examples (NumeralValue 7)
             [ "7"
             , "syv"
             , "sju"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "fjorten"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "seksten"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "sytten"
             , "søtten"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "atten"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "tyve"
             , "Tjue"
             ]
  , examples (NumeralValue 30)
             [ "30"
             , "tretti"
             , "tredve"
             ]
  , examples (NumeralValue 70)
             [ "70"
             , "søtti"
             , "sytti"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             , "100 000"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             , "3 000 000"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             , "1 200 000"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             , "negativ 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             , "-1 200 000"
             ]
  , examples (NumeralValue 5000)
             [ "5 tusen"
             , "fem tusen"
             , "5 000"
             ]
  , examples (NumeralValue 100)
             [ "hundre"
             ]
  , examples (NumeralValue 5020)
             [ "fem tusen og tjue"
             ]
  , examples (NumeralValue 1e9)
             [ "en milliard"
             ]
  , examples (NumeralValue 1e12)
             [ "en billion"
             ]
  , examples (NumeralValue 1e15)
             [ "en billiard"
             ]
  , examples (NumeralValue 1e18)
             [ "en trillion"
             ]
  , examples (NumeralValue 1e21)
             [ "en trilliard"
             ]
  , examples (NumeralValue 1e24)
             [ "en kvadrillion"
             ]
  , examples (NumeralValue 1e27)
             [ "en kvadrilliard"
             ]
  , examples (NumeralValue 6e10)
             [ "seksti milliarder"
             , "60 milliarder"
             ]
  , examples (NumeralValue 3e12)
             [ "tre billioner"
             , "3 billioner"
             ]
  , examples (NumeralValue 4e17)
             [ "fire hundre billiarder"
             , "400 billiarder"
             ]
  , examples (NumeralValue 2e18)
             [ "to trillioner"
             , "2 trillioner"
             ]
  , examples (NumeralValue 1.8e22)
             [ "atten trilliarder"
             , "18 trilliarder"
             ]
  , examples (NumeralValue 1.8e22)
             [ "atten trilliarder"
             , "18 trilliarder"
             ]
  , examples (NumeralValue 4.11e26)
             [ "fire hundre og elleve kvadrillioner"
             , "411 kvadrillioner"
             ]
  , examples (NumeralValue 9e27)
             [ "ni kvadrilliarder"
             , "9 kvadrilliarder"
             ]
  , examples (NumeralValue 21)
             [ "tjueen"
             , "tjueén"
             ]
  , examples (NumeralValue 22)
             [ "tjueto"
             ]
  , examples (NumeralValue 23)
             [ "tjuetre"
             ]
  , examples (NumeralValue 24)
             [ "tjuefire"
             ]
  , examples (NumeralValue 25)
             [ "tjuefem"
             ]
  , examples (NumeralValue 26)
             [ "tjueseks"
             ]
  , examples (NumeralValue 27)
             [ "tjuesju"
             , "tjuesyv"
             ]
  , examples (NumeralValue 28)
             [ "tjueåtte"
             ]
  , examples (NumeralValue 29)
             [ "tjueni"
             ]
  , examples (NumeralValue 31)
             [ "trettien"
             , "trettién"
             ]
  , examples (NumeralValue 32)
             [ "trettito"
             ]
  , examples (NumeralValue 33)
             [ "trettitre"
             ]
  , examples (NumeralValue 34)
             [ "trettifire"
             ]
  , examples (NumeralValue 35)
             [ "trettifem"
             ]
  , examples (NumeralValue 36)
             [ "trettiseks"
             ]
  , examples (NumeralValue 37)
             [ "trettisju"
             , "trettisyv"
             ]
  , examples (NumeralValue 38)
             [ "trettiåtte"
             ]
  , examples (NumeralValue 39)
             [ "trettini"
             ]
  , examples (NumeralValue 41)
             [ "førtien"
             , "førtién"
             ]
  , examples (NumeralValue 42)
             [ "førtito"
             ]
  , examples (NumeralValue 43)
             [ "førtitre"
             ]
  , examples (NumeralValue 44)
             [ "førtifire"
             ]
  , examples (NumeralValue 45)
             [ "førtifem"
             ]
  , examples (NumeralValue 46)
             [ "førtiseks"
             ]
  , examples (NumeralValue 47)
             [ "førtisju"
             , "førtisyv"
             ]
  , examples (NumeralValue 48)
             [ "førtiåtte"
             ]
  , examples (NumeralValue 49)
             [ "førtini"
             ]
  , examples (NumeralValue 51)
             [ "femtien"
             , "femtién"
             ]
  , examples (NumeralValue 52)
             [ "femtito"
             ]
  , examples (NumeralValue 53)
             [ "femtitre"
             ]
  , examples (NumeralValue 54)
             [ "femtifire"
             ]
  , examples (NumeralValue 55)
             [ "femtifem"
             ]
  , examples (NumeralValue 56)
             [ "femtiseks"
             ]
  , examples (NumeralValue 57)
             [ "femtisju"
             , "femtisyv"
             ]
  , examples (NumeralValue 58)
             [ "femtiåtte"
             ]
  , examples (NumeralValue 59)
             [ "femtini"
             ]
  , examples (NumeralValue 61)
             [ "sekstien"
             , "sekstién"
             ]
  , examples (NumeralValue 62)
             [ "sekstito"
             ]
  , examples (NumeralValue 63)
             [ "sekstitre"
             ]
  , examples (NumeralValue 64)
             [ "sekstifire"
             ]
  , examples (NumeralValue 65)
             [ "sekstifem"
             ]
  , examples (NumeralValue 66)
             [ "sekstiseks"
             ]
  , examples (NumeralValue 67)
             [ "sekstisju"
             , "sekstisyv"
             ]
  , examples (NumeralValue 68)
             [ "sekstiåtte"
             ]
  , examples (NumeralValue 69)
             [ "sekstini"
             ]
  , examples (NumeralValue 71)
             [ "syttien"
             , "søttien"
             , "søttién"
             , "søttién"
             ]
  , examples (NumeralValue 72)
             [ "syttito"
             , "søttito"
             ]
  , examples (NumeralValue 73)
             [ "syttitre"
             , "søttitre"
             ]
  , examples (NumeralValue 74)
             [ "syttifire"
             , "søttifire"
             ]
  , examples (NumeralValue 75)
             [ "syttifem"
             , "søttifem"
             ]
  , examples (NumeralValue 76)
             [ "syttiseks"
             , "søttiseks"
             ]
  , examples (NumeralValue 77)
             [ "syttisju"
             , "syttisju"
             , "søttisyv"
             , "søttisyv"
             ]
  , examples (NumeralValue 78)
             [ "søttiåtte"
             , "søttiåtte"
             ]
  , examples (NumeralValue 79)
             [ "søttini"
             , "søttini"
             ]
  , examples (NumeralValue 81)
             [ "åttien"
             , "åttién"
             ]
  , examples (NumeralValue 82)
             [ "åttito"
             ]
  , examples (NumeralValue 83)
             [ "åttitre"
             ]
  , examples (NumeralValue 84)
             [ "åttifire"
             ]
  , examples (NumeralValue 85)
             [ "åttifem"
             ]
  , examples (NumeralValue 86)
             [ "åttiseks"
             ]
  , examples (NumeralValue 87)
             [ "åttisju"
             , "åttisyv"
             ]
  , examples (NumeralValue 88)
             [ "åttiåtte"
             ]
  , examples (NumeralValue 89)
             [ "åttini"
             ]
  , examples (NumeralValue 91)
             [ "nittien"
             , "nittién"
             ]
  , examples (NumeralValue 92)
             [ "nittito"
             ]
  , examples (NumeralValue 93)
             [ "nittitre"
             ]
  , examples (NumeralValue 94)
             [ "nittifire"
             ]
  , examples (NumeralValue 95)
             [ "nittifem"
             ]
  , examples (NumeralValue 96)
             [ "nittiseks"
             ]
  , examples (NumeralValue 97)
             [ "nittisju"
             , "nittisyv"
             ]
  , examples (NumeralValue 98)
             [ "nittiåtte"
             ]
  , examples (NumeralValue 99)
             [ "nittini"
             ]
  ]
