-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.TR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "yok"
             , "hiç"
             , "sıfır"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "bir"
             , "tek"
             , "yek"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "iki"
             , "çift"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "otuzüç"
             , "otuz üç"
             , "0033"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "ondört"
             , "on dört"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "onaltı"
             , "on altı"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "onyedi"
             , "on yedi"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "onsekiz"
             , "on sekiz"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             , "bir virgül bir"
             , "bir nokta bir"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , ",77"
             , "sıfır virgül yetmişyedi"
             , "sıfır virgül yetmiş yedi"
             ]
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             , "100b"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             , "1200B"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "eksi 1.200.000"
             , "negatif 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             , "-1200B"
             ]
  , examples (NumeralValue 5000)
             [ "5 bin"
             , "beş bin"
             ]
  , examples (NumeralValue 50)
             [ "5 deste"
             , "beş deste"
             ]
  , examples (NumeralValue 200000)
             [ "iki yüz bin"
             , "ikiyüzbin"
             ]
  , examples (NumeralValue 21011)
             [ "yirmi bir bin on bir"
             , "yirmibir bin onbir"
             ]
  , examples (NumeralValue 721012)
             [ "yedi yüz yirmibir bin on iki"
             , "yedi yüz yirmi bir bin on iki"
             , "yediyüz yirmibir bin oniki"
             ]
  , examples (NumeralValue 300341)
             [ "üçyüzbin üçyüz kırkbir"
             , "üç yüz bin üç yüz kırk bir"
             ]
  , examples (NumeralValue 40348)
             [ "kırkbin üçyüz kırksekiz"
             , "kırk bin üç yüz kırk sekiz"
             ]
  , examples (NumeralValue 31256721)
             [ "otuz bir milyon iki yüz elli altı bin yedi yüz yirmi bir"
             ]
  , examples (NumeralValue 107)
             [ "107"
             , "yüz yedi"
             ]
  , examples (NumeralValue 5.5)
             [ "beş buçuk"
             , "beşbuçuk"
             , "5 buçuk"
             , "5,5"
             ]
  , examples (NumeralValue 3500000)
             [ "3,5 milyon"
             , "3500000"
             , "üç buçuk milyon"
             , "üçbuçuk milyon"
             , "3,5M"
             ]
  , examples (NumeralValue 0.5)
             [ "yarım"
             , "0,5"
             ]
  , examples (NumeralValue 2500)
             [ "2,5 bin"
             , "2500"
             , "iki bin beş yüz"
             , "ikibin beşyüz"
             ]
  , examples (NumeralValue 2200000)
             [ "2,2 milyon"
             , "iki nokta iki milyon"
             , "iki virgül iki milyon"
             ]
  , examples (NumeralValue 72.5)
             [ "yetmişikibuçuk"
             , "yetmişiki buçuk"
             , "yetmiş iki buçuk"
             , "72,5"
             ]
  ]
