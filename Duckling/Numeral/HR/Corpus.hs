-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.HR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale HR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nula"
             , "nista"
             , "ništa"
             , "nistica"
             , "ništica"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "jedan"
             , "sam"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "dva"
             , "par"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "trideset i tri"
             , "trideset tri"
             , "0033"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "cetrnaest"
             , "četrnaest"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "šesnaest"
             , "sesnaest"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "sedamnaest"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "osamnaest"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "jedan cijela jedan"
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
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (NumeralValue 5000)
             [ "5 tisuća"
             , "pet tisuća"
             , "pet tisuca"
             ]
  , examples (NumeralValue 122)
             [ "stotinu dvadeset dva"
             ]
  , examples (NumeralValue 200000)
             [ "dvjesto tisuća"
             , "dvjesto tisuca"
             , "dvije stotine tisuca"
             , "dvije stotine tisuća"
             ]
  , examples (NumeralValue 21011)
             [ "dvadeset i jedna tisuca jedanaest"
             , "dvadeset i jedna tisuća jedanaest"
             ]
  , examples (NumeralValue 721012)
             [ "sedam stotina dvadeset jedna tisuća dvanaest"
             , "sedam stotina dvadeset jedna tisuca dvanaest"
             ]
  ]
