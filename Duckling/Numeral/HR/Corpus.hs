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
  [ examples (simple 0)
             [ "0"
             , "nula"
             , "nista"
             , "ništa"
             , "nistica"
             , "ništica"
             ]
  , examples (simple 1)
             [ "1"
             , "jedan"
             , "sam"
             ]
  , examples (simple 2)
             [ "2"
             , "dva"
             , "par"
             ]
  , examples (simple 33)
             [ "33"
             , "trideset i tri"
             , "trideset tri"
             , "0033"
             ]
  , examples (simple 14)
             [ "14"
             , "cetrnaest"
             , "četrnaest"
             ]
  , examples (simple 16)
             [ "16"
             , "šesnaest"
             , "sesnaest"
             ]
  , examples (simple 17)
             [ "17"
             , "sedamnaest"
             ]
  , examples (simple 18)
             [ "18"
             , "osamnaest"
             ]
  , examples (simple 1.1)
             [ "1,1"
             , "jedan cijela jedan"
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
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (simple 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (simple (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (simple 5000)
             [ "5 tisuća"
             , "pet tisuća"
             , "pet tisuca"
             ]
  , examples (simple 122)
             [ "stotinu dvadeset dva"
             ]
  , examples (simple 200000)
             [ "dvjesto tisuća"
             , "dvjesto tisuca"
             , "dvije stotine tisuca"
             , "dvije stotine tisuća"
             ]
  , examples (simple 21011)
             [ "dvadeset i jedna tisuca jedanaest"
             , "dvadeset i jedna tisuća jedanaest"
             ]
  , examples (simple 721012)
             [ "sedam stotina dvadeset jedna tisuća dvanaest"
             , "sedam stotina dvadeset jedna tisuca dvanaest"
             ]
  ]
