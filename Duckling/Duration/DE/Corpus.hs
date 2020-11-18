-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.DE.Corpus
  ( corpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext {locale = makeLocale DE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "ein sekunde"
             , "zirka eine sekunde"
             ]
  , examples (DurationData 30 Minute)
             [ "1/2 stunde"
             , "1/2stunde"
             , "eine halbe stunde"
             , "einer halben stunde"
             , "halbe stunde"
             , "halben stunde"
             , "ungefahr einer halben stunde"
             ]
  , examples (DurationData 15 Minute)
             [ "einer Viertelstunde"
             , "eine viertelstunde"
             , "ViErTelStUnDe"
             , "genau viertelstunde"
             ]
  , examples (DurationData 45 Minute)
             [ "3/4 stunde"
             , "3/4stunde"
             , "eine dreiviertel stunde"
             , "einer dreiviertel stunde"
             , "dreiviertel stunde"
             , "drei viertelstunden"
             ]
  , examples (DurationData 92 Minute)
             [ "92 minuten"
             , "zweiundneunzig minuten"
             , "eine Stunde und zweiunddreißig Minuten"
             , "ein stunde und zweiunddreissig minuten"
             ]
  , examples (DurationData 30 Day)
             [ "30 tage"
             , "dreißig tage"
             , "dreissig tage"
             ]
  , examples (DurationData 7 Week)
             [ "7 Wochen"
             , "sieben wochen"
             ]
  , examples (DurationData 90 Minute)
             [ "1,5 stunden"
             , "1,5 stunde"
             , "90 min"
             , "90min"
             ]
  , examples (DurationData 75 Minute)
             [ "1,25 stunden"
             ]
  , examples (DurationData 31719604 Second)
             [ "1 Jahr, 2 Tage, 3 Stunden und 4 Sekunden"
             , "1 Jahr 2 Tage 3 Stunden und 4 Sekunden"
             ]
  , examples (DurationData 330 Second)
             [ "5 und eine halbe Minuten"
             , "5,5 min"
             ]
  , examples (DurationData 330 Minute)
             [ "5 und eine halbe stunden"
             , "5,5 stunde"
             , "exakt 5,5 stunden"
             ]
  , examples (DurationData 930 Second)
              [ "15,5 minuten"
              , "15,5 minute"
              , "15,5 min"
              ]
  ]
