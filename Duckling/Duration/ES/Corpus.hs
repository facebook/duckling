-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.ES.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "cuatro meses"
      , "en días"
      , "minutos"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "un seg"
             , "1 segundo"
             , "1\""
             ]
  , examples (DurationData 2 Minute)
             [ "2 mins"
             , "dos minutos"
             , "2'"
             , "2 minutos más"
             , "dos minutos adicionales"
             , "2 minutos extras"
             , "2 minutos menos"
             ]
  , examples (DurationData 30 Day)
             [ "30 días"
             ]
  , examples (DurationData 7 Week)
             [ "siete semanas"
             ]
  , examples (DurationData 1 Month)
             [ "1 mes"
             , "un mes"
             ]
  , examples (DurationData 3 Quarter)
             [ "3 cuartos"
             ]
  , examples (DurationData 2 Year)
             [ "2 años"
             ]
  , examples (DurationData 30 Minute)
             [ "media hora"
             , "treinta minutos"
             , "1/2 hora"
             , "1/2h"
             , "1/2 h"
             ]
  , examples (DurationData 12 Hour)
             [ "medio día"
             , "mitad de día"
             , "1/2 día"
             ]
  , examples (DurationData 90 Minute)
             [ "una hora y media"
             , "hora y media"
             ]
  , examples (DurationData 45 Day)
             [ "un mes y medio"
             , "mes y medio"
             ]
  , examples (DurationData 27 Month)
             [ "2 años y tres meses"
             , "2 años, tres meses"
             , "2 años tres meses"
             ]
  , examples (DurationData 31719604 Second)
             [ "1 año, 2 días, 3 horas y 4 segundos"
             , "1 año 2 días 3 horas y 4 segundos"
               -- Oxford comma not supported:
--           , "1 year, 2 days, 3 hours, and 4 seconds"
             ]
  , examples (DurationData 330 Second)
             [ "5 minutos y medio"
             , "cinco min y medio"
             ]
  ]
