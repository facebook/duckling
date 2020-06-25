-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.ES.Corpus
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
corpus = (testContext {locale = makeLocale ES Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 30 Minute)
             [ "media hora"
             , "media horas"
             , "30 minutos"
             , "treinta minutos"
             ]
  , examples (DurationData 15 Minute)
             [  "cuarto de hora"
             ,  "cuartos de hora"
             ,  "quince minutos"
             ,  "15 minutos"
             ]
  , examples (DurationData 45 Minute)
             [  "tres cuartos de hora"
             ,  "tres cuartos de horas"
             ,  "tres cuarto de hora"
             ,  "tres cuarto de horas"
             ,  "cuarenta y cinco minutos"
             ,  "45 minutos"
             ]
  , examples (DurationData 92 Minute)
             [
               "una hora y treinta y dos minutos"
             , "una hora treinta y dos minutos"
             ]
  , examples (DurationData 155 Minute)
             [
               "dos hora y treinta y cinco minutos"
             , "dos hora  treinta y cinco minutos"
             ]
  ]
