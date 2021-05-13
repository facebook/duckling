-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.CA.Corpus
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
corpus = (testContext {locale = makeLocale CA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 30 Minute)
             [ "mitja hora"
             , "dos quarts"
             , "30 minuts"
             , "trenta minuts"
             ]
  , examples (DurationData 15 Minute)
             [  "quart d'hora"
             ,  "un quart d'hora"
             ,  "1 quart d'hora"
             ,  "1/4 d'hora"
             ,  "quinze minuts"
             ,  "15 minuts"
             ]
  , examples (DurationData 45 Minute)
             [  "tres quarts d'hora"
             ,  "3/4 d'hora"
             ,  "3 quarts d'hora"
             ,  "tres quarts"
             ,  "quaranta-cinc minuts"
             ,  "45 minuts"
             ]
  , examples (DurationData 92 Minute)
             [ "una hora i trenta-dos minuts"
             , "una hora trenta-dos minuts"
             ]
  , examples (DurationData 155 Minute)
             [ "dues hores i trenta-cinc minuts"
             , "dues hores trenta-cinc minuts"
             ]
  ]
