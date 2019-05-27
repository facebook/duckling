-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.RO.Corpus
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
corpus = (testContext {locale = makeLocale RO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "o sec"
             , "1 secunda"
             ]
  , examples (DurationData 30 Minute)
             [ "jumatate ora"
             , "1/2h"
             , "treizeci minute"
             ]
  , examples (DurationData 45 Minute)
             [ "trei sferturi de oră"
             , "45min"
             , "45 de minute"
             ]
  , examples (DurationData 12 Week)
             [ "doișpe saptamanile"
             , "doisprezece saptămâni"
             ]
  , examples (DurationData 2 Month)
             [ "2 luni"
             ]
  , examples (DurationData 1 Quarter)
             [ "un trimestru"
             ]
  ]
