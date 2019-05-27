-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.HU.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Duration.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext {locale = makeLocale HU Nothing}, testOptions, allExamples)


allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "egy másodperc"
             , "1 másodperc"
             ]
  , examples (DurationData 2 Minute)
             [ "2 perc"
             , "kettő perc"
             ]
  , examples (DurationData 15 Minute)
             [ "negyed óra"
             , "negyedóra"
             , "negyed-óra"
             ]
  , examples (DurationData 30 Minute)
             [ "fél óra"
             , "félóra"
             , "fél-óra"
             ]
  , examples (DurationData 45 Minute)
             [ "háromnegyed óra"
             , "háromnegyed-óra"
             , "háromnegyedóra"
             , "3/4 óra"
             , "3/4óra"
             ]
  , examples (DurationData 150 Minute)
             [ "2.5 óra"
             , "kettő és fél óra"
             , "kettő és fél-óra"
             , "kettő és félóra"
             ]
  , examples (DurationData 30 Day)
             [ "30 nap"
             ]
  , examples (DurationData 7 Week)
             [ "hét hét"
             ]
  , examples (DurationData 1 Month)
             [ "egy hónap"
             ]
  , examples (DurationData 3 Quarter)
             [ "3 negyedév"
             ]
  , examples (DurationData 2 Year)
             [ "2 év"
             ]
  ]
