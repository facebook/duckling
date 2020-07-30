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
  [ examples (single 1 Second)
             [ "egy másodperc"
             , "1 másodperc"
             ]
  , examples (single 2 Minute)
             [ "2 perc"
             , "kettő perc"
             ]
  , examples (single 15 Minute)
             [ "negyed óra"
             , "negyedóra"
             , "negyed-óra"
             ]
  , examples (single 30 Minute)
             [ "fél óra"
             , "félóra"
             , "fél-óra"
             ]
  , examples (single 45 Minute)
             [ "háromnegyed óra"
             , "háromnegyed-óra"
             , "háromnegyedóra"
             , "3/4 óra"
             , "3/4óra"
             ]
  , examples (single 150 Minute)
             [ "2.5 óra"
             , "kettő és fél óra"
             , "kettő és fél-óra"
             , "kettő és félóra"
             ]
  , examples (single 30 Day)
             [ "30 nap"
             ]
  , examples (single 7 Week)
             [ "hét hét"
             ]
  , examples (single 1 Month)
             [ "egy hónap"
             ]
  , examples (single 3 Quarter)
             [ "3 negyedév"
             ]
  , examples (single 2 Year)
             [ "2 év"
             ]
  ]
