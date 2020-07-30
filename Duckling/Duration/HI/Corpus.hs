-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.HI.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Duration.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext {locale = makeLocale HI Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (single 15 Minute)
             [ "पंद्रह मिनट"
             , "लगभग पंद्रह मिनट"
             ]
  , examples (single 30 Minute)
             [ "आधा घंटा"
             ]
  , examples (single 1 Day)
             [ "दिवस"
             , "एक दिन"
             , "बिल्कुल एक दिन"
             ]
  , examples (single 14 Day)
             [ "पखवाड़ा"
             , "एक पखवाड़ा"
             ]
  , examples (single 1 Year)
             [ "एक साल"
             , "केवल एक वर्ष"
             , "लगभग एक साल"
             , "एक बरस"
             , "केवल एक साल"
             ]
  ]
