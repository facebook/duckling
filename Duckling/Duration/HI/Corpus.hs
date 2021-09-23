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
  [ examples (DurationData 15 Minute)
             [ "पंद्रह मिनट"
             , "लगभग पंद्रह मिनट"
             ]
  , examples (DurationData 30 Minute)
             [ "आधा घंटा"
             ]
  , examples (DurationData 1 Day)
             [ "दिवस"
             , "एक दिन"
             , "बिल्कुल एक दिन"
             ]
  , examples (DurationData 14 Day)
             [ "पखवाड़ा"
             , "एक पखवाड़ा"
             ]
  , examples (DurationData 1 Year)
             [ "एक साल"
             , "केवल एक वर्ष"
             , "लगभग एक साल"
             , "एक बरस"
             , "केवल एक साल"
             ]
  , examples (DurationData 21 Month)
             [ "पौने 2 साल"
             ]
  , examples (DurationData 105 Minute)
             [ "पौने 2 घंटा"
             ]
  , examples (DurationData 27 Month)
             [ "सवा 2 साल"
             , "2 साल और 3 महीना"
             , "2 साल, 3 महीना"
             , "2 साल 3 महीना"
             ]
  , examples (DurationData 135 Minute)
             [ "सवा 2 घंटा"
             ]
  , examples (DurationData 42 Month)
             [ "साड़े 3 साल"
             ]
  , examples (DurationData 210 Minute)
             [ "साड़े 3 घंटा"
             ]
  , examples (DurationData 31719604 Second)
             [ "1 साल, 2 दिन, 3 घंटा और 4 सेकंड"
             , "1 साल 2 दिन 3 घंटा और 4 सेकंड"
             ]
  , examples (DurationData 30 Month)
             [ "ढाई साल"
             ]
  , examples (DurationData 90 Minute)
             [ "डेढ़ घंटा"
             ]
  ]
