-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.NL.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext {locale = makeLocale NL Nothing}, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {locale = makeLocale NL Nothing}, testOptions, examples)
  where
    examples =
      [ "voor maanden"
      , "in enkele dagen"
      , "secretaris"
      , "last minute"
      , "12 uurtje"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "een seconde"
             , "één seconde"
             , "1 secondes"
             , "1 s"
             , "1\""
             ]
  , examples (DurationData 14 Second)
             [ "veertien seconden"
             , "14 s"
             ]
  , examples (DurationData 2 Minute)
             [ "2 min"
             , "twee minuten"
             , "2 m"
             , "2'"
             ]
  , examples (DurationData 45 Minute)
             [ "3 kwartier"
             ]
  , examples (DurationData 1 Hour)
             [ "een uur"
             , "één uur"
             , "1 u"
             , "1 h"
             ]
  , examples (DurationData 30 Day)
             [ "30 dagen"
             ]
  , examples (DurationData 7 Day)
             [ "7 dagen"
             ]
  , examples (DurationData 7 Week)
             [ "zeven weken"
             , "7 w"
             ]
  , examples (DurationData 1 Month)
             [ "1 mnd"
             , "een maand"
             , "één maand"
             ]
  , examples (DurationData 3 Quarter)
             [ "drie kwartalen"
             ]
  , examples (DurationData 2 Year)
             [ "2 jaar"
             , "2 jaren"
             , "twee jaren"
             , "2 j"
             ]
  , examples (DurationData 150 Minute)
            [ "twee en een half uur"
            , "2,5 uur"
            ]
  ]
