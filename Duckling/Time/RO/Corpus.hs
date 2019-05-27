-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RO.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "sa"
      ]

corpus :: Corpus
corpus = (testContext {locale = makeLocale RO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "acum"
             , "chiar acum"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "azi"
             , "astazi"
             , "astăzi"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "ieri"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "maine"
             , "mâine"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "luni"
             , "lunea asta"
             , "lunea aceasta"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Luni, 18 Feb"
             , "Luni, 18 Februarie"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "marti"
             , "marți"
             , "Marti 19"
             , "Marti pe 19"
             , "Marți 19"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "joi"
             , "jo"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "vineri"
             , "vi"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "sambata"
             , "sâmbătă"
             , "sam"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "duminica"
             , "duminică"
             , "du"
             , "dum"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1 martie"
             , "intai martie"
             ]
  , examples (datetimeInterval ((2013, 6, 19, 0, 0, 0), (2013, 6, 21, 0, 0, 0)) Day)
             [ "iunie 19-20"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "ziua de craciun"
             , "crăciun"
             ]
  ]
