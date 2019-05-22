-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.GA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {locale = makeLocale GA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "anois"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "inniu"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "inné"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "arú inné"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "amárach"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "arú amárach"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "dé luain"
             , "an luan"
             , "an luan seo"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "an luan seo chugainn"
             , "an luan seo atá ag teacht"
             , "dé luain seo chugainn"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "18/2/2013"
             ]
  ]
