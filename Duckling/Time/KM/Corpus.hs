-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.KM.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

context :: Context
context = testContext{locale = makeLocale KM Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "ឥឡូវ"
             , "ឥឡូវនេះ"
             , "ឥលូវនេះ"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "ថ្ងៃនេះ"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "ម្សិល"
             , "ម្សិលមិញ"
             , "ថ្ងៃម្សិលមិញ"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "ស្អែក"
             , "ថ្ងៃស្អែក"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "ច័ន្ទ"
             , "ថ្ងៃច័ន្ទ"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "អង្គារ"
             , "ថ្ងៃអង្គារ"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "ខែមីនា"
             ]
  ]
