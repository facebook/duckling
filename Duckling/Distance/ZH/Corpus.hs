-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.ZH.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Centimetre 2)
             [ "2cm"
             , "2 厘米"
             , "二厘米"
             , "2公分"
             , "二公分"
             ]
  , examples (simple Metre 9)
             [ "9m"
             , "9 m"
             , "九米"
             , "9公尺"
             ]
  , examples (simple Kilometre 3)
             [ "3公里"
             , "三公裏"
             , "3 km"
             , "3km"
             ]
  , examples (simple Kilometre 3.0)
             [ "3.0 公里"
             ]
  , examples (simple Foot 8)
             [ "8 foot"
             , "8 feet"
             , "8 foots"
             , "8 feets"
             , "8'"
             , "8英尺"
             , "8呎"
             ]
  , examples (simple Inch 4)
             [ "4 inch"
             , "4 inches"
             , "4''"
             , "4英寸"
             , "4英吋"
             , "四吋"
             ]
  , examples (simple Mile 1)
             [ "1 mile"
             , "1 miles"
             , "1英里"
             , "一英裏"
             ]
  , examples (simple Metre 1.9)
             [ "1.9 m"
             , "1.9米"
             , "米九"
             ]
  , examples (simple Metre 3.9)
             [ "3.9 m"
             , "3.9米"
             , "三米九"
             ]
  , examples (between Metre (1.6, 3.9))
             [ "1.6m-3.9m"
             , "1.6~3.9米"
             , "一點六至三點九公尺"
             , "米六到三米九"
             ]
  , examples (under Mile 3.5)
             [ "最多三點五英里"
             , "3.5英裏以下"
             ]
  , examples (above Inch 5)
             [ "至少5\""
             , "最少五吋"
             , "五英吋以上"
             , "起碼五英寸"
             ]
  ]
