-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.ZH.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Litre 1)
             [ "1升"
             , "一升"
             , "一公升"
             , "1L"
             ]
  , examples (simple Litre 2)
             [ "2升"
             , "兩升"
             , "兩公升"
             , "2L"
             ]
  , examples (simple Litre 1000)
             [ "1000公升"
             , "一千公升"
             ]
  , examples (simple Litre 0.5)
             [ "半公升"
             , "0.5L"
             ]
  , examples (simple Litre 0.25)
             [ "四分一升"
             , "四分之一公升"
             ]
  , examples (simple Millilitre 1)
             [ "1毫升"
             , "1ml"
             , "一毫升"
             , "1cc"
             ]
  , examples (simple Millilitre 250)
             [ "250毫升"
             , "二百五十毫升"
             , "250ml"
             , "250cc"
             ]
  , examples (simple Gallon 3)
             [ "3加侖"
             , "三加侖"
             ]
  , examples (simple Gallon 0.5)
             [ "0.5加侖"
             , "半加侖"
             , "二分一加侖"
             ]
  , examples (simple Gallon 0.1)
             [ "0.1加侖"
             , "零點一加侖"
             ]
  , examples (between Litre (2,3))
             [ "二至三公升"
             , "2-3L"
             , "2~3公升"
             , "兩到三升"
             , "兩升到三升"
             ]
  , examples (under Gallon 6)
             [ "最多六個加侖"
             , "六加侖以下"
             ]
  , examples (above Millilitre 4)
             [ "至少四ml"
             , "最少四毫升"
             , "四毫升或以上"
             ]
  , examples (simple Millilitre 5)
             [ "一茶匙"
             , "三分一湯匙"
             ]
  ]
