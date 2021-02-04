-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.ZH.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing},
  testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Cent 5)
             [ "五分"
             , "5分"
             , "五仙"
             ]
  , examples (simple Cent 20)
             [ "20分"
             , "二十分"
             , "2角"
             , "两毛"
             , "两毫"
             ]
  , examples (simple Cent 25)
             [ "25分"
             , "二十五分"
             , "两角五分"
             , "两毛五"
             ]
  , examples (simple Dollar 7)
             [ "7块"
             , "七元"
             , "七蚊"
             ]
  , examples (simple Dollar 5.5)
             [ "五個半"
             ]
  , examples (simple Dollar 1.5)
             [ "個半"
             , "一個半"
             ]
  , examples (simple Dollar 1.9)
             [ "個九"
             ]
  , examples (simple CNY 3.14)
             [ "3.14人民币"
             , "人民幣3.14"
             ]
  , examples (simple HKD 3.14)
             [ "3.14港幣"
             , "港幣3.14"
             , "港幣三個一毫四"
             ]
  , examples (under Dollar 1.2)
             [ "1.2元以下"
             , "最多一块二角"
             , "最多一块二"
             ]
  , examples (above Dollar 3.04)
             [ "3.04块以上"
             , "至少三块四分"
             , "至少三块零四"
             , "起碼三蚊零四"
             ]
  , examples (between Dollar (5.6, 5.78))
             [ "5.6到5.78元"
             , "五元六角-五元七毛八分"
             , "五块六到五块七毛八"
             , "五蚊六毫至五蚊七毫八"
             ]
  ]
