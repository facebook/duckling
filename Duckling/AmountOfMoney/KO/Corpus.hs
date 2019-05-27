-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.KO.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale KO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "십달러"
             , "십불"
             ]
  , examples (simple Cent 10)
             [ "십센트"
             ]
  , examples (simple Dollar 10000)
             [ "만달러"
             , "만불"
             ]
  , examples (simple Dollar 1.23)
             [ "일점이삼달러"
             , "일쩜이삼달러"
             , "일점이삼불"
             , "일쩜이삼불"
             ]
  , examples (simple Dollar 2.23)
             [ "이달러이십삼센트"
             , "이불이십삼센트"
             ]
  , examples (simple EUR 20)
             [ "이십유로"
             , "EUR 20"
             ]
  , examples (simple EUR 29.99)
             [ "이십구점구구유로"
             , "EUR29.99"
             ]
  , examples (simple Pound 9)
             [ "구파운드"
             ]
  , examples (simple KRW 27350000)
             [ "이천칠백삼십오만원"
             , "27,350,000원"
             , "27350000원"
             ]
  , examples (simple KRW 27000)
             [ "이만칠천원"
             , "27,000원"
             , "27000원"
             ]
  , examples (simple KRW 100)
             [ "백원"
             , "100원"
             ]
  , examples (simple KRW 10)
             [ "십원"
             , "10원"
             , "10₩"
             ]
  , examples (between KRW (25000, 30000))
             [ "25000 - 30000원"
             , "25000원 - 30000원"
             , "이만오천원 - 삼만원"
             ]
  , examples (above KRW 5000)
             [ "5000원 이상"
             , "5000원 초과"
             , "오천원 이상"
             , "오천원 초과"
             ]
  , examples (under KRW 10000)
             [ "10000원 이하"
             , "10000원 미만"
             , "만원 이하"
             , "만원 미만"
             ]
  , examples (between Dollar (10, 20))
             [ "10 - 20 달러"
             , "10달러 - 20달러"
             , "십달러 - 이십달러"
             ]
  , examples (above Dollar 10)
             [ "10달러 이상"
             , "10달러 초과"
             , "십달러 이상"
             , "십달러 초과"
             ]
  , examples (under Dollar 10)
             [ "10달러 이하"
             , "10달러 미만"
             , "십달러 이하"
             , "십달러 미만"
             ]
  ]
