-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.AR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale AR Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (QuantityData Ounce 3 (Just "الذهب"))
             [ "ثلاثة اونصات من الذهب"
             ]
  , examples (QuantityData Gram 2 Nothing)
             [ "2 غرام"
             , "0.002 كيلوغرام"
             , "2/1000 كغ"
             , "2000 ملغ"
             ]
  , examples (QuantityData Gram 1000 Nothing)
             [ "كغ"
             , "كيلوغرام"
             ]
  , examples (QuantityData Ounce 2 Nothing)
             [ "2 اونصة"
             , "أونصتان"
             , "اونصتين"
             ]
  , examples (QuantityData Cup 3 (Just "السكر"))
             [ "3 اكواب من السكر"
             ]
  , examples (QuantityData Cup 0.75 Nothing)
             [ "3/4 كوب"
             , "0.75 كوب"
             , ".75 كوب"
             ]
  , examples (QuantityData Gram 500 (Just "الفراولة"))
             [ "500 غرام من الفراولة"
             , "500 غم من الفراولة"
             , "0.5 كيلوغرام من الفراولة"
             , "0.5 كغ من الفراولة"
             , "500000 ملغ من الفراولة"
             ]
  ]
