-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.AR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale AR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Ounce 3 (Just "الذهب"))
             [ "ثلاثة اونصات من الذهب"
             ]
  , examples (simple Gram 2 Nothing)
             [ "2 غرام"
             , "2 جرام"
             , "0.002 كيلوغرام"
             , "0.002 كيلوجرام"
             , "2/1000 كغ"
             , "2000 ملغ"
             , "2000 ملج"
             ]
  , examples (simple Gram 1000 Nothing)
             [ "كغ"
             , "كيلوغرام"
             , "كيلوجرام"
             ]
  , examples (simple Ounce 2 Nothing)
             [ "2 اونصة"
             , "أونصتان"
             , "اونصتين"
             ]
  , examples (simple Cup 3 (Just "السكر"))
             [ "3 اكواب من السكر"
             ]
  , examples (simple Cup 0.75 Nothing)
             [ "3/4 كوب"
             , "0.75 كوب"
             , ".75 كوب"
             ]
  , examples (simple Gram 500 (Just "الفراولة"))
             [ "500 غرام من الفراولة"
             , "500 غم من الفراولة"
             , "500 جرام من الفراولة"
             , "500 جم من الفراولة"
             , "0.5 كيلوجرام من الفراولة"
             , "0.5 كيلوغرام من الفراولة"
             , "0.5 كغ من الفراولة"
             , "500000 ملغ من الفراولة"
             ]
  ]
