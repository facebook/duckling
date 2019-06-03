-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.NL.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale NL Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Gram 2 Nothing)
             [ "2 gram"
             , "0,002 kg"
             , "0,002 kilo"
             , "2/1000 kilogram"
             , "2000 milligram"
             ]
  , examples (simple Gram 1000 Nothing)
             [ "1 kg"
             , "1,0 kg"
             , "1 kilogram"
             , "1 kilo"
             , "1000 gram"
             , "1000 g"
             , "1000 gr"
             , "duizend gram"
             , "duizend gr"
             , "2,0 pond"
             , "10 ons"
             , "1000000 mg"
             , "1000000 milligram"
             ]
  , examples (simple Cup 1 (Just "suiker"))
             [ "1 kopje suiker"
             ]
  , examples (simple Cup 3 (Just "suiker"))
             [ "3 kopjes suiker"
             ]
  , examples (simple Cup 0.75 Nothing)
             [ "3/4 kopje"
             , "0,75 kopje"
             , ",75 kopje"
             ]
  , examples (simple Gram 500 (Just "aardbeien"))
             [ "500 gram aardbeien"
             , "500g aardbeien"
             , "0,5 kilogram aardbeien"
             , "0,5 kg aardbeien"
             , "5 ons aardbeien"
             , "1 pond aardbeien"
             , "500000mg aardbeien"
             ]
  , examples (between Gram (100,1000) (Just "aardbeien"))
              [ "100-1000 gram aardbeien"
              , "tussen 100 en 1000 gram aardbeien"
              , "van 100 tot 1000 g aardbeien"
              , "tussen 1 ons en 10 ons aardbeien"
              , "100 - 1000 g aardbeien"
              ]
  , examples (between Gram (2,7) Nothing)
              [ "~2-7 gram"
              , "van 2 tot 7 g"
              , "tussen 2,0 g en ongeveer 7,0 g"
              , "tussen 0,002 kg en ongeveer 0,007 kg"
              , "2 - ~7 gram"
              ]
  ]
