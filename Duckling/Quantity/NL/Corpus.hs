-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.NL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Quantity.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Gram 2 Nothing)
             [ "2 gram"
             , "0.002 kg"
             , "2/1000 kilogram"
             , "2000 milligram"
             ]
  , examples (simple Gram 1000 Nothing)
             [ "een kilogram"
             , "een kilo"
             , "een kilootje"
             , "een kg"
             , "1 kilo"
             , "1 kg"
             , "1.0 kg"
             , "1 kilogram"
             , "1000 gram"
             , "1000 g"
             , "1000 gr"
             , "duizend gram"
             , "duizend gr"
             ]
  , examples (simple Gram 500 (Just "aardbeien"))
             [ "500 gram aardbeien"
             , "0.5 kilogram aardbeien"
             , "halve kilo aardbeien"
             , "0.5 kilo aardbeien"
             , "0.5 kg aardbeien"
             , "500000mg aardbeien"
             ]
  , examples (between Gram (100,1000) (Just "aardbeien"))
              [ "100-1000 gram aardbeien"
              , "tussen 100 en 1000 gram aardbeien"
              , "van 100 tot 1000 g aardbeien"
              , "100 - 1000 g aardbeien"
              ]
  , examples (between Gram (2,7) Nothing)
              [ "rond 2 -7 g"
              , "~2-7 gram"
              , "van 2 tot 7 g"
              , "tussen 2.0 g en ongeveer 7.0 g"
              , "tussen 0.002 kg en ongeveer 0.007 kg"
              , "2 - ~7 gram"
              ]
  ]
