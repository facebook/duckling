-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.KM.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale KM Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Cup 3 Nothing)
             [ "បីកែវ"
             ]
  , examples (simple (Custom "Person") 1 Nothing)
             [ "មួយនាក់"
             ]
  , examples (simple (Custom "Flower") 1 (Just "កុលាប"))
             [ "កុលាប១ទង"
             ]
  , examples (simple (Custom "Building") 15 (Just "ផ្ទះ"))
             [ "ផ្ទះដប់ប្រាំខ្នង"
             ]
  , examples (simple (Custom "Person") 8 (Just "បងប្អូន"))
             [ "បងប្អូន៨នាក់"
             ]
  ]
