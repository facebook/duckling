-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.KM.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Distance.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext{locale = makeLocale KM Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 km"
             , "៣គីឡូ"
             , "បីគីឡូម៉ែត្រ"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "២សង់ទីម៉ែត្រ"
             , "ពីរសង់ទីម៉ែត្រ"
             ]
  , examples (between Metre (3, 5))
             [ "ចាប់ពី 3 ដល់ 5 m"
             , "ចន្លោះពី ៣ ដល់ ៥ម៉ែត្រ"
             , "ចន្លោះ ៣ម៉ែត្រ និង ៥ម៉ែត្រ"
             , "ប្រហែល ៣-៥ ម៉ែត្រ"
             , "~3-5ម៉ែត្រ"
             ]
  , examples (under Centimetre 4)
             [ "តិចជាងបួនសង់ទីម៉ែត្រ"
             , "មិនលើស៤សង់ទីម៉ែត្រ"
             , "ក្រោម៤សង់ទីម៉ែត្រ"
             , "យ៉ាងច្រើន៤សង់ទីម៉ែត្រ"
             ]
  , examples (above Millimetre 10)
             [ "ច្រើនជាងដប់មីលីម៉ែត្រ"
             , "មិនតិចជាងដប់មីលីម៉ែត្រ"
             , "លើសពីដប់មីលីម៉ែត្រ"
             , "យ៉ាងតិចដប់មីលីម៉ែត្រ"
             ]
  ]
