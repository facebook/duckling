-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.KM.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext{locale = makeLocale KM Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Cup 3 Nothing)
             [ "បីកែវ"
             ]
  , examples (simple Bowl 1 Nothing)
             [ "១ចាន"
             ]
  , examples (simple Pint 15 Nothing)
             [ "ដប់ប្រាំថូ"
             ]
  , examples (simple (Custom "For Persons") 2 (Just "មនុស្ស"))
             [ "មនុស្ស២នាក់"
             , "មនុស្សពីរនាក់"
             ]
  , examples (simple (Custom "For Buildings") 8 (Just "ផ្ទះ"))
             [ "ផ្ទះ៨ខ្នង"
             , "ផ្ទះប្រាំបីខ្នង"
             ]
  , examples (simple Gram 1000 Nothing)
             [ "មួយពាន់ក្រាម"
             , "មួយគីឡូក្រាម"
             , "មួយលានមីលីក្រាម"
             ]
  , examples (simple (Custom "Meters") 1000 Nothing)
             [ "មួយពាន់ម៉ែត្រ"
             , "មួយគីឡូម៉ែត្រ"
             , "មួយលានមីលីម៉ែត្រ"
             ]
  , examples (simple (Custom "Liters") 5 Nothing)
             [ "៥លីត្រ"
             , "ប្រាំលីត្រ"
             , "ប្រាំពាន់មីលីលីត្រ"
             ]
  ]
