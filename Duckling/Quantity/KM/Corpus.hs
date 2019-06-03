-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
  , examples (between Gram (2,7) Nothing)
             [ "ចាប់ពី 2 ដល់ 7 ក្រាម"
             , "ចន្លោះពី ២ ដល់ ៧ក្រាម"
             , "ចន្លោះ ២ក្រាម និង ៧ក្រាម"
             , "ប្រហែល ២-៧ ក្រាម"
             , "~2-7ក្រាម"
             ]
  , examples (under Tablespoon 4 Nothing)
             [ "តិចជាងបួនស្លាបព្រា"
             , "មិនលើស៤ស្លាបព្រា"
             , "ក្រោម៤ស្លាបព្រា"
             , "យ៉ាងច្រើន៤ស្លាបព្រា"
             ]
  , examples (above Bowl 10 Nothing)
             [ "ច្រើនជាងដប់ចាន"
             , "មិនតិចជាងដប់ចាន"
             , "លើសពីដប់ចាន"
             , "យ៉ាងតិចដប់ចាន"
             ]
  ]
