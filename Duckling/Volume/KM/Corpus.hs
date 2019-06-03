-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.KM.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Volume.Types
import Duckling.Testing.Types

context :: Context
context = testContext{locale = makeLocale KM Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Litre 1)
             [ "1 លីត្រ"
             , "1l"
             , "១លីត្រ"
             ]
  , examples (simple Litre 0.5)
             [ "កន្លះលីត្រ"
             , "១/២លីត្រ"
             ]
  , examples (simple Litre 0.25)
             [ "មួយភាគបួនលីត្រ"
             , "១/៤លីត្រ"
             ]
  , examples (simple Millilitre 1)
             [ "1 មីលីលីត្រ"
             , "1ml"
             , "១មីលីលីត្រ"
             ]
 , examples (between Litre (2,7))
             [ "ចាប់ពី 2 ដល់ 7 l"
             , "ចន្លោះពី ២ ដល់ ៧លីត្រ"
             , "ចន្លោះ ២លីត្រ និង ៧លីត្រ"
             , "ប្រហែល ២-៧ លីត្រ"
             , "~2-7លីត្រ"
             ]
  , examples (under Millilitre 500)
             [ "តិចជាងប្រាំរយមីលីលីត្រ"
             , "មិនលើសប្រាំរយមីលីលីត្រ"
             , "ក្រោមប្រាំរយមីលីលីត្រ"
             , "យ៉ាងច្រើនប្រាំរយមីលីលីត្រ"
             ]
  , examples (above Millilitre 500)
             [ "ច្រើនជាងប្រាំរយមីលីលីត្រ"
             , "មិនតិចជាងប្រាំរយមីលីលីត្រ"
             , "លើសពីប្រាំរយមីលីលីត្រ"
             , "យ៉ាងតិចប្រាំរយមីលីលីត្រ"
             ]
  ]
