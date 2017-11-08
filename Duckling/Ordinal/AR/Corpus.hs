-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.AR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale AR Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "الاول"
             , "الأول"
             ]
  , examples (OrdinalData 2)
             [ "الثاني"
             , "الثان"
             ]
  , examples (OrdinalData 3)
             [ "الثالث"
             ]
  , examples (OrdinalData 4)
             [ "الرابع"
             ]
  , examples (OrdinalData 8)
             [ "الثامن"
             ]
  , examples (OrdinalData 11)
             [ "الأحد عشر"
             , "الإحدى عشرة"
             , "الحادي عشرة"
             ]
  , examples (OrdinalData 12)
             [ "الثاني عشرة"
             , "الثان عشر"
             , "الاثنى عشر"
             ]
  , examples (OrdinalData 25)
             [ "الخامس والعشرين"
             , "الخامس و العشرون"
             ]
  , examples (OrdinalData 31)
             [ "الواحد والثلاثون"
             , "الواحد والثلاثين"
             ]
  , examples (OrdinalData 72)
             [ "الثان والسبعون"
             , "الثاني والسبعين"
             ]
  , examples (OrdinalData 90)
             [ "التسعون"
             , "التسعين"
             ]
  ]
