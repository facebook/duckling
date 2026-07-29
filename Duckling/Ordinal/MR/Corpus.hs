-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.MR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "पहिला"
             , "प्रथम"
             , "पहिल्या"
             ]
  , examples (OrdinalData 2)
             [ "दुसरा"
             , "दुसऱ्या"
             ]
  , examples (OrdinalData 3)
             [ "तिसरा"
             ]
  , examples (OrdinalData 11)
             [ "अकरावा"
             , "अकरावे"
             ]
  , examples (OrdinalData 23)
             [ "तेविसावे"
             , "तेविसावा"
             , "तेविसावी"
             , "तेविसाव्या"
             , "23वे"
             , "२३वे"
             , "२३व्या"
             ]
  ]
