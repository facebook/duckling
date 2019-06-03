-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.MN.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MN Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "нэг дүгээр"
             , "нэг дэх"
             , "1-р"
             ]
  , examples (OrdinalData 3)
             [ "гурав дахь"
             , "гурав дугаар"
             , "3-р"
             ]
  , examples (OrdinalData 4)
             [ "дөрөв дэх"
             , "4-р"
             ]
  , examples (OrdinalData 15)
             [ "15-р"
             ]
  , examples (OrdinalData 21)
             [ "21-р"
             ]
  , examples (OrdinalData 23)
             [ "23-р"
             ]
  , examples (OrdinalData 31)
             [ "31-р"
             ]
  , examples (OrdinalData 48)
             [ 
               "48-р"
             ]
  , examples (OrdinalData 99)
             [ "99-р"
             ]
  ]
