-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.GU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale GU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "પહેલું"
             , "પ્રથમ"
             , "પહેલા"
             ]
  , examples (OrdinalData 2)
             [ "બીજું"
             , "બીજા"
             ]
  , examples (OrdinalData 3)
             [ "ત્રીજું"
             ]
  , examples (OrdinalData 11)
             [ "અગિયારમું"
             ]
  , examples (OrdinalData 23)
             [ "તેવીસમું"
             , "તેવીસમો"
             , "તેવીસમી"
             , "23મું"
             , "૨૩મું"
             ]
  ]
