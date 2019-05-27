-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.KA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Ordinal.Types
import Duckling.Testing.Types

import Duckling.Locale
import Duckling.Resolve

corpus :: Corpus
corpus = (testContext {locale = makeLocale KA Nothing}, testOptions, allExamples)


allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "პირველი"
             , "1-ლი"
             ]
  , examples (OrdinalData 2)
             [ "მეორე"
             , "მე-2"
             ]
  , examples (OrdinalData 3)
             [ "მესამე"
             , "მე-3"
             ]
  , examples (OrdinalData 4)
             [ "მეოთხე"
             , "მე-4"
             ]
  , examples (OrdinalData 8)
             [ "მერვე"
             , "მე-8"
             ]
  , examples (OrdinalData 25)
             [ "ოცდამეხუთე"
             , "25-ე"
             ]
  , examples (OrdinalData 31)
             [ "ოცდამეთერთმეტე"
             , "31-ე"
             ]
  , examples (OrdinalData 42)
             [ "ორმოცდამეორე"
             , "42-ე"
             ]
  , examples (OrdinalData 73)
            [ "სამოცდამეცამეტე"
            , "73-ე"
            ]
  , examples (OrdinalData 90)
            [ "ოთხმოცდამეათე"
            , "90-ე"
            ]
  ]
