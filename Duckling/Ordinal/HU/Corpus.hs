-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.HU.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale HU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "első"
             , "1."
             ]
  , examples (OrdinalData 2)
             [ "második"
             , "2."
             ]
  , examples (OrdinalData 3)
             [ "harmadik"
             , "3."
             ]
  , examples (OrdinalData 4)
             [ "negyedik"
             , "4."
             ]
  , examples (OrdinalData 8)
             [ "nyolcadik"
             , "8."
             ]
  , examples (OrdinalData 25)
             [ "huszonötödik"
             , "25."
             ]
  , examples (OrdinalData 31)
             [ "harmincegyedik"
             , "31."
             ]
  , examples (OrdinalData 42)
             [ "negyvenkettedik"
             , "42."
             ]
  , examples (OrdinalData 77)
            [ "hetvenhetedik"
            , "77."
            ]
  , examples (OrdinalData 90)
            [ "kilencvenedik"
            , "90."
            ]
  ]
