-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.CA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale CA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 kilòmetres"
             , "3 kilometres"
             , "3 kilómetres"
             , "3 quilòmetres"
             , "3 km"
             , "3km"
             ]
  , examples (simple Kilometre 3.0)
             [ "3 km"
             ]
  , examples (simple Mile 8)
             [ "8 milles"
             ]
  , examples (simple Metre 9)
             [ "9m"
             , "9 metres"
             , "9 m"
             ]
  , examples (simple Metre 1)
             [ "1m"
             , "1 metre"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 centímetres"
             , "2 cm"
             ]
  ]
