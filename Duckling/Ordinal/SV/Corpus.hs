-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.SV.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale SV Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "första"
             , "förste"
             , "1a"
             , "1:a"
             ]
  , examples (OrdinalData 2)
             [ "andra"
             , "andre"
             , "2a"
             , "2:a"
             ]
  , examples (OrdinalData 3)
             [ "tredje"
             , "3e"
             , "3:e"
             ]
  , examples (OrdinalData 4)
             [ "fjärde"
             , "4e"
             , "4:e"
             ]
  , examples (OrdinalData 8)
             [ "åttonde"
             , "8e"
             , "8:e"
             ]
  , examples (OrdinalData 25)
             [ "tjugofemte"
             , "25e"
             , "25:e"
             ]
  , examples (OrdinalData 31)
             [ "trettioförsta"
             , "trettioförste"
             , "31a"
             , "31:a"
             ]
  , examples (OrdinalData 42)
             [ "fyrtioandra"
             , "fyrtioandre"
             , "42a"
             , "42:a"
             ]
  , examples (OrdinalData 77)
            [ "sjuttiosjunde"
            , "77e"
            , "77:e"
            ]
  , examples (OrdinalData 90)
            [ "nittionde"
            , "90e"
            , "90:e"
            ]
  ]
