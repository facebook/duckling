-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.IT.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale IT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "primo"
             , "prima"
             , "1°"
             , "1ª"
             ]
  , examples (OrdinalData 2)
             [ "secondo"
             , "seconda"
             , "2°"
             , "2ª"
             ]
  , examples (OrdinalData 3)
             [ "terzo"
             , "Terza"
             , "3°"
             , "3ª"
             ]
  , examples (OrdinalData 4)
             [ "quarto"
             , "quarta"
             , "4°"
             , "4ª"
             ]
  , examples (OrdinalData 5)
             [ "quinto"
             , "quinta"
             , "5°"
             , "5ª"
             ]
  , examples (OrdinalData 6)
             [ "sesto"
             , "sesta"
             , "6°"
             , "6ª"
             ]
  , examples (OrdinalData 7)
             [ "settimo"
             , "settima"
             , "7°"
             , "7ª"
             ]
  , examples (OrdinalData 8)
             [ "ottavo"
             , "ottava"
             , "8°"
             , "8ª"
             ]
  , examples (OrdinalData 9)
             [ "nono"
             , "nona"
             , "9°"
             , "9ª"
             ]
  , examples (OrdinalData 10)
             [ "decimo"
             , "decima"
             , "10°"
             , "10ª"
             ]
  ]
