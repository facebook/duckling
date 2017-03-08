-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.SV.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = SV}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "förste"
             , "första"
             , "1:a"
             , "1:e"
             ]
  , examples (OrdinalData 10)
             [ "tionde"
             , "10."
             , "10"
             ]
  , examples (OrdinalData 26)
             [ "seksogtjuende"
             , "Seksogtyvende"
             , "26e"
             ]
  ]
