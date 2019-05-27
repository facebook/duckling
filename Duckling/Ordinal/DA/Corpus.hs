-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.DA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale DA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 4)
             [ "4."
             , "fjerde"
             , "Fjerde"
             ]
  , examples (OrdinalData 41)
             [ "enogfyrrende"
             ]
  , examples (OrdinalData 78)
             [ "otteoghalvfjerdsindstyvende"
             ]
  , examples (OrdinalData 263)
             [ "to hundrede og treogtresindstyvende"
             , "tohundrede og treogtresindstyvende"
             ]
  , examples (OrdinalData 70)
             [ "halvfjerdsende"
             , "halvfjerdsindstyvende"
             ]
  ]
