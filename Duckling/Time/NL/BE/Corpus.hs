-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.NL.BE.Corpus
  ( allExamples
  ) where

import Data.String
import Prelude

import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types

allExamples :: [Example]
allExamples = concat
  [ examples (datetimeHoliday (2013, 12, 6, 0, 0, 0) Day "Sinterklaas")
             [ "Sinterklaas"
             ]
  ]
