-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
module Duckling.Numeral.ES.MX.Corpus (allExamples) where

import Data.String
import Prelude

import Duckling.Numeral.Types
import Duckling.Testing.Types

allExamples :: [Example]
allExamples =
  concat
    [ examples (simple 1) ["1"]
    , examples (simple 33) ["33"]
    , examples (simple 1.1) ["1.1", "1.10", "01.10"]
    , examples (simple 0.77) ["0.77", ".77"]
    , examples (simple 100000) ["100,000", "100000"]
    , examples (simple 243) ["243"]
    , examples (simple 3000000) ["3000000", "3,000,000"]
    , examples (simple 1200000) ["1,200,000", "1200000"]
    , examples
        (simple (-1200000))
        ["- 1,200,000", "menos 1,200,000", "-1.2M", "-.0012G"]
    , examples (simple 1.5) ["1.5"]
    ]
