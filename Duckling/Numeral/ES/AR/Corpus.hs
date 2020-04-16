-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
module Duckling.Numeral.ES.AR.Corpus (allExamples) where

import Data.String
import Prelude

import Duckling.Numeral.Types
import Duckling.Testing.Types

allExamples :: [Example]
allExamples =
  concat
    [ examples (NumeralValue 1) ["1"]
    , examples (NumeralValue 33) ["33"]
    , examples (NumeralValue 1.1) ["1,1", "1,10", "01,10"]
    , examples (NumeralValue 0.77) ["0,77", ",77"]
    , examples (NumeralValue 100000) ["100.000", "100000"]
    , examples (NumeralValue 243) ["243"]
    , examples (NumeralValue 3000000) ["3000000", "3.000.000"]
    , examples (NumeralValue 1200000) ["1.200.000", "1200000"]
    , examples
        (NumeralValue (-1200000))
        ["- 1.200.000", "menos 1.200.000", "-1,2M", "-,0012G"]
    , examples (NumeralValue 1.5) ["1,5"]
    ]
