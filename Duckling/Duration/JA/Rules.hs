-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.JA.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (numberWith)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleDurationMonthWithCounter :: Rule
ruleDurationMonthWithCounter = Rule
  { name = "<integer> counter months"
  , pattern =
    [ numberWith TNumeral.value (>= 1)
    , regex "(ケ|ヶ|カ|ヵ|か|箇)(月|げつ|つき)"
    ]
  , prod = \case
      (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:
       _) -> Just $ Token Duration $ duration TG.Month $ floor v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDurationMonthWithCounter
  ]
