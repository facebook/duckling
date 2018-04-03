-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.Rules
  ( rules
  ) where

import Data.Semigroup ((<>))
import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral

ruleIntegerUnitofduration :: Rule
ruleIntegerUnitofduration = Rule
  { name = "<integer> <unit-of-duration>"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain grain:
       _) -> Just . Token Duration . duration grain $ floor v
      _ -> Nothing
  }

ruleCompositeDuration :: Rule
ruleCompositeDuration = Rule
  { name = "composite <duration>"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex ",|and"
    , dimension Duration
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}: Token TimeGrain g: _:
       Token Duration dur: _)
        | g > TDuration.grain dur ->
            let dur1 = duration g $ floor v
            in Just . Token Duration $ dur1 <> dur
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCompositeDuration
  , ruleIntegerUnitofduration
  ]
