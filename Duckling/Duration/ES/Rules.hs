-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.ES.Rules
  ( rules
  ) where

import Prelude
import Data.Semigroup ((<>))
import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Duration.Types (DurationData(..))
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleDurationQuarterOfAnHour :: Rule
ruleDurationQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "cuartos? de hora"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 15
  }

ruleDurationHalfOfAnHour :: Rule
ruleDurationHalfOfAnHour = Rule
  { name = "half of an hour"
  , pattern =
    [ regex "media horas?"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 30
  }

ruleDurationThreeQuartersOfAnHour :: Rule
ruleDurationThreeQuartersOfAnHour = Rule
  { name = "three-quarters of an hour"
  , pattern =
    [ regex "tres cuartos? de horas?"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 45
  }

ruleCompositeDurationCommasAnd :: Rule
ruleCompositeDurationCommasAnd = Rule
  { name = "composite <duration> (with and)"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex "y"
    , dimension Duration
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain g:
       _:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just $ Token Duration $ duration g (floor v) <> dd
      _ -> Nothing
  }

ruleCompositeDuration :: Rule
ruleCompositeDuration = Rule
  {
    name = "composite <duration>"
  , pattern =
    [
      Predicate isNatural
    , dimension TimeGrain
    , dimension Duration
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain g:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just $ Token Duration $ duration g (floor v) <> dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDurationHalfOfAnHour
  , ruleDurationQuarterOfAnHour
  , ruleDurationThreeQuartersOfAnHour
  , ruleCompositeDuration
  , ruleCompositeDurationCommasAnd
  ]
