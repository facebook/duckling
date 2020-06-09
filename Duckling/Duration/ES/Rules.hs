-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.ES.Rules
  ( rules
  ) where

import Duckling.Duration.Helpers
import Data.Maybe
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG
import Prelude

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

rules :: [Rule]
rules =
  [ ruleDurationHalfOfAnHour
  , ruleDurationQuarterOfAnHour
  , ruleDurationThreeQuartersOfAnHour
  ]
