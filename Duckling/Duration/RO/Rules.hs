-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.RO.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

ruleQuarterOfAnHour :: Rule
ruleQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "(1/4\\s?(h|or(a|\x0103))|sfert de or(a|\x0103))"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleJumatateDeOra :: Rule
ruleJumatateDeOra = Rule
  { name = "jumatate de ora"
  , pattern =
    [ regex "(1/2\\s?(h|or(a|\x0103))|jum(a|\x0103)tate (de )?or(a|\x0103))"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleTreiSferturiDeOra :: Rule
ruleTreiSferturiDeOra = Rule
  { name = "trei sferturi de ora"
  , pattern =
    [ regex "(3/4\\s?(h|or(a|\x0103))|trei sferturi de or(a|\x0103))"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 45
  }

ruleOUnitofduration :: Rule
ruleOUnitofduration = Rule
  { name = "o <unit-of-duration>"
  , pattern =
    [ regex "o|un"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 1
      _ -> Nothing
  }

ruleExactInJurDeDuration :: Rule
ruleExactInJurDeDuration = Rule
  { name = "exact|in jur de <duration>"
  , pattern =
    [ regex "(exact|aproximativ|(i|\x00ee)n jur de)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleExactInJurDeDuration
  , ruleJumatateDeOra
  , ruleOUnitofduration
  , ruleQuarterOfAnHour
  , ruleTreiSferturiDeOra
  ]
