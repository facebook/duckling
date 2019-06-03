-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.RO.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (numberWith)
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleQuarterOfAnHour :: Rule
ruleQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "(1/4\\s?(h|or(a|ă))|sfert de or(a|ă))"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleJumatateDeOra :: Rule
ruleJumatateDeOra = Rule
  { name = "jumatate de ora"
  , pattern =
    [ regex "(1/2\\s?(h|or(a|ă))|jum(a|ă)tate (de )?or(a|ă))"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleTreiSferturiDeOra :: Rule
ruleTreiSferturiDeOra = Rule
  { name = "trei sferturi de ora"
  , pattern =
    [ regex "(3/4\\s?(h|or(a|ă))|trei sferturi de or(a|ă))"
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
  , prod = \case
      (_:Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 1
      _ -> Nothing
  }

ruleExactInJurDeDuration :: Rule
ruleExactInJurDeDuration = Rule
  { name = "exact|in jur de <duration>"
  , pattern =
    [ regex "(exact|aproximativ|(i|î)n jur de)"
    , dimension Duration
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntegerDeUnitofduration :: Rule
ruleIntegerDeUnitofduration = Rule
  { name = "<integer> <unit-of-duration>"
  , pattern =
    [ numberWith TNumeral.value (>= 20)
    , regex "de"
    , dimension TimeGrain
    ]
  , prod = \case
      (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:
       _:
       Token TimeGrain grain:
       _) -> Just . Token Duration . duration grain $ floor v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleExactInJurDeDuration
  , ruleJumatateDeOra
  , ruleOUnitofduration
  , ruleQuarterOfAnHour
  , ruleTreiSferturiDeOra
  , ruleIntegerDeUnitofduration
  ]
