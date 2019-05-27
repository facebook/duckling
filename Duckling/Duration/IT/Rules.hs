-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.IT.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

ruleUneUnitofduration :: Rule
ruleUneUnitofduration = Rule
  { name = "une <unit-of-duration>"
  , pattern =
    [ regex "un[a']?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 1
      _ -> Nothing
  }

ruleUnQuartoDora :: Rule
ruleUnQuartoDora = Rule
  { name = "un quarto d'ora"
  , pattern =
    [ regex "un quarto d['i] ?ora"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleMezzOra :: Rule
ruleMezzOra = Rule
  { name = "mezz'ora"
  , pattern =
    [ regex "mezz[a'] ?ora"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleTreQuartiDora :: Rule
ruleTreQuartiDora = Rule
  { name = "tre quarti d'ora"
  , pattern =
    [ regex "(3|tre) quarti d['i] ?ora"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 45
  }

rules :: [Rule]
rules =
  [ ruleUneUnitofduration
  , ruleUnQuartoDora
  , ruleMezzOra
  , ruleTreQuartiDora
  ]
