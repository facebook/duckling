
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

import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Duration.Helpers

ruleDurationHalfOfAnHour :: Rule
ruleDurationHalfOfAnHour = Rule
  { name = "half of an hour"
  , pattern =
    [ regex "media hora"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

rules :: [Rule]
rules =
  [ ruleDurationHalfOfAnHour
  ]