-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.AR
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Number.AR.Rules as Number
import qualified Duckling.Ordinal.AR.Rules as Ordinal
import Duckling.Types

rules :: Some Dimension -> [Rule]
rules (Some Distance) = []
rules (Some Duration) = []
rules (Some Numeral) = Number.rules
rules (Some Email) = []
rules (Some Finance) = []
rules (Some Ordinal) = Ordinal.rules
rules (Some PhoneNumber) = []
rules (Some Quantity) = []
rules (Some RegexMatch) = []
rules (Some Temperature) = []
rules (Some Time) = []
rules (Some TimeGrain) = []
rules (Some Url) = []
rules (Some Volume) = []
