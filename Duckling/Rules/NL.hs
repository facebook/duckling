-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.NL
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Distance.NL.Rules as Distance
import qualified Duckling.Number.NL.Rules as Number
import qualified Duckling.Ordinal.NL.Rules as Ordinal
import qualified Duckling.Volume.NL.Rules as Volume
import Duckling.Types

rules :: Some Dimension -> [Rule]
rules (Some Distance) = Distance.rules
rules (Some Duration) = []
rules (Some DNumber) = Number.rules
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
rules (Some Volume) = Volume.rules
