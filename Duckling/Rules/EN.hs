-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.EN
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.AmountOfMoney.EN.Rules as AmountOfMoney
import qualified Duckling.Distance.EN.Rules as Distance
import qualified Duckling.Duration.EN.Rules as Duration
import qualified Duckling.Email.EN.Rules as Email
import qualified Duckling.Numeral.EN.Rules as Numeral
import qualified Duckling.Ordinal.EN.Rules as Ordinal
import qualified Duckling.Quantity.EN.Rules as Quantity
import qualified Duckling.Temperature.EN.Rules as Temperature
import qualified Duckling.Time.EN.Rules as Time
import qualified Duckling.Time.EN.CA.Rules as TimeCA
import qualified Duckling.Time.EN.GB.Rules as TimeGB
import qualified Duckling.Time.EN.US.Rules as TimeUS
import qualified Duckling.TimeGrain.EN.Rules as TimeGrain
import qualified Duckling.Volume.EN.Rules as Volume

defaultRules :: Some Dimension -> [Rule]
defaultRules dim@(This Time) =
  [ TimeUS.ruleMMDD
  , TimeUS.ruleMMDDYYYY
  , TimeUS.ruleThanksgiving
  ] ++ langRules dim
defaultRules dim = langRules dim

localeRules :: Region -> Some Dimension -> [Rule]
localeRules CA (This Time) = TimeCA.rules
localeRules GB (This Time) = TimeGB.rules
localeRules US (This Time) = TimeUS.rules
localeRules _ _            = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = AmountOfMoney.rules
langRules (This Distance) = Distance.rules
langRules (This Duration) = Duration.rules
langRules (This Email) = Email.rules
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = Quantity.rules
langRules (This RegexMatch) = []
langRules (This Temperature) = Temperature.rules
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = Volume.rules
