-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.BG
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Numeral.BG.Rules as Numeral
import qualified Duckling.AmountOfMoney.BG.Rules as AmountOfMoney
import qualified Duckling.Distance.BG.Rules as Distance
import qualified Duckling.Duration.BG.Rules as Duration
import qualified Duckling.TimeGrain.BG.Rules as TimeGrain
import qualified Duckling.Ordinal.BG.Rules as Ordinal

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Some Dimension -> [Rule]
localeRules region (This (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = AmountOfMoney.rules
langRules (This CreditCardNumber) = []
langRules (This Distance) = Distance.rules
langRules (This Duration) = Duration.rules
langRules (This Email) = []
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = []
langRules (This RegexMatch) = []
langRules (This Temperature) = []
langRules (This Time) = []
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = []
langRules (This (CustomDimension dim)) = dimLangRules BG dim
