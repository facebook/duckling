-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.IT
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.AmountOfMoney.IT.Rules as AmountOfMoney
import qualified Duckling.Distance.IT.Rules as Distance
import qualified Duckling.Duration.IT.Rules as Duration
import qualified Duckling.Email.IT.Rules as Email
import qualified Duckling.Numeral.IT.Rules as Numeral
import qualified Duckling.Ordinal.IT.Rules as Ordinal
import qualified Duckling.Temperature.IT.Rules as Temperature
import qualified Duckling.Time.IT.Rules as Time
import qualified Duckling.TimeGrain.IT.Rules as TimeGrain
import qualified Duckling.Volume.IT.Rules as Volume

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = AmountOfMoney.rules
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = Distance.rules
langRules (Seal Duration) = Duration.rules
langRules (Seal Email) = Email.rules
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Ordinal) = Ordinal.rules
langRules (Seal PhoneNumber) = []
langRules (Seal Quantity) = []
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = Temperature.rules
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension dim)) = dimLangRules IT dim
