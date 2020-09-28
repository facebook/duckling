-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
module Duckling.Rules.DE
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Distance.DE.Rules as Distance
import qualified Duckling.Duration.DE.Rules as Duration
import qualified Duckling.Email.DE.Rules as Email
import Duckling.Locale
import qualified Duckling.Numeral.DE.Rules as Numeral
import qualified Duckling.Ordinal.DE.Rules as Ordinal
import qualified Duckling.Time.DE.Rules as Time
import qualified Duckling.TimeGrain.DE.Rules as TimeGrain
import Duckling.Types
import qualified Duckling.Volume.DE.Rules as Volume

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = []
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = Distance.rules
langRules (Seal Duration) = Duration.rules
langRules (Seal Email) = Email.rules
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Ordinal) = Ordinal.rules
langRules (Seal PhoneNumber) = []
langRules (Seal Quantity) = []
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = []
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension dim)) = dimLangRules DE dim
