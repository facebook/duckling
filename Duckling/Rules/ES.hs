-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
module Duckling.Rules.ES
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Prelude

import qualified Duckling.AmountOfMoney.ES.Rules as AmountOfMoney
import Duckling.Dimensions.Types
import qualified Duckling.Distance.ES.Rules as Distance
import Duckling.Locale
import qualified Duckling.Numeral.ES.AR.Rules as NumeralAR
import qualified Duckling.Numeral.ES.CL.Rules as NumeralCL
import qualified Duckling.Numeral.ES.CO.Rules as NumeralCO
import qualified Duckling.Numeral.ES.ES.Rules as NumeralES
import qualified Duckling.Numeral.ES.MX.Rules as NumeralMX
import qualified Duckling.Numeral.ES.PE.Rules as NumeralPE
import qualified Duckling.Numeral.ES.Rules as Numeral
import qualified Duckling.Numeral.ES.VE.Rules as NumeralVE
import qualified Duckling.Ordinal.ES.Rules as Ordinal
import qualified Duckling.Region as R
  ( Region
      ( AR
      , ES
      )
  )
import qualified Duckling.Temperature.ES.Rules as Temperature
import qualified Duckling.Time.ES.Rules as Time
import qualified Duckling.TimeGrain.ES.Rules as TimeGrain
import Duckling.Types
import qualified Duckling.Volume.ES.Rules as Volume
import qualified Duckling.Duration.ES.Rules as Duration
import qualified Duckling.Quantity.ES.Rules as Quantity

defaultRules :: Seal Dimension -> [Rule]
defaultRules dim@(Seal Numeral) =
  NumeralES.rulesBackwardCompatible ++ langRules dim
defaultRules dim = langRules dim

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules R.AR (Seal Numeral) = NumeralAR.rules
localeRules CL (Seal Numeral) = NumeralCL.rules
localeRules CO (Seal Numeral) = NumeralCO.rules
localeRules R.ES (Seal Numeral) = NumeralES.rules
localeRules MX (Seal Numeral) = NumeralMX.rules
localeRules PE (Seal Numeral) = NumeralPE.rules
localeRules VE (Seal Numeral) = NumeralVE.rules
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = AmountOfMoney.rules
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = Distance.rules
langRules (Seal Duration) = Duration.rules
langRules (Seal Email) = []
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Ordinal) = Ordinal.rules
langRules (Seal PhoneNumber) = []
langRules (Seal Quantity) = Quantity.rules
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = Temperature.rules
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension dim)) = dimLangRules ES dim
