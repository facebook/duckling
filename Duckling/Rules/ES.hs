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

defaultRules :: Some Dimension -> [Rule]
defaultRules dim@(This Numeral) =
  NumeralES.rulesBackwardCompatible ++ langRules dim
defaultRules dim = langRules dim

localeRules :: Region -> Some Dimension -> [Rule]
localeRules R.AR (This Numeral) = NumeralAR.rules
localeRules CL (This Numeral) = NumeralCL.rules
localeRules CO (This Numeral) = NumeralCO.rules
localeRules R.ES (This Numeral) = NumeralES.rules
localeRules MX (This Numeral) = NumeralMX.rules
localeRules PE (This Numeral) = NumeralPE.rules
localeRules VE (This Numeral) = NumeralVE.rules
localeRules region (This (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = AmountOfMoney.rules
langRules (This CreditCardNumber) = []
langRules (This Distance) = Distance.rules
langRules (This Duration) = []
langRules (This Email) = []
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = []
langRules (This RegexMatch) = []
langRules (This Temperature) = Temperature.rules
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = Volume.rules
langRules (This (CustomDimension dim)) = dimLangRules ES dim
