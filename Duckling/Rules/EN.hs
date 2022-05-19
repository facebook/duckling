-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


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
import qualified Duckling.AmountOfMoney.EN.AU.Rules as AmountOfMoneyAU
import qualified Duckling.AmountOfMoney.EN.BZ.Rules as AmountOfMoneyBZ
import qualified Duckling.AmountOfMoney.EN.CA.Rules as AmountOfMoneyCA
import qualified Duckling.AmountOfMoney.EN.GB.Rules as AmountOfMoneyGB
import qualified Duckling.AmountOfMoney.EN.IE.Rules as AmountOfMoneyIE
import qualified Duckling.AmountOfMoney.EN.IN.Rules as AmountOfMoneyIN
import qualified Duckling.AmountOfMoney.EN.JM.Rules as AmountOfMoneyJM
import qualified Duckling.AmountOfMoney.EN.NZ.Rules as AmountOfMoneyNZ
import qualified Duckling.AmountOfMoney.EN.PH.Rules as AmountOfMoneyPH
import qualified Duckling.AmountOfMoney.EN.TT.Rules as AmountOfMoneyTT
import qualified Duckling.AmountOfMoney.EN.US.Rules as AmountOfMoneyUS
import qualified Duckling.AmountOfMoney.EN.ZA.Rules as AmountOfMoneyZA
import qualified Duckling.Distance.EN.Rules as Distance
import qualified Duckling.Duration.EN.Rules as Duration
import qualified Duckling.Email.EN.Rules as Email
import qualified Duckling.Numeral.EN.Rules as Numeral
import qualified Duckling.Ordinal.EN.Rules as Ordinal
import qualified Duckling.Quantity.EN.Rules as Quantity
import qualified Duckling.Region as R
import qualified Duckling.Temperature.EN.Rules as Temperature
import qualified Duckling.Time.EN.Rules as Time
import qualified Duckling.Time.EN.AU.Rules as TimeAU
import qualified Duckling.Time.EN.BZ.Rules as TimeBZ
import qualified Duckling.Time.EN.CA.Rules as TimeCA
import qualified Duckling.Time.EN.GB.Rules as TimeGB
import qualified Duckling.Time.EN.IE.Rules as TimeIE
import qualified Duckling.Time.EN.IN.Rules as TimeIN
import qualified Duckling.Time.EN.JM.Rules as TimeJM
import qualified Duckling.Time.EN.NZ.Rules as TimeNZ
import qualified Duckling.Time.EN.PH.Rules as TimePH
import qualified Duckling.Time.EN.TT.Rules as TimeTT
import qualified Duckling.Time.EN.US.Rules as TimeUS
import qualified Duckling.Time.EN.ZA.Rules as TimeZA
import qualified Duckling.TimeGrain.EN.Rules as TimeGrain
import qualified Duckling.Volume.EN.Rules as Volume

defaultRules :: Seal Dimension -> [Rule]
defaultRules dim@(Seal Time) = TimeUS.rulesBackwardCompatible ++ langRules dim
defaultRules dim             = langRules dim

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules AU (Seal AmountOfMoney) = AmountOfMoneyAU.rules
localeRules BZ (Seal AmountOfMoney) = AmountOfMoneyBZ.rules
localeRules R.CA (Seal AmountOfMoney) = AmountOfMoneyCA.rules
localeRules GB (Seal AmountOfMoney) = AmountOfMoneyGB.rules
localeRules IE (Seal AmountOfMoney) = AmountOfMoneyIE.rules
localeRules IN (Seal AmountOfMoney) = AmountOfMoneyIN.rules
localeRules JM (Seal AmountOfMoney) = AmountOfMoneyJM.rules
localeRules NZ (Seal AmountOfMoney) = AmountOfMoneyNZ.rules
localeRules PH (Seal AmountOfMoney) = AmountOfMoneyPH.rules
localeRules TT (Seal AmountOfMoney) = AmountOfMoneyTT.rules
localeRules US (Seal AmountOfMoney) = AmountOfMoneyUS.rules
localeRules ZA (Seal AmountOfMoney) = AmountOfMoneyZA.rules
localeRules AU (Seal Time) = TimeAU.rules
localeRules BZ (Seal Time) = TimeBZ.rules
localeRules R.CA (Seal Time) = TimeCA.rules
localeRules GB (Seal Time) = TimeGB.rules
localeRules IE (Seal Time) = TimeIE.rules
localeRules IN (Seal Time) = TimeIN.rules
localeRules JM (Seal Time) = TimeJM.rules
localeRules NZ (Seal Time) = TimeNZ.rules
localeRules PH (Seal Time) = TimePH.rules
localeRules TT (Seal Time) = TimeTT.rules
localeRules US (Seal Time) = TimeUS.rules
localeRules ZA (Seal Time) = TimeZA.rules
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
langRules (Seal Quantity) = Quantity.rules
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = Temperature.rules
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension dim)) = dimLangRules EN dim
