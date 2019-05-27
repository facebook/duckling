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

defaultRules :: Some Dimension -> [Rule]
defaultRules dim@(This Time) = TimeUS.rulesBackwardCompatible ++ langRules dim
defaultRules dim             = langRules dim

localeRules :: Region -> Some Dimension -> [Rule]
localeRules AU (This AmountOfMoney) = AmountOfMoneyAU.rules
localeRules BZ (This AmountOfMoney) = AmountOfMoneyBZ.rules
localeRules CA (This AmountOfMoney) = AmountOfMoneyCA.rules
localeRules GB (This AmountOfMoney) = AmountOfMoneyGB.rules
localeRules IE (This AmountOfMoney) = AmountOfMoneyIE.rules
localeRules IN (This AmountOfMoney) = AmountOfMoneyIN.rules
localeRules JM (This AmountOfMoney) = AmountOfMoneyJM.rules
localeRules NZ (This AmountOfMoney) = AmountOfMoneyNZ.rules
localeRules PH (This AmountOfMoney) = AmountOfMoneyPH.rules
localeRules TT (This AmountOfMoney) = AmountOfMoneyTT.rules
localeRules US (This AmountOfMoney) = AmountOfMoneyUS.rules
localeRules ZA (This AmountOfMoney) = AmountOfMoneyZA.rules
localeRules AU (This Time) = TimeAU.rules
localeRules BZ (This Time) = TimeBZ.rules
localeRules CA (This Time) = TimeCA.rules
localeRules GB (This Time) = TimeGB.rules
localeRules IE (This Time) = TimeIE.rules
localeRules IN (This Time) = TimeIN.rules
localeRules JM (This Time) = TimeJM.rules
localeRules NZ (This Time) = TimeNZ.rules
localeRules PH (This Time) = TimePH.rules
localeRules TT (This Time) = TimeTT.rules
localeRules US (This Time) = TimeUS.rules
localeRules ZA (This Time) = TimeZA.rules
localeRules region (This (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = AmountOfMoney.rules
langRules (This CreditCardNumber) = []
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
langRules (This (CustomDimension dim)) = dimLangRules EN dim
