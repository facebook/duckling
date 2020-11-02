-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.Common
  ( rules
  ) where

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.AmountOfMoney.Rules as AmountOfMoney
import qualified Duckling.CreditCardNumber.Rules as CreditCardNumber
import qualified Duckling.Distance.Rules as Distance
import qualified Duckling.Duration.Rules as Duration
import qualified Duckling.Email.Rules as Email
import qualified Duckling.Numeral.Rules as Numeral
import qualified Duckling.PhoneNumber.Rules as PhoneNumber
import qualified Duckling.Temperature.Rules as Temperature
import qualified Duckling.Url.Rules as Url
import qualified Duckling.Volume.Rules as Volume

rules :: Seal Dimension -> [Rule]
rules (Seal AmountOfMoney) = AmountOfMoney.rules
rules (Seal CreditCardNumber) = CreditCardNumber.rules
rules (Seal Distance) = Distance.rules
rules (Seal Duration) = Duration.rules
rules (Seal Email) = Email.rules
rules (Seal Numeral) = Numeral.rules
rules (Seal Ordinal) = []
rules (Seal PhoneNumber) = PhoneNumber.rules
rules (Seal Quantity) = []
rules (Seal RegexMatch) = []
rules (Seal Temperature) = Temperature.rules
rules (Seal Time) = []
rules (Seal TimeGrain) = []
rules (Seal Url) = Url.rules
rules (Seal Volume) = Volume.rules
rules (Seal (CustomDimension dim)) = dimRules dim
