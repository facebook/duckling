-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}

module Duckling.Rules.IS
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Email.IS.Rules as Email
import qualified Duckling.Numeral.IS.Rules as Numeral

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = []
langRules (Seal Duration) = []
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Email) = Email.rules
langRules (Seal AmountOfMoney) = []
langRules (Seal Ordinal) = []
langRules (Seal PhoneNumber) = []
langRules (Seal Quantity) = []
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = []
langRules (Seal Time) = []
langRules (Seal TimeGrain) = []
langRules (Seal Url) = []
langRules (Seal Volume) = []
langRules (Seal (CustomDimension dim)) = dimLangRules IS dim
