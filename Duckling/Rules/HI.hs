--Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

-------------------------WORK IN PROGRESS------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.HI
  ( defaultRules,
    langRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.Numeral.HI.Rules as Numeral

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

langRules :: Some Dimension -> [Rule]
langRules (This Distance) = []
langRules (This Duration) = []
langRules (This Numeral) = Numeral.rules
langRules (This Email) = []
langRules (This AmountOfMoney) = []
langRules (This Ordinal) = []
langRules (This PhoneNumber) = []
langRules (This Quantity) = []
langRules (This RegexMatch) = []
langRules (This Temperature) = []
langRules (This Time) = []
langRules (This TimeGrain) = []
langRules (This Url) = []
langRules (This Volume) = []

-----------------------------------------------------------------