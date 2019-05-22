-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.KM.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Helpers
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "ទី"
    , dimension Numeral
    ]
  , prod = \case
      (_:Token Numeral NumeralData{TNumeral.value = x}:_) ->
        Just . ordinal $ floor x
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  ]
