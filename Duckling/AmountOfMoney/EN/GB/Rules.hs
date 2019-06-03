-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.EN.GB.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..))
import Duckling.Numeral.Helpers (isPositive)
import Duckling.Regex.Types
import Duckling.Types

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Duckling.AmountOfMoney.Helpers as Helpers
import qualified Duckling.Numeral.Types as TNumeral

ruleAGrand :: Rule
ruleAGrand = Rule
  { name = "a grand"
  , pattern =
    [ regex "a grand"
    ]
  , prod = \_ -> Just . Token AmountOfMoney . withValue 1000 $ currencyOnly GBP
  }

ruleGrand :: Rule
ruleGrand = Rule
  { name = "<amount> grand"
  , pattern =
    [ Predicate isPositive
    , regex "grand"
    ]
  , prod = \case
      (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:_)
        -> Just . Token AmountOfMoney . withValue (1000 * v) $ currencyOnly GBP
      _ -> Nothing
  }

ruleDollarCoin :: Rule
ruleDollarCoin = Rule
  { name = "dollar coin"
  , pattern =
    [ regex "(nickel|dime|quarter)s?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        c <- HashMap.lookup (Text.toLower match) Helpers.dollarCoins
        Just . Token AmountOfMoney . withValue c $ currencyOnly Dollar
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAGrand
  , ruleGrand
  , ruleDollarCoin
  ]
