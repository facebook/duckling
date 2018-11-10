-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.MN.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Quantity.Helpers
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

quantities :: [(Text, String, TQuantity.Unit, Double -> Double)]
quantities =
  [ ("<quantity> milligrams", "(мг|миллиграмм)", TQuantity.Gram, (/ 1000))
  , ("<quantity> grams", "г(рамм)", TQuantity.Gram, id)
  , ("<quantity> kilograms", "(кг|килограмм)", TQuantity.Gram, (* 1000))
  , ("<quantity> lb", "фунт", TQuantity.Pound, id)
  , ("<quantity> oz", "унц", TQuantity.Ounce, id)
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go quantities
  where
    go :: (Text, String, TQuantity.Unit, Double -> Double) -> Rule
    go (name, regexPattern, u, convert) = Rule
      { name = name
      , pattern =
        [ numberWith TNumeral.value (> 0)
        , regex regexPattern
        ]
      , prod = \tokens -> case tokens of
        (Token Numeral nd:_) ->
          Just . Token Quantity . quantity u . convert $ TNumeral.value nd
        _ -> Nothing
      }

ruleAQuantity :: [Rule]
ruleAQuantity = map go quantities
  where
    go :: (Text, String, TQuantity.Unit, Double -> Double) -> Rule
    go (name, regexPattern, u, convert) = Rule
      { name = name
      , pattern = [ regex regexPattern ]
      , prod = \_ -> Just . Token Quantity . quantity u $ convert 1
      }

rules :: [Rule]
rules = ruleNumeralQuantities ++ ruleAQuantity
