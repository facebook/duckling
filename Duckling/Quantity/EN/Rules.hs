-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.EN.Rules
  ( rules ) where

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

quantities :: [(Text, String, TQuantity.Unit)]
quantities =
  [ ("<quantity> cups", "(cups?)", TQuantity.Cup)
  , ("<quantity> grams", "(((m(illi)?)|(k(ilo)?))?g(ram)?s?)", TQuantity.Gram)
  , ("<quantity> lb", "((lb|pound)s?)", TQuantity.Pound)
  , ("<quantity> oz", "((ounces?)|oz)", TQuantity.Ounce)
  ]

getValue :: Text -> Double -> Double
getValue match value = case Text.toLower match of
  "milligram" -> value / 1000
  "milligrams" -> value / 1000
  "mg" -> value / 1000
  "mgs" -> value / 1000
  "kilogram" -> value * 1000
  "kilograms" -> value * 1000
  "kg" -> value * 1000
  "kgs" -> value * 1000
  _ -> value

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ numberWith TNumeral.value (> 0), regex regexPattern ]
      , prod = \tokens -> case tokens of
        (Token Numeral nd:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Quantity $ quantity u value
          where value = getValue match $ TNumeral.value nd
        _ -> Nothing
      }

ruleAQuantity :: [Rule]
ruleAQuantity = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ regex ("an? " ++ regexPattern) ]
      , prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Quantity $ quantity u $ getValue match 1
        _ -> Nothing
      }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ dimension Quantity
    , regex "of (\\w+)"
    ]
  , prod = \tokens -> case tokens of
    (Token Quantity qd:Token RegexMatch (GroupMatch (product:_)):_) ->
      Just . Token Quantity $ withProduct product qd
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleQuantityOfProduct
  ]
  ++ ruleNumeralQuantities
  ++ ruleAQuantity
