-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.AR.Rules
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
  [ ("<quantity> cups", "(كوب(ان|ين)?|[أا]كواب)", TQuantity.Cup)
  , ("<quantity> grams", "(((كيلو|مي?لي?) ?)?(غرام(ات|ين|ان)?)|كغم?|ملغ|غم)", TQuantity.Gram)
  , ("<quantity> lb", "(باوند(ان|ين)?)", TQuantity.Pound)
  , ("<quantity> oz", "([أا]ونص([ةه]|تان|تين|ات))", TQuantity.Ounce)
  ]

getValue :: Text -> Double -> Double
getValue match value = case Text.toLower match of
  "ميلي غرام" -> value / 1000
  "ميليغرام" -> value / 1000
  "ميلغرام" -> value / 1000
  "ميلي غرامان" -> value / 500
  "ميليغرامان" -> value / 500
  "ميلغرامان" -> value / 500
  "ميلي غرامين" -> value / 500
  "ميليغرامين" -> value / 500
  "ميلغرامين" -> value / 500
  "ميلي غرامات" -> value / 1000
  "ميليغرامات" -> value / 1000
  "ميلغرامات" -> value / 1000
  "ملغ" -> value / 1000
  "غرامان" -> value * 2
  "غرامين" -> value * 2
  "كوبان" -> value * 2
  "كوبين" -> value * 2
  "باوندان" -> value * 2
  "باوندين" -> value * 2
  "اونصتان" -> value * 2
  "اونصتين" -> value * 2
  "أونصتان" -> value * 2
  "أونصتين" -> value * 2
  "كيلوغرام" -> value * 1000
  "كيلو غرام" -> value * 1000
  "كيلوغرامان" -> value * 2000
  "كيلوغرامين" -> value * 2000
  "كيلو غرامان" -> value * 2000
  "كيلو غرامين" -> value * 2000
  "كيلوغرامات" -> value * 1000
  "كيلو غرامات" -> value * 1000
  "كغ" -> value * 1000
  "كغم" -> value * 1000
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
      , pattern = [ regex regexPattern ]
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
    , regex "من ([ء-ي]+)"
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
