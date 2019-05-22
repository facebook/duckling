-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.ZH.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
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
  [ ("<quantity> grams", "(千克|公斤|斤|两|兩|克|毫克|kg|g|mg)", TQuantity.Gram)
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "千克" , (* 1000))
  , ( "公斤",  (* 1000))
  , ( "kg",   (* 1000))
  , ( "斤",    (* 500))
  , ( "两",    (* 50))
  , ( "兩",    (* 50))
  , ( "毫克",  (/ 1000))
  , ( "mg",   (/ 1000))
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go quantities
  where
    getValue :: Text -> Double -> Double
    getValue match = HashMap.lookupDefault id (Text.toLower match) opsMap

    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern =
        [ Predicate isPositive
        , regex regexPattern
        ]
      , prod = \case
        (Token Numeral nd:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Quantity $ quantity u value
          where value = getValue match $ TNumeral.value nd
        _ -> Nothing
      }

ruleCattyTael :: Rule
ruleCattyTael = Rule
  { name = "<quantity> catty <quantity> tael"
  , pattern =
    [ Predicate isPositive
    , regex "斤"
    , numberBetween 1 10
    , regex "两|兩"
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData{TNumeral.value = x}:
     Token RegexMatch (GroupMatch _):
     Token Numeral TNumeral.NumeralData{TNumeral.value = y}:
     Token RegexMatch (GroupMatch _):
     _) -> Just . Token Quantity $ quantity TQuantity.Gram (x * 500 + y * 50)
    _ -> Nothing
  }

ruleCattyHalf :: Rule
ruleCattyHalf = Rule
  { name = "<quantity> catty half"
  , pattern =
    [ Predicate isPositive
    , regex "斤半"
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData{TNumeral.value = x}:
     Token RegexMatch (GroupMatch _):
     _) -> Just . Token Quantity $ quantity TQuantity.Gram (x * 500 + 250)
    _ -> Nothing
  }

ruleTaelHalf :: Rule
ruleTaelHalf = Rule
  { name = "<quantity> tael half"
  , pattern =
    [ Predicate isPositive
    , regex "(两|兩)半"
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData{TNumeral.value = x}:
     Token RegexMatch (GroupMatch _):
     _) -> Just . Token Quantity $ quantity TQuantity.Gram (x * 50 + 25)
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCattyTael
  , ruleCattyHalf
  , ruleTaelHalf
  ]
  ++ ruleNumeralQuantities
