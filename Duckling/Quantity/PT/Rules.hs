-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Quantity.PT.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Prelude
import Data.String
import Data.Text (Text)

import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Numeral.Helpers
import Duckling.Quantity.Helpers
import qualified Duckling.Quantity.Types as TQuantity
import Duckling.Regex.Types
import Duckling.Types

quantities :: [(Text, String, TQuantity.Unit)]
quantities =
  [ ("<quantity> copos", "(copos?)", TQuantity.Cup)
  , ("<quantity> gramas", "((((mili)|(quilo))?(grama)s?)|(quilos?)|((m|k)?g))", TQuantity.Gram)
  , ("<quantity> libras", "((lb|libra)s?)", TQuantity.Pound)
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "miligrama"  , (/ 1000))
  , ( "miligramas" , (/ 1000))
  , ( "mg"         , (/ 1000))
  , ( "mgs"        , (/ 1000))
  , ( "quilograma" , (* 1000))
  , ( "quilogramas", (* 1000))
  , ( "quilo"      , (* 1000))
  , ( "quilos"     , (* 1000))
  , ( "kg"         , (* 1000))
  , ( "kgs"        , (* 1000))
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [Predicate isPositive, regex regexPattern]
      , prod = \case
          (Token Numeral nd:
           Token RegexMatch (GroupMatch (match:_)):
           _) -> do
            let value = getValue opsMap match $ TNumeral.value nd
            Just $ Token Quantity $ quantity u value
          _ -> Nothing
      }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ dimension Quantity
    , regex "de (\\w+)"
    ]
  , prod = \case
      (Token Quantity qd:Token RegexMatch (GroupMatch (product:_)):_) ->
        Just $ Token Quantity $ withProduct (Text.toLower product) qd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleQuantityOfProduct ]
  ++ ruleNumeralQuantities
