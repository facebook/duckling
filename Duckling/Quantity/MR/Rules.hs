-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Quantity.MR.Rules
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
  [ ("<quantity> grams", "(किलोग्राम|किलोग्रॅम|किग्रॅ|किग्रा|कीलो|किलो|kg|kgs|ग्रॅम|ग्राम|g|gm|मिलीग्राम|मिलीग्रॅम|मिग्रॅ|mg)", TQuantity.Gram)
  , ("<quantity> pounds", "(पाउंड|पाऊंड|lb|lbs)", TQuantity.Pound)
  , ("<quantity> cups", "(कप)", TQuantity.Cup)
  , ("<quantity> bowls", "(वाट्या|वाटी)", TQuantity.Bowl)
  , ("<quantity> tablespoons", "(चमचा|चमचे)", TQuantity.Tablespoon)
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "किलोग्राम" , (* 1000))
  , ( "किलोग्रॅम" , (* 1000))
  , ( "किग्रॅ"     , (* 1000))
  , ( "किग्रा"    , (* 1000))
  , ( "कीलो"      , (* 1000))
  , ( "किलो"      , (* 1000))
  , ( "kg"        , (* 1000))
  , ( "kgs"       , (* 1000))
  , ( "ग्रॅम"     , id)
  , ( "ग्राम"     , id)
  , ( "g"         , id)
  , ( "gm"        , id)
  , ( "मिलीग्राम" , (/ 1000))
  , ( "मिलीग्रॅम" , (/ 1000))
  , ( "मिग्रॅ"     , (/ 1000))
  , ( "mg"        , (/ 1000))
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

isWithoutProduct :: Predicate
isWithoutProduct (Token Quantity TQuantity.QuantityData{TQuantity.aproduct = Nothing}) = True
isWithoutProduct _ = False

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> <product>"
  , pattern =
    [ Predicate isWithoutProduct
    , regex "(?:च्या|चे|ची)?\\s*([a-zA-Z\\x{0900}-\\x{097F}]+)"
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
