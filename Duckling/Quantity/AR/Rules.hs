-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.AR.Rules
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
  [ ("<quantity> cups", "(كوب(ان|ين)?|[أا]كواب)", TQuantity.Cup)
  , ("<quantity> grams", "(((كيلو|مي?لي?) ?)?((غ|ج)رام(ات|ين|ان)?)|ك(غ|ج)م?|مل(غ|ج)|(غ|ج)م)", TQuantity.Gram)
  , ("<quantity> lb", "(باوند(ان|ين)?)", TQuantity.Pound)
  , ("<quantity> oz", "([أا]ونص([ةه]|تان|تين|ات))", TQuantity.Ounce)
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "غرامان", (* 2))
  , ( "غرامين", (* 2))
  , ( "كوبان", (* 2))
  , ( "كوبين", (* 2))
  , ( "باوندان", (* 2))
  , ( "باوندين", (* 2))
  , ( "اونصتان", (* 2))
  , ( "اونصتين", (* 2))
  , ( "أونصتان", (* 2))
  , ( "أونصتين", (* 2))
  , ( "جرامان", (* 2))
  , ( "جرامين", (* 2))
  , ( "ميلي غرامان", (/ 500))
  , ( "ميليغرامان", (/ 500))
  , ( "ميلغرامان", (/ 500))
  , ( "ميلي غرامين", (/ 500))
  , ( "ميليغرامين", (/ 500))
  , ( "ميلغرامين", (/ 500))
  , ( "ميلي جرامان", (/ 500))
  , ( "ميليجرامان", (/ 500))
  , ( "ميلجرامان", (/ 500))
  , ( "ميلي جرامين", (/ 500))
  , ( "ميليجرامين", (/ 500))
  , ( "ميلجرامين", (/ 500))
  , ( "ميلي غرام", (/ 1000))
  , ( "ميليغرام", (/ 1000))
  , ( "ميلغرام", (/ 1000))
  , ( "كيلوغرام", (* 1000))
  , ( "كيلو غرام", (* 1000))
  , ( "ميلي غرامات", (/ 1000))
  , ( "ميليغرامات", (/ 1000))
  , ( "ميلغرامات", (/ 1000))
  , ( "ملغ", (/ 1000))
  , ( "كغ", (* 1000))
  , ( "كغم", (* 1000))
  , ( "ميلي جرام", (/ 1000))
  , ( "ميليجرام", (/ 1000))
  , ( "ميلجرام", (/ 1000))
  , ( "ميلي جرامات", (/ 1000))
  , ( "ميليجرامات", (/ 1000))
  , ( "ميلجرامات", (/ 1000))
  , ( "كيلوغرامات", (* 1000))
  , ( "كيلو غرامات", (* 1000))
  , ( "ملج", (/ 1000))
  , ( "كيلوجرام", (* 1000))
  , ( "كيلو جرام", (* 1000))
  , ( "كيلوجرامات", (* 1000))
  , ( "كيلو جرامات", (* 1000))
  , ( "كج", (* 1000))
  , ( "كجم", (* 1000))
  , ( "كيلوغرامان", (* 2000))
  , ( "كيلوغرامين", (* 2000))
  , ( "كيلو غرامان", (* 2000))
  , ( "كيلو غرامين", (* 2000))
  , ( "كيلوجرامان", (* 2000))
  , ( "كيلوجرامين", (* 2000))
  , ( "كيلو جرامان", (* 2000))
  , ( "كيلو جرامين", (* 2000))
  ]

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
          where value = getValue opsMap match $ TNumeral.value nd
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
         _) -> Just . Token Quantity $ quantity u $ getValue opsMap match 1
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
