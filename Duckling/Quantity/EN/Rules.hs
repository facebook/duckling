-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Quantity.EN.Rules
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
import Duckling.Regex.Types (GroupMatch(..))
import Duckling.Types
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Quantity.Types (QuantityData(..))
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

quantities :: [(Text, String, TQuantity.Unit)]
quantities =
  [ ("<quantity> cups", "(cups?)", TQuantity.Cup)
  , ("<quantity> grams", "(((m(illi)?)|(k(ilo)?))?g(ram)?s?)", TQuantity.Gram)
  , ("<quantity> lb", "((lb|pound)s?)", TQuantity.Pound)
  , ("<quantity> oz", "((ounces?)|oz)", TQuantity.Ounce)
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "milligram" , (/ 1000))
  , ( "milligrams", (/ 1000))
  , ( "mg"        , (/ 1000))
  , ( "mgs"       , (/ 1000))
  , ( "kilogram"  , (* 1000))
  , ( "kilograms" , (* 1000))
  , ( "kg"        , (* 1000))
  , ( "kgs"       , (* 1000))
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
      , pattern = [ regex ("an? " ++ regexPattern) ]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Quantity $ quantity u $ getValue opsMap match 1
        _ -> Nothing
      }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ dimension Quantity
    , regex "of (\\w+)"
    ]
  , prod = \case
    (Token Quantity qd:Token RegexMatch (GroupMatch (product:_)):_) ->
      Just . Token Quantity $ withProduct (Text.toLower product) qd
    _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
    { name = "about|exactly <quantity>"
    , pattern =
      [ regex "\\~|exactly|precisely|about|approx(\\.|imately)?|close to|near( to)?|around|almost"
      , dimension Quantity
      ]
      , prod = \case
        (_:token:_) -> Just token
        _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
    { name = "between|from <numeral> and|to <quantity>"
    , pattern =
      [ regex "between|from"
      , Predicate isPositive
      , regex "to|and"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Numeral NumeralData{TNumeral.value = from}:
         _:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) | from < to ->
          Just . Token Quantity . withInterval (from, to) $ unitOnly u
        _ -> Nothing
    }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
    { name = "between|from <quantity> to|and <quantity>"
    , pattern =
      [ regex "between|from"
      , Predicate isSimpleQuantity
      , regex "and|to"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Quantity QuantityData{TQuantity.value = Just from
                                    , TQuantity.unit = Just u1
                                    , TQuantity.aproduct = Nothing}:
         _:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u2
                                    , TQuantity.aproduct = Nothing}:
         _) | from < to && u1 == u2 ->
          Just . Token Quantity . withInterval (from, to) $ unitOnly u1
        _ -> Nothing
    }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
    { name = "<numeral> - <quantity>"
    , pattern =
      [ Predicate isPositive
      , regex "\\-"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (Token Numeral NumeralData{TNumeral.value = from}:
         _:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) | from < to ->
           Just . Token Quantity . withInterval (from, to) $ unitOnly u
        _ -> Nothing
    }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
    { name = "<quantity> - <quantity>"
    , pattern =
      [ Predicate isSimpleQuantity
      , regex "\\-"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (Token Quantity QuantityData{TQuantity.value = Just from
                                    , TQuantity.unit = Just u1
                                    , TQuantity.aproduct = Nothing}:
         _:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u2
                                    , TQuantity.aproduct = Nothing}:
         _) | from < to && u1 == u2 ->
          Just . Token Quantity . withInterval (from, to) $ unitOnly u1
        _ -> Nothing
    }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
    { name = "under/below/less/lower/at most/no more than <dist>"
    , pattern =
      [ regex "under|below|at most|(less|lower|not? more) than"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) -> Just . Token Quantity . withMax to $ unitOnly u
        _ -> Nothing
    }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/exceeding/beyond/at least/more than <quantity>"
  , pattern =
      [ regex "over|above|exceeding|beyond|at least|(more|larger|bigger|heavier) than"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Quantity QuantityData{TQuantity.value = Just from
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) -> Just . Token Quantity . withMin from $ unitOnly u
        _ -> Nothing
    }

rules :: [Rule]
rules =
  [ ruleQuantityOfProduct
  , ruleIntervalMin
  , ruleIntervalMax
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , rulePrecision
  ]
  ++ ruleNumeralQuantities
  ++ ruleAQuantity
