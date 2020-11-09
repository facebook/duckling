-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Quantity.ES.Rules
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

-- Our quantities: (name, regex, quantityType). Please make sure to wrap
-- each regex in parentheses, because we extend these regexes later with
-- articles (un/una).
quantities :: [(Text, String, TQuantity.Unit)]
quantities =
  [ ("<quantity> bowls", "(bol(es)?|tazón(es)?|cuencos?|platos? (soperos?)|(hondos?))", TQuantity.Bowl)
  , ("<quantity> cups", "(tazas?)", TQuantity.Cup)
  , ("<quantity> dishes", "(platos?|fuentes?)", TQuantity.Dish)
  , ("<quantity> grams", "(((m(ili)?)|(k(ilo)?))?g(ramo)?s?)", TQuantity.Gram)
  , ("<quantity> ounces", "((onzas?)|oz)", TQuantity.Ounce)
  , ("<quantity> pints", "(pintas?)", TQuantity.Pint)
  , ("<quantity> pounds", "((lb|libra)s?)", TQuantity.Pound)
  , ("<quantity> quarts", "(cuartos? de galón)", TQuantity.Quart)
  , ("<quantity> tablespoons", "(cucharadas? (grande)?)", TQuantity.Tablespoon)
  , ("<quantity> teaspoons", "(cucharaditas?)", TQuantity.Teaspoon)
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "miligram"   , (/ 1000))
  , ( "miligramos" , (/ 1000))
  , ( "mg"         , (/ 1000))
  , ( "mgs"        , (/ 1000))
  , ( "kilogramo"  , (* 1000))
  , ( "kilogramos" , (* 1000))
  , ( "kg"         , (* 1000))
  , ( "kgs"        , (* 1000))
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (nm, regexPattern, u) = Rule
      { name = nm
      , pattern = [Predicate isPositive, regex regexPattern]
      , prod = \case
        (Token Numeral nd:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> Just $ Token Quantity $ quantity u val
          where val = getValue opsMap match $ TNumeral.value nd
        _ -> Nothing
      }

-- Quantities prefixed by "un" or "una"
ruleAQuantity :: [Rule]
ruleAQuantity = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (nm, regexPattern, u) = Rule
      { name = nm
      , pattern = [ regex ("una? " ++ regexPattern) ]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just $ Token Quantity $ quantity u $ getValue opsMap match 1
        _ -> Nothing
      }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> de producto"
  , pattern =
    [ dimension Quantity
    , regex "de (\\w+)"
    ]
  , prod = \case
    (Token Quantity qd:Token RegexMatch (GroupMatch (prdct:_)):_) ->
      Just $ Token Quantity $ withProduct (Text.toLower prdct) qd
    _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
    { name = "about|exactly <quantity>"
    , pattern =
      [ regex "exactamente|precisamente|a?cerca( de)?|aproximadamente|casi"
      , dimension Quantity
      ]
      , prod = \case
        (_:tkn:_) -> Just tkn
        _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
    { name = "between|from <numeral> and|to <quantity>"
    , pattern =
      [ regex "entre|de"
      , Predicate isPositive
      , regex "a|y"
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
          Just $ Token Quantity $ withInterval (from, to) $ unitOnly u
        _ -> Nothing
    }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
    { name = "between|from <quantity> to|and <quantity>"
    , pattern =
      [ regex "entre/de"
      , Predicate isSimpleQuantity
      , regex "a/y"
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
          Just $ Token Quantity $ withInterval (from, to) $ unitOnly u1
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
           Just $ Token Quantity $ withInterval (from, to) $ unitOnly u
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
          Just $ Token Quantity $ withInterval (from, to) $ unitOnly u1
        _ -> Nothing
    }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
    { name = "under/below/less/lower/at most/no more than <dist>"
    , pattern =
      [ regex "no m(á|a)s que|menos de|por debajo de|como mucho|como m(á|a)xim(o|a)|a lo sumo|menos (que|de)"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) -> Just $ Token Quantity $ withMax to $ unitOnly u
        _ -> Nothing
    }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/exceeding/beyond/at least/more than <quantity>"
  , pattern =
      [ regex "(?<!no )m(á|a)s( grande| pesado)? (de|que)|mayor de|por encima de|excesivo|fuera de|por lo menos|como m(í|i)nim(o|a)|al menos"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Quantity QuantityData{TQuantity.value = Just from
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) -> Just $ Token Quantity $ withMin from $ unitOnly u
        _ -> Nothing
    }

ruleQuantityLatent :: Rule
ruleQuantityLatent = Rule
  { name = "<quantity> (latent)"
  , pattern =
    [ Predicate isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}: _) ->
        Just $ Token Quantity $ mkLatent $ valueOnly v
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
  , ruleQuantityLatent
  ]
  ++ ruleNumeralQuantities
  ++ ruleAQuantity
