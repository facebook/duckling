-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.KM.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text, toLower)
import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Quantity.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Quantity.Types (QuantityData(..))
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ regex "(មនុស្ស|បងប្អូន|សត្វ|ឆ្កែ|ឆ្មា|ដើមឈើ|ផ្កា|កុលាប|ផ្ទះ)"
    , dimension Quantity
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       Token Quantity qd:
       _) -> Just . Token Quantity $ withProduct match qd
      _ -> Nothing
  }

unitsMap :: HashMap Text TQuantity.Unit
unitsMap = HashMap.fromList
  [ ("ចាន", TQuantity.Bowl)
  , ("ពែង", TQuantity.Cup)
  , ("កែវ", TQuantity.Cup)
  , ("ថូ", TQuantity.Pint)
  , ("ស្លាបព្រា", TQuantity.Tablespoon)
  , ("ស្លាបព្រាបាយ", TQuantity.Tablespoon)
  , ("ស្លាបព្រាកាហ្វេ", TQuantity.Teaspoon)
  , ("នាក់", TQuantity.Custom "For Persons")
  , ("ក្បាល", TQuantity.Custom "For Animals")
  , ("ដើម", TQuantity.Custom "For Trees")
  , ("ទង", TQuantity.Custom "For Flowers")
  , ("ខ្នង", TQuantity.Custom "For Buildings")
  , ("គ្រឿង", TQuantity.Custom "For Vehicles/Devices")
  , ("កញ្ចប់", TQuantity.Custom "For Packages")
  , ("ឈុត", TQuantity.Custom "Sets")
  ]

ruleNumeralUnits :: Rule
ruleNumeralUnits = Rule
  { name = "<number><units>"
  , pattern =
    [ dimension Numeral
    , regex "(ចាន|ពែង|កែវ|ថូ|ស្លាបព្រា|ស្លាបព្រាបាយ|ស្លាបព្រាកាហ្វេ|នាក់|ក្បាល|ដើម|ទង|ខ្នង|គ្រឿង|កញ្ចប់|ឈុត)"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         unit <- HashMap.lookup match unitsMap
         Just . Token Quantity $ quantity unit v
      _ -> Nothing
  }

quantities :: [(Text, String, TQuantity.Unit)]
quantities =
  [ ("<quantity> grams", "((មីលី|គីឡូ)?ក្រាម)", TQuantity.Gram)
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "មីលីក្រាម" , (/ 1000))
  , ( "គីឡូក្រាម" , (* 1000))
  ]

ruleNumeralUnits2 :: [Rule]
ruleNumeralUnits2 = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [dimension Numeral, regex regexPattern]
      , prod = \case
        (Token Numeral nd:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Quantity $ quantity u value
          where value = getValue opsMap match $ TNumeral.value nd
        _ -> Nothing
      }

rulePrecision :: Rule
rulePrecision = Rule
    { name = "about|exactly <quantity>"
    , pattern =
      [ regex "\\~|ប្រហែល"
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
      [ regex "ចន្លោះ(ពី)?|ចាប់ពី"
      , Predicate isPositive
      , regex "និង|ដល់"
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
      [ regex "ចន្លោះ(ពី)?|ចាប់ពី"
      , Predicate isSimpleQuantity
      , regex "និង|ដល់"
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
    { name = "Max Rule"
    , pattern =
      [ regex "ក្រោម|តិចជាង|មិនដល់|យ៉ាងច្រើន|មិនលើស"
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
  { name = "Min Rule"
  , pattern =
      [ regex "លើស(ពី)?|មិនតិចជាង|លើ|ច្រើនជាង|យ៉ាងតិច|យ៉ាងហោច"
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
  [ ruleNumeralUnits
  , ruleQuantityOfProduct
  , rulePrecision
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleIntervalMax
  , ruleIntervalMin
  ]
  ++ruleNumeralUnits2
