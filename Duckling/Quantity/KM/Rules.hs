-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.KM.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Quantity.Helpers
import Duckling.Regex.Types
import Duckling.Types
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
  , ("នាក់", (TQuantity.Custom "For Persons"))
  , ("ក្បាល", (TQuantity.Custom "For Animals"))
  , ("ដើម", (TQuantity.Custom "For Trees"))
  , ("ទង", (TQuantity.Custom "For Flowers"))
  , ("ខ្នង", (TQuantity.Custom "For Buildings"))
  , ("គ្រឿង", (TQuantity.Custom "For Vehicles/Devices"))
  , ("កញ្ចប់", (TQuantity.Custom "For Packages"))
  , ("ឈុត", (TQuantity.Custom "Sets"))
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
  , ("<quantity> liters", "((មីលី)?លីត្រ)", (TQuantity.Custom "Liters"))
  , ("<quantity> meters", "((មីលី|គីឡូ)?ម៉ែត្រ)", (TQuantity.Custom "Meters"))
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "មីលីក្រាម" , (/ 1000))
  , ( "គីឡូក្រាម"  , (* 1000))
  , ( "មីលីលីត្រ" , (/ 1000))
  , ( "មីលីម៉ែត្រ" , (/ 1000))
  , ( "គីឡូម៉ែត្រ"  , (* 1000))
  ]

getValue :: Text -> Double -> Double
getValue match = HashMap.lookupDefault id match opsMap

ruleMetricSysQuantities :: [Rule]
ruleMetricSysQuantities = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [dimension Numeral, regex regexPattern]
      , prod = \case
        (Token Numeral nd:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Quantity $ quantity u value
          where value = getValue match $ TNumeral.value nd
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [ ruleNumeralUnits
  , ruleQuantityOfProduct
  ]
  ++ruleMetricSysQuantities
