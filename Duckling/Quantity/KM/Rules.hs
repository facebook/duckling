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
       _) -> Just . Token Quantity $ withProduct (Text.toLower match) qd
      _ -> Nothing
  }

unitsMap :: HashMap Text TQuantity.Unit
unitsMap = HashMap.fromList
  [ ("ចាន", TQuantity.Bowl)
  , ("ពែង", TQuantity.Cup)
  , ("កែវ", TQuantity.Cup)
  , ("ថូ", TQuantity.Pint)
  ]

ruleNumeralUnits :: Rule
ruleNumeralUnits = Rule
  { name = "<number><units>"
  , pattern =
    [ dimension Numeral
    , regex "(ចាន|ពែង|កែវ|ថូ)"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         unit <- HashMap.lookup (Text.toLower match) unitsMap
         Just . Token Quantity $ quantity unit v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralUnits
  , ruleQuantityOfProduct
  ]
