-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}


module Duckling.Quantity.Helpers
  ( getValue
  , isSimpleQuantity
  , quantity
  , unitOnly
  , withProduct
  , withUnit
  , withValue
  , withInterval
  , withMin
  , withMax
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Quantity.Types (QuantityData(..))
import Duckling.Types
import qualified Duckling.Quantity.Types as TQuantity

getValue :: HashMap Text (Double -> Double) -> Text -> Double -> Double
getValue opsMap match = HashMap.lookupDefault id (Text.toLower match) opsMap

-- -----------------------------------------------------------------
-- Patterns

isSimpleQuantity :: Predicate
isSimpleQuantity (Token Quantity QuantityData {TQuantity.unit = Just _
                                              , TQuantity.value = Just _})
 = True
isSimpleQuantity _ = False

-- -----------------------------------------------------------------
-- Production

quantity :: TQuantity.Unit -> Double -> QuantityData
quantity u v = QuantityData {TQuantity.unit = Just u
                            , TQuantity.value = Just v
                            , TQuantity.aproduct = Nothing
                            , TQuantity.minValue = Nothing
                            , TQuantity.maxValue = Nothing}

unitOnly :: TQuantity.Unit -> QuantityData
unitOnly u = QuantityData {TQuantity.unit = Just u
                          , TQuantity.value = Nothing
                          , TQuantity.aproduct = Nothing
                          , TQuantity.minValue = Nothing
                          , TQuantity.maxValue = Nothing}

withProduct :: Text -> QuantityData -> QuantityData
withProduct p qd = qd {TQuantity.aproduct = Just p}

withUnit :: TQuantity.Unit -> QuantityData -> QuantityData
withUnit u qd = qd {TQuantity.unit = Just u}

withValue :: Double -> QuantityData -> QuantityData
withValue value qd = qd {TQuantity.value = Just value}

withInterval :: (Double, Double) -> QuantityData -> QuantityData
withInterval (from, to) qd = qd {minValue = Just from, maxValue = Just to}

withMin :: Double -> QuantityData -> QuantityData
withMin from qd = qd {minValue = Just from}

withMax :: Double -> QuantityData -> QuantityData
withMax to qd = qd {maxValue = Just to}
