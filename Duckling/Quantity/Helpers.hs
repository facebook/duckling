-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Quantity.Helpers
  ( quantity
  , withProduct
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Quantity.Types (QuantityData(..))
import qualified Duckling.Quantity.Types as TQuantity

-- -----------------------------------------------------------------
-- Patterns

-- -----------------------------------------------------------------
-- Production

quantity :: TQuantity.Unit -> Double -> QuantityData
quantity u x = QuantityData
  {TQuantity.unit = u, TQuantity.value = x, TQuantity.product = Nothing}

withProduct :: Text -> QuantityData -> QuantityData
withProduct value qd = qd {TQuantity.product = Just value}
