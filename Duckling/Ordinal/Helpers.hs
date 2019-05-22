-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}

module Duckling.Ordinal.Helpers
  ( ordinal
  , oneOf
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal

-- -----------------------------------------------------------------
-- Patterns

oneOf :: [Int] -> PatternItem
oneOf vs = Predicate $ \x ->
  case x of
    (Token Ordinal OrdinalData {TOrdinal.value = v}) -> elem v vs
    _ -> False

-- -----------------------------------------------------------------
-- Production

ordinal :: Int -> Token
ordinal x = Token Ordinal OrdinalData {TOrdinal.value = x}
