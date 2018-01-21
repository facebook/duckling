-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}

module Duckling.Ordinal.Helpers
  ( ordinal
  , oneOf
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Ordinal.Types (OrdinalData (..))
import qualified Duckling.Ordinal.Types as TOrdinal
import Duckling.Types

-- -----------------------------------------------------------------
-- Patterns

-- -----------------------------------------------------------------
-- Production

ordinal :: Int -> Token
ordinal x = Token Ordinal OrdinalData {TOrdinal.value = x}

oneOf :: [Int] -> PatternItem
oneOf vs = Predicate $ \x ->
  case x of
    (Token Ordinal OrdinalData {TOrdinal.value = v}) -> elem v vs
    _ -> False
