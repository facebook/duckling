-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Dimensions.HR
  ( allDimensions
  ) where

import Duckling.Dimensions.Types

allDimensions :: [Seal Dimension]
allDimensions =
  [ Seal Distance
  , Seal Duration
  , Seal Numeral
  , Seal Ordinal
  , Seal Quantity
  , Seal Temperature
  , Seal Time
  , Seal Volume
  ]
