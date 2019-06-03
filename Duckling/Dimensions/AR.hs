-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Dimensions.AR
  ( allDimensions
  ) where

import Duckling.Dimensions.Types

allDimensions :: [Some Dimension]
allDimensions =
  [ This Duration
  , This Numeral
  , This Ordinal
  , This Quantity
  , This Temperature
  , This Time
  , This Volume
  ]
