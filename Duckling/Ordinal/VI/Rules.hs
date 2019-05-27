-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.VI.Rules
  ( rules ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Ordinal.Helpers
import Duckling.Types

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals"
  , pattern =
    [ regex "(đầu tiên|thứ nhất|thứ 1)"
    ]
  , prod = \_ -> Just $ ordinal 1
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  ]
