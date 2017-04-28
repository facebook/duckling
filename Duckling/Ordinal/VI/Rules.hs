-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

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
    [ regex "(\x0111\x1ea7u ti\x00ean|th\x1ee9 nh\x1ea5t|th\x1ee9 1)"
    ]
  , prod = \_ -> Just $ ordinal 1
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  ]
