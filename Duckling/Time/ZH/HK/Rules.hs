-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ZH.HK.Rules
  ( rules
  ) where

import Prelude

import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Types

ruleNationalDay :: Rule
ruleNationalDay = Rule
  { name = "national day"
  , pattern =
    [ regex "(国庆|國慶)(节|節)?"
    ]
  , prod = \_ -> tt $ monthDay 10 1
  }

rules :: [Rule]
rules =
  [ ruleNationalDay
  ]
