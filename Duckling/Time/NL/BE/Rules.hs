-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.NL.BE.Rules
  ( rules
  ) where

import Duckling.Time.Helpers
import Duckling.Types

ruleHolidays :: [Rule]
ruleHolidays = mkRuleHolidays
  [ ( "Sinterklaas", "sinterklaas", monthDay 12 6 )
  ]

rules :: [Rule]
rules = ruleHolidays
