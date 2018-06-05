-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.NL.NL.Rules
  ( rulesBackwardCompatible
  ) where

import Duckling.Time.Helpers
import Duckling.Types

rulesBackwardCompatible :: [Rule]
rulesBackwardCompatible = mkRuleHolidays
  [ ( "Sinterklaas", "sinterklaas(avond)?|pakjesavond", monthDay 12 5 )
  ]
