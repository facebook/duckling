-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.HI.Rules
  ( rules
  ) where

import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Regex.Types
import Duckling.Temperature.Helpers
import Duckling.Types
import qualified Duckling.Temperature.Types as TTemperature

ruleLatentTempDegrees :: Rule
ruleLatentTempDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "डिग्री|°"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTempSpecific :: Rule
ruleTempSpecific = Rule
  { name = "<temp> Fahrenheit|Celsius"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "(सेल्सीयस|फारेनहाइट)"
    ]
  , prod = \case
      (Token Temperature td:Token RegexMatch (GroupMatch (match:_)):_) ->
        case Text.toLower match of
          "सेल्सीयस" -> Just . Token Temperature $ withUnit TTemperature.Celsius td
          "फारेनहाइट" -> Just . Token Temperature $ withUnit TTemperature.Fahrenheit td
          _  -> Nothing
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleLatentTempDegrees
  , ruleTempSpecific
  ]
