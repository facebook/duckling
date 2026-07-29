-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.HI.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume

volumes :: [(Text, String, TVolume.Unit)]
volumes = [ ("<vol> liters", "(लीटर|ली\\.|लिटरों)", TVolume.Litre)
          , ("<vol> milliliters", "(मि\\.ली\\.|एम\\.एल\\.|मिलीलीटर|मिलीलीटरों)", TVolume.Millilitre)
          , ("<vol> gallons", "गैलन", TVolume.Gallon)
          ]

rules :: [Rule]
rules = map go volumes
  where
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just . Token Volume $ unitOnly u
      }
