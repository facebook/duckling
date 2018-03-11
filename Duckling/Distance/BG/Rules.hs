-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.BG.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Distance.Types (DistanceData(..))
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance

distances :: [(Text, String, TDistance.Unit)]
distances = [ ("<latent dist> km", "км|километ(ра|ри|ър)", TDistance.Kilometre)
            , ("<latent dist> feet", "('|фут(а|ове)?)", TDistance.Foot)
            , ("<latent dist> inch", "(\"|''|инч(а|ове)?)", TDistance.Inch)
            , ("<latent dist> yard", "ярд(а|ове)?", TDistance.Yard)
            , ("<dist> meters", "м(етър|етр(а|и))?", TDistance.Metre)
            , ("<dist> centimeters", "см|сантимет(ри|ра|ър)", TDistance.Centimetre)
            , ("<dist> millimeters", "мм|милимет(ра|ри|ър)", TDistance.Millimetre)
            , ("<dist> miles", "мил(я|и)", TDistance.Mile)
            ]

ruleDistances :: [Rule]
ruleDistances = map go distances
  where
    go :: (Text, String, TDistance.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ dimension Distance, regex regexPattern ]
      , prod = \tokens -> case tokens of
          (Token Distance dd:_) -> Just . Token Distance $ withUnit u dd
          _ -> Nothing
      }

rules :: [Rule]
rules = ruleDistances
