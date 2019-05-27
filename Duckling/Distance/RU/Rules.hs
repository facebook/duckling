-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.RU.Rules
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
distances = [ ("<latent dist> km", "км|километр(а|ов)?", TDistance.Kilometre)
            , ("<latent dist> feet", "('|фут(а|ов)?)", TDistance.Foot)
            , ("<latent dist> inch", "(\"|''|дюйм(а|ов)?)", TDistance.Inch)
            , ("<latent dist> yard", "ярд(а|ов)?", TDistance.Yard)
            , ("<dist> meters", "м(етр(а|ов)?)?", TDistance.Metre)
            , ("<dist> centimeters", "см|сантиметр(а|ов)?", TDistance.Centimetre)
            , ("<dist> millimeters", "мм|миллиметр(а|ов)?", TDistance.Millimetre)
            , ("<dist> miles", "мил(я|и|ь)", TDistance.Mile)
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
