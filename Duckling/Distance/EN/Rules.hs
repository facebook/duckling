-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.EN.Rules
  ( rules ) where


import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Distance.Types (DistanceData(..))
import qualified Duckling.Distance.Types as TDistance
import Duckling.Types

ruleDistanceFeetInch :: Rule
ruleDistanceFeetInch = Rule
  { name = "<distance|feet> <distance|inch>"
  , pattern =
    [ unitDistance TDistance.Foot
    , unitDistance TDistance.Inch
    ]
  , prod = \tokens -> case tokens of
      (Token Distance DistanceData {TDistance.value = feet}:
       Token Distance DistanceData {TDistance.value = inches}:
       _) -> Just . Token Distance . withUnit TDistance.Inch . distance $
        feet * 12 + inches
      _ -> Nothing
  }

ruleDistanceFeetAndInch :: Rule
ruleDistanceFeetAndInch = Rule
  { name = "<distance|feet> and <distance|inch>"
  , pattern =
    [ unitDistance TDistance.Foot
    , regex "and"
    , unitDistance TDistance.Inch
    ]
  , prod = \tokens -> case tokens of
      (Token Distance DistanceData {TDistance.value = feet}:
       _:
       Token Distance DistanceData {TDistance.value = inches}:
       _) -> Just . Token Distance . withUnit TDistance.Inch . distance $
        feet * 12 + inches
      _ -> Nothing
  }

distances :: [(Text, String, TDistance.Unit)]
distances = [ ("<latent dist> km", "k(ilo)?m?(eter)?s?", TDistance.Kilometre)
            , ("<latent dist> feet", "('|f(oo|ee)?ts?)", TDistance.Foot)
            , ("<latent dist> inch", "(\"|''|in(ch(es)?)?)", TDistance.Inch)
            , ("<latent dist> yard", "y(ar)?ds?", TDistance.Yard)
            , ("<dist> meters", "meters?", TDistance.Metre)
            , ("<dist> centimeters", "cm|centimeters?", TDistance.Centimetre)
            , ("<dist> miles", "mi(le(s)?)?", TDistance.Mile)
            , ("<dist> m (ambiguous miles or meters)", "m", TDistance.M)
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
rules =
  [ ruleDistanceFeetInch
  , ruleDistanceFeetAndInch
  ]
  ++ ruleDistances
