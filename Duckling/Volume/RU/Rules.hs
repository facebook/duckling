-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.RU.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume

ruleHalfLiter :: Rule
ruleHalfLiter = Rule
  { name = "half liter"
  , pattern =
    [ regex "пол[-\\s]?литра"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ volume 0.5
  }

volumes :: [(Text, String, TVolume.Unit)]
volumes = [ ("<latent vol> ml"    , "мл|миллилитр(а|ов)?" , TVolume.Millilitre)
          , ("<vol> hectoliters"  , "гл|гектолитр(а|ов)?" , TVolume.Hectolitre)
          , ("<vol> liters"       , "л(итр(а|ов)?)?"      , TVolume.Litre)
          , ("<latent vol> gallon", "галлон(а|ов)?"       , TVolume.Gallon)
          ]

ruleVolumes :: [Rule]
ruleVolumes = map go volumes
  where
    go :: (Text, String, TVolume.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ dimension Volume, regex regexPattern ]
      , prod = \tokens -> case tokens of
          (Token Volume vd:_) -> Just . Token Volume $ withUnit u vd
          _ -> Nothing
      }

rules :: [Rule]
rules = ruleHalfLiter:ruleVolumes
