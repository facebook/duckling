-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.FR.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume

ruleLatentVolMl :: Rule
ruleLatentVolMl = Rule
  { name = "<latent vol> ml"
  , pattern =
    [ dimension Volume
    , regex "m(l|illilitres?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Millilitre vd
      _ -> Nothing
  }

ruleVolHectoliters :: Rule
ruleVolHectoliters = Rule
  { name = "<vol> hectoliters"
  , pattern =
    [ dimension Volume
    , regex "hectolitres?"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Hectolitre vd
      _ -> Nothing
  }

ruleVolLiters :: Rule
ruleVolLiters = Rule
  { name = "<vol> liters"
  , pattern =
    [ dimension Volume
    , regex "l(itres?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Litre vd
      _ -> Nothing
  }

ruleHalfLiter :: Rule
ruleHalfLiter = Rule
  { name = "half liter"
  , pattern =
    [ regex "demi( |-)?litre"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ volume 0.5
  }

ruleLatentVolGallon :: Rule
ruleLatentVolGallon = Rule
  { name = "<latent vol> gallon"
  , pattern =
    [ dimension Volume
    , regex "gal(l?ons?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Gallon vd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleHalfLiter
  , ruleLatentVolGallon
  , ruleLatentVolMl
  , ruleVolHectoliters
  , ruleVolLiters
  ]
