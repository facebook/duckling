-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.RO.Rules
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
    , regex "m(ililitr[ui]|l)"
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
    , regex "hectolitr[ui]"
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
    , regex "l(itr[ui])?"
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
    [ regex "jum(a|\x0103)tate de litr[ui]"
    ]
  , prod = \_ -> Just . Token Volume . withUnit TVolume.Litre $ volume 0.5
  }

ruleLatentVolGalon :: Rule
ruleLatentVolGalon = Rule
  { name = "<latent vol> galon"
  , pattern =
    [ dimension Volume
    , regex "gal(oane|on)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Volume vd:_) ->
        Just . Token Volume $ withUnit TVolume.Gallon vd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleHalfLiter
  , ruleLatentVolGalon
  , ruleLatentVolMl
  , ruleVolHectoliters
  , ruleVolLiters
  ]
